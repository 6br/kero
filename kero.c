/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*** defines ***/
#define KERO_VERSION "0.0.1"
#define KERO_TAB_STOP 4
#define KERO_QUIT_TIMES 2

#define CTRL_KEY(k) ((k) & 0x1f)

enum editorKey {
  BACKSPACE = 127,
  ARROW_LEFT = 1000,
  ARROW_RIGHT,
  ARROW_UP,
  ARROW_DOWN,
  DEL_KEY,
  HOME_KEY,
  END_KEY,
  PAGE_UP,
  PAGE_DOWN
};

enum editorHighlight {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
};

enum editorMode { MODE_NORMAL = 0, MODE_INSERT = 1 };

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** data ***/

struct editorSyntax {
  char *filetype;
  char **filematch;
  char **keywords;
  char *singleline_comment_start;
  char *multiline_comment_start;
  char *multiline_comment_end;
  int flags;
};

typedef struct erow {
  int idx;
  int size;
  int rsize;
  char *chars;
  char *render;
  unsigned char *hl;
  int hl_open_comment;
} erow;

struct editorConfig {
  int cx, cy;
  int rx;
  int rowoff;
  int coloff;
  int screenrows;
  int screencols;
  int numrows;
  erow *row;
  int dirty;
  char *filename;
  char statusmsg[80];
  time_t statusmsg_time;
  struct editorSyntax *syntax;
  struct termios orig_termios;
  int mode;
  int rownumber;
  char *yank;
  int yanksize;
  char *find;
  int findsize;
  struct editorConfig *undo;
  struct editorConfig *redo;
};
struct editorConfig E;

struct editorConfig history_push(struct editorConfig *e);
struct editorConfig history_undo(struct editorConfig *e);
struct editorConfig history_redo(struct editorConfig *e);

/*** filetypes ***/
char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "default", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case","#define", "#include",
  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|", "bool|",
  "void|", NULL
};

char *BASH_HL_extensions[] = { ".sh", NULL };
char *BASH_HL_keywords[] = {
  "break", "continue", "cd", "continue", "eval", "exec", "exit", "export",
  "getopts", "hash", "pwd", "readonly", "return", "shift", "test", "times", "trap", "umask", "unset",
  NULL
};

char *PYTHON_HL_extensions[] = { ".py", NULL };
char *PYTHON_HL_keywords[] = {
  "and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else", "else:" "except", "exec",
  "finally", "for", "global", "if", "in", "is", "lambda", "not", "or", "pass", "raise", "return",
  "try" "while" "with", "yield", "import", "from", NULL
};


char *JS_HL_extensions[] = { ".js", ".jsx", ".ts", ".tsx", ".json",  NULL};
char *JS_HL_keywords[] = {
  "break","case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else", "export", "extends", "finally", "for", "from", "function", "if", "import", "in", "instanceof", "new", "return", "super", "switch", "this", "throw", "try", "typeof", "var", "void", "while", "with", "yield", NULL
};

struct editorSyntax HLDB[] = {
  {
    "c",
    C_HL_extensions,
    C_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
  {
    "bash",
    BASH_HL_extensions,
    BASH_HL_keywords,
    "#", "<<", "<<",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
  {
    "python",
    PYTHON_HL_extensions,
    PYTHON_HL_keywords,
    "#", "\"\"\"", "\"\"\"",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
  {
    "javascript",
    JS_HL_extensions,
    JS_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  }
};																	
#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** prototypes ***/
void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int), char *buf);
void editorUpdateSyntax(erow *row);
void editorSelectSyntaxHighlight();
void message(const char *fmt, ...);
void editorInsertRow(int at, char *s, size_t len, int dirty);
void editorMoveCursor(int key);

/*** terminal ***/
void die(const char *s) {
  write(STDOUT_FILENO, "\x1b[2J", 4);
  write(STDOUT_FILENO, "\x1b[H", 3);

  perror(s);
  exit(1);
}
void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
}

void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
  atexit(disableRawMode);

  struct termios raw = E.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 0;

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int editorReadKey() {
  int nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN) die("read");
  }

  if (c == '\x1b') {
    char seq[5];
    if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
    if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
    if (seq[0] == '[') {
      if (seq[1] >= '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
        if (seq[2] == '~') {
          switch (seq[1]) {
            case '1': return HOME_KEY;
            case '3': return DEL_KEY;
            case '4': return END_KEY;
            case '5': return PAGE_UP;
            case '6': return PAGE_DOWN;
            case '7': return HOME_KEY;
            case '8': return END_KEY;
          }
        } else if (seq[2] == ';') {
          if (read(STDIN_FILENO, &seq[3], 1) != 1) return '\x1b';
          if (read(STDIN_FILENO, &seq[4], 1) != 1) return '\x1b';
          switch (seq[4]) {
            case 'A': return PAGE_UP;
            case 'B': return PAGE_DOWN;
          } 
        }
      } else {
      switch (seq[1]) {
        case 'A': return ARROW_UP;
        case 'B': return ARROW_DOWN;
        case 'C': return ARROW_RIGHT;
        case 'D': return ARROW_LEFT;
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
      }
    }
    } else if (seq[0] == 'O') {
      switch (seq[1]) {
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
      }
   }
    return '\x1b';
  } else {
    return c;
  }
}

int getCursorPosition(int *rows, int *cols) {
  char buf[32];
  unsigned int i = 0;

  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

  while (i < sizeof(buf) - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
    if (buf[i] == 'R') break;
    i++;
  }
  buf[i] = '\0';
  if (buf[0] != '\x1b' || buf[1] != '[') return -1;
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
  return 0;
}

/*** row operations ***/
int editorRowCxToRx(erow *row, int cx) {
  int rx = E.rownumber;
  int j;
  for (j = 0; j < cx; j++) {
    if (row->chars[j] == '\t')
      rx += (KERO_TAB_STOP - 1) - (rx % KERO_TAB_STOP);
    rx++;
  }
  return rx;
}
int editorRowRxToCx(erow *row, int rx) {
  int cur_rx = 0;
  int cx;
  for (cx = 0; cx < row->size; cx++) {
    if (row->chars[cx] == '\t')
      cur_rx += (KERO_TAB_STOP - 1) - (cur_rx % KERO_TAB_STOP);
    cur_rx++;
    if (cur_rx > rx) return cx - E.rownumber;
  }
  return cx - E.rownumber;
}

void editorUpdateRow(erow *row) {
  int tabs = 0;
  int j;
  for (j = 0; j < row->size; j++)
    if (row->chars[j] == '\t') tabs++;
  free(row->render);
  row->render = malloc(row->size + tabs*(KERO_TAB_STOP - 1) + 1 + E.rownumber);
  int idx = 0;
  //char* rownumber = itoa(row->idx, NULL, 10);
  char rownumber[5];
  int len = sprintf(rownumber,"%u",row->idx);
  if (E.rownumber > 0) { 
    for (j = 0; j < E.rownumber; j++) {
      row->render[idx++] = ' ';
    }
    sprintf(row->render + (E.rownumber - 1 - len), "%d", row->idx);
    row->render[idx-1] = ' ';
  }
  for (j = 0; j < row->size; j++) {
    if (row->chars[j] == '\t') {
      row->render[idx++] = ' ';
      while (idx % KERO_TAB_STOP != 0) row->render[idx++] = ' ';
    } else {
      row->render[idx++] = row->chars[j];
    }
  }
  row->render[idx] = '\0';
  row->rsize = idx;

  editorUpdateSyntax(row);
}

void editorPasteRow() {
  if (E.yank != NULL) {
    E.cy++;
    editorInsertRow(E.cy, E.yank, E.yanksize,1);
    erow *row = &E.row[E.cy];
    editorUpdateRow(row);
    E.cx = E.rownumber;
  }
}

void editorInsertRow(int at, char *s, size_t len, int dirty) {
  if (at < 0 || at > E.numrows) return;
  if (dirty > 0) history_push(&E);
  E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
  memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));

  for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;

  E.row[at].idx = at;
  E.row[at].size = len;
  E.row[at].chars = malloc(len + 1);
  memcpy(E.row[at].chars, s, len);
  E.row[at].chars[len] = '\0';
  E.row[at].rsize = 0;
  E.row[at].render = NULL;
  E.row[at].hl = NULL;
  E.row[at].hl_open_comment = 0;
  editorUpdateRow(&E.row[at]);
  E.numrows++;
  E.dirty++;
}

void editorFreeRow(erow *row) {
  free(row->render);
  free(row->chars);
  free(row->hl);
}

void editorDelRow(int at) {
  if (at < 0 || at >= E.numrows) return;
  editorFreeRow(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
  for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
  E.cx = 0;
  E.numrows--;
  E.dirty++;
}

void editorCopyRow(int at) {
  E.yank = realloc(E.yank, (E.row[at].size));
  E.yanksize = E.row[at].size;
  memcpy(E.yank, E.row[at].chars, E.row[at].size);
}

void editorRowInsertChar(erow *row, int at, int c) {
  if (at < 0 || at > row->size) at = row->size;
  row->chars = realloc(row->chars, row->size + 2);
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}
void editorJoinLines() {
  if (E.cy == E.numrows - 1)
    return;
  erow *row = &E.row[E.cy];
  erow *rowBelow = &E.row[E.cy + 1];
  editorRowAppendString(row, " ", 1);
  editorRowAppendString(row, rowBelow->chars, rowBelow->size);
  editorDelRow(E.cy + 1);
}


void editorRowDelChar(erow *row, int at) {
  if (at < 0 || at >= row->size) return;
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
  row->size--;
  editorUpdateRow(row);
  E.dirty++;
}

/*** editor operations ***/
void editorInsertChar(int c) {
  if (E.cy == E.numrows) {
    editorInsertRow(E.numrows, "", 0, 1);
  }
  editorRowInsertChar(&E.row[E.cy], E.cx, c);
  E.cx++;
}

void editorInsertNewline() {
  if (E.cx == E.rownumber) {
    editorInsertRow(E.cy, "", 0, 1);
  } else {
    erow *row = &E.row[E.cy];
    editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx, 1);
    row = &E.row[E.cy];
    row->size = E.cx;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
  }
  E.cy++;
  E.cx = 0;
}

void editorDelChar() {
  if (E.cy == E.numrows) return;
  if (E.cx == E.rownumber && E.cy == 0) return;

  erow *row = &E.row[E.cy];
  if (E.cx > E.rownumber) {
    editorRowDelChar(row, E.cx - 1);
    E.cx--;
  } else {
    int tmp = E.row[E.cy - 1].size;
    editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
    editorDelRow(E.cy);
    E.cx = tmp;
    E.cy--;
  }
}

/*** file i/o ***/
char *editorRowsToString(int *buflen) {
  int totlen = 0;
  int j;
  for (j = 0; j < E.numrows; j++)
    totlen += E.row[j].size + 1;
  *buflen = totlen;
  char *buf = malloc(totlen);
  char *p = buf;
  for (j = 0; j < E.numrows; j++) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }
  return buf;
}

size_t split(char* s, const char* separator, char** result, size_t result_size)
{
    //assert(s != NULL);
    //assert(separator != NULL);
    //assert(result != NULL);
    //assert(result_size > 0);

    size_t i = 0;

    char* p = strtok(s, separator);
    while (p != NULL && result_size >= i) {
        //assert(i < result_size);
        result[i] = p;
        ++i;

        p = strtok(NULL, separator);
    }

    return i;
}

void editorOpen(char *filename) {
  free(E.filename);
  int cx = 0;
  int cy = 0;

  FILE *fp = fopen(filename, "r");
  if (!fp) {
    char* result1[4];
    size_t split_num = split(filename, ":", result1, sizeof(result1)/sizeof(result1[0]));
    if (split_num == 2 || split_num == 3) {
      fp = fopen(result1[0], "r");
      if (fp) {
        E.filename = strdup(result1[0]);
        cy = atoi(result1[1]);
        if (split_num == 3) 
          cx = atoi(result1[2]);
      } else {
        die("fopen");
      }
    } else {
      die("fopen");
    }
  } else {
    E.filename = strdup(filename);
  }
  editorSelectSyntaxHighlight();
  char *line = NULL;
  size_t linecap = 0;
  ssize_t linelen;
  while ((linelen = getline(&line, &linecap, fp)) != -1) {
    while (linelen > 0 && (line[linelen - 1] == '\n' ||
                           line[linelen - 1] == '\r'))
      linelen--;
    editorInsertRow(E.numrows, line, linelen, 0);
  }
  free(line);
  fclose(fp);
  E.dirty = 0;
  E.cy = cy;
  E.cx = cx;
}

void editorQuit() {
  write(STDOUT_FILENO, "\x1b[2J", 4);
  write(STDOUT_FILENO, "\x1b[H", 3);
  exit(0);
}

void editorSave() {
  E = history_push(&E);
  if (E.filename == NULL) {
    E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL, NULL);
    if (E.filename == NULL) {
      editorSetStatusMessage("Save aborted");
      return;
    }

    editorSelectSyntaxHighlight();

  }
  int len;
  char *buf = editorRowsToString(&len);
  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if (write(fd, buf, len) == len) {
        close(fd);
        free(buf);

        E.dirty = 0;
        editorSetStatusMessage("%d bytes written to disk", len);
        return;
      }
    }
    close(fd);
  }
  free(buf);

  editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/*** find ***/
void editorFindCallback(char *query, int key) {
  static int last_match = -1;
  static int direction = 1;

  static int saved_hl_line;
  static char *saved_hl = NULL;
  if (saved_hl) {
    memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
    free(saved_hl);
    saved_hl = NULL;
  }

  if (key == '\r' || key == '\x1b') {
    last_match = -1;
    direction = 1;
    return;
  } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
    direction = 1;
  } else if (key == ARROW_LEFT || key == ARROW_UP) {
    direction = -1;
  } else {
    last_match = -1;
    direction = 1;
  }
  if (last_match == -1) direction = 1;
  int current = last_match;
  int i;
  for (i = 0; i < E.numrows; i++) {
    current += direction;
    if (current == -1) current = E.numrows - 1;
    else if (current == E.numrows) current = 0;
    erow *row = &E.row[current];
    char *match = strstr(row->render, query);
    if (match) {
      last_match = current;
      E.cy = current;
      E.cx = editorRowRxToCx(row, match - row->render);
      E.rowoff = E.numrows;


      saved_hl_line = current;
      saved_hl = malloc(row->rsize);
      memcpy(saved_hl, row->hl, row->rsize);

      memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
      break;
    }
  }
}

void editorFind() {
  int saved_cx = E.cx;
  int saved_cy = E.cy;
  int saved_coloff = E.coloff;
  int saved_rowoff = E.rowoff;

  char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)",
                             editorFindCallback, E.find);
  if (query) {
    if (E.find) {
      free(E.find);
    }
    E.find = query;
    // free(query);
  } else {
    E.cx = saved_cx;
    E.cy = saved_cy;
    E.coloff = saved_coloff;
    E.rowoff = saved_rowoff;
  }
}

int getWindowSize(int *rows, int *cols) {
  struct winsize ws;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
    return getCursorPosition(rows, cols);
  } else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

/*** syntax highlighting ***/
int is_separator(int c) {
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
  row->hl = realloc(row->hl, row->rsize);
  memset(row->hl, HL_NORMAL, row->rsize);
  if (E.syntax == NULL) return;

  char **keywords = E.syntax->keywords;

  char *scs = E.syntax->singleline_comment_start;

  char *mcs = E.syntax->multiline_comment_start;
  char *mce = E.syntax->multiline_comment_end;

  int scs_len = scs ? strlen(scs) : 0;

  int mcs_len = mcs ? strlen(mcs) : 0;
  int mce_len = mce ? strlen(mce) : 0;
  int prev_sep = 1;
  int in_string = 0;

  int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);
  int i = 0;
  while (i < row->rsize) {
    char c = row->render[i];
    unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;
    if (scs_len && !in_string && !in_comment) {
      if (!strncmp(&row->render[i], scs, scs_len)) {
        memset(&row->hl[i], HL_COMMENT, row->rsize - i);
        break;
      }
    }
    if (mcs_len && mce_len && !in_string) {
      if (in_comment) {
        row->hl[i] = HL_MLCOMMENT;
        if (!strncmp(&row->render[i], mce, mce_len)) {
          memset(&row->hl[i], HL_MLCOMMENT, mce_len);
          i += mce_len;
          in_comment = 0;
          prev_sep = 1;
          continue;
        } else {
          i++;
          continue;
        }
      } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
        memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
        i += mcs_len;
        in_comment = 1;
        continue;
      }
    }
    if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
      if (in_string) {
        row->hl[i] = HL_STRING;
        if (c == '\\' && i + 1 < row->rsize) {
          row->hl[i + 1] = HL_STRING;
          i += 2;
          continue;
        }
        if (c == in_string) in_string = 0;
        i++;
        prev_sep = 1;
        continue;
      } else {
        if (c == '"' || c == '\'') {
          in_string = c;
          row->hl[i] = HL_STRING;
          i++;
          continue;
        }
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
    if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
        (c == '.' && prev_hl == HL_NUMBER)) {
      row->hl[i] = HL_NUMBER;
      i++;
      prev_sep = 0;
      continue;
    }}

    if (prev_sep) {
      int j;
      for (j = 0; keywords[j]; j++) {
        int klen = strlen(keywords[j]);
        int kw2 = keywords[j][klen - 1] == '|';
        if (kw2) klen--;
        if (!strncmp(&row->render[i], keywords[j], klen) &&
            is_separator(row->render[i + klen])) {
          memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
          i += klen;
          break;
        }
      }
      if (keywords[j] != NULL) {
        prev_sep = 0;
        continue;
      }
    }
    prev_sep = is_separator(c);
    i++;
  }

  int changed = (row->hl_open_comment != in_comment);
  row->hl_open_comment = in_comment;
  if (changed && row->idx + 1 < E.numrows)
    editorUpdateSyntax(&E.row[row->idx + 1]);
}

int editorSyntaxToColor(int hl) {
  switch (hl) {
    case HL_COMMENT: 
    case HL_MLCOMMENT: return 36;
    case HL_KEYWORD1: return 33;
    case HL_KEYWORD2: return 32;
    case HL_STRING: return 35;
    case HL_NUMBER: return 31;
    case HL_MATCH: return 34;
    default: return 37;
  }
}

void editorSelectSyntaxHighlight() {
  E.syntax = NULL;
  if (E.filename == NULL) return;
  char *ext = strrchr(E.filename, '.');
  for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
    struct editorSyntax *s = &HLDB[j];
    unsigned int i = 0;
    while (s->filematch[i]) {
      int is_ext = (s->filematch[i][0] == '.');
      if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
          (!is_ext && strstr(E.filename, s->filematch[i]))) {
        E.syntax = s;
        int filerow;
        for (filerow = 0; filerow < E.numrows; filerow++) {
          editorUpdateSyntax(&E.row[filerow]);
        }
        return;
      }
      i++;
    }
  }
}

/*** append buffer ***/
struct abuf {
  char *b;
  int len;
};
#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len) {
  char *new = realloc(ab->b, ab->len + len);
  if (new == NULL) return;
  memcpy(&new[ab->len], s, len);
  ab->b = new;
  ab->len += len;
}
void abFree(struct abuf *ab) {
  free(ab->b);
}

/*** output ***/
void editorColon() {
  errno = 0;
  char* end;
  char* query = editorPrompt(":%s", NULL, NULL);
  if (query) {
    long int v = strtol(query, &end, 10);
    if (strcmp(query, "q!") == 0 || strcmp(query, "Q!") == 0) {
      editorQuit();
    } else if (strcmp(query, "wq") == 0 || strcmp(query, "WQ") == 0) {
      editorSave();
      editorQuit();
    } else if (strcmp(query,"q") == 0 || strcmp(query,"Q") == 0) {
      if (E.dirty == 0) {
        editorQuit();
      } else {
        message("No write since last change.");
      }
    } else if (strcmp(query, "W") == 0 || strcmp(query, "w") == 0) {
      editorSave();
    } else if (errno != ERANGE && end != query) {
      if (E.numrows <= v) v = E.numrows - 1;
      message(query);
      int previous = E.cy;
      while (previous != v) {
        if (previous < v) {
          editorMoveCursor(ARROW_DOWN);
          previous++;
        } else {
          editorMoveCursor(ARROW_UP);
          previous--;
        }
      }
      //message("");
    } else {
      message("Nothing happens.");
    }
    free(query);
  }
}


void editorScroll() {
  E.rx = 0;
  if (E.cy < E.numrows) {
    E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
  }
  if (E.cy < E.rowoff) {
    E.rowoff = E.cy;
  }
  if (E.cy >= E.rowoff + E.screenrows) {
    E.rowoff = E.cy - E.screenrows + 1;
  }
  if (E.cx < E.coloff) {
    E.coloff = E.rx;
  }
  if (E.cx >= E.coloff + E.screencols) {
    E.coloff = E.rx - E.screencols + 1;
  }
}

void editorDrawRows(struct abuf *ab) {
  int y;
  for (y = 0; y < E.screenrows; y++) {
    int filerow = y + E.rowoff;
    if (filerow >= E.numrows) {
    if (E.numrows == 0 && y == E.screenrows / 3) {
      char welcome[80];
      int welcomelen = snprintf(welcome, sizeof(welcome),
        "Kilo editor -- version %s", KERO_VERSION);
      if (welcomelen > E.screencols) welcomelen = E.screencols;
      int padding = (E.screencols - welcomelen) / 2;
      if (padding) {
        abAppend(ab, "~", 1);
        padding--;
      }
      while (padding--) abAppend(ab, " ", 1);
      abAppend(ab, welcome, welcomelen);
    } else {
      int rownumber = E.rownumber;
      while (rownumber--) abAppend(ab, " ", 1);
      abAppend(ab, "~", 1);
    } 
    } else {
      int len = E.row[filerow].rsize - E.coloff;
      if (len < 0) len = 0;
      if (len > E.screencols) len = E.screencols;
      char *c = &E.row[filerow].render[E.coloff];
      unsigned char *hl = &E.row[filerow].hl[E.coloff];
      int current_color = -1; 
      int j;
      for (j = 0; j < len; j++) {
        if (iscntrl(c[j])) {
          char sym = (c[j] <= 26) ? '@' + c[j] : '?';
          abAppend(ab, "\x1b[7m", 4);
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3);
          if (current_color != -1) {
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
            abAppend(ab, buf, clen);
          }
        } else if (hl[j] == HL_NORMAL) {
          if (current_color != -1) {
          abAppend(ab, "\x1b[39m", 5);
            current_color = -1;
          }
          abAppend(ab, &c[j], 1);
        } else {
          int color = editorSyntaxToColor(hl[j]);
          if (color != current_color) {
            current_color = color;
          char buf[16];
          int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
          abAppend(ab, buf, clen);
          }
          abAppend(ab, &c[j], 1);
        }
      }
      abAppend(ab, "\x1b[39m", 5);
    }

    abAppend(ab, "\x1b[K", 3);
      abAppend(ab, "\r\n", 2);
  }
}
void editorDrawStatusBar(struct abuf *ab) {
  abAppend(ab, "\x1b[7m", 4);
  char status[80], rstatus[80], statusmode[4], statuscolor[6];

  switch (E.mode) {
  case MODE_NORMAL:
    strcpy(statusmode, "<N>");
    strcpy(statuscolor, "\x1b[37m");
    break;
  case MODE_INSERT:
    strcpy(statusmode, "<I>");
    strcpy(statuscolor, "\x1b[33m");
    break;
  default:
    strcpy(statusmode, "???");
    strcpy(statuscolor, "\x1b[31m");
  }

  int len = snprintf(status, sizeof(status), "%s%04d:%02d  %s: %s %.20s - %d lines %s",
    statuscolor, E.cy, E.cx, statusmode,
    E.syntax ? E.syntax->filetype : "Fundamental",
    E.filename ? E.filename : "[No Name]", E.numrows,
    E.dirty ? "(modified)" : "");
  int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
    E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
  if (len > E.screencols) len = E.screencols;
  abAppend(ab, status, len);
  while (len < E.screencols) {
    if (E.screencols - len == rlen) {
      abAppend(ab, rstatus, rlen);
      break;
    } else {
      abAppend(ab, " ", 1);
      len++;
    }
  }
  abAppend(ab, "\x1b[m", 3);

  abAppend(ab, "\r\n", 2);
}
void editorDrawMessageBar(struct abuf *ab) {
  abAppend(ab, "\x1b[K", 3);
  int msglen = strlen(E.statusmsg);
  if (msglen > E.screencols) msglen = E.screencols;
  if (msglen && time(NULL) - E.statusmsg_time < 5)
    abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen() {
  editorScroll();
  struct abuf ab = ABUF_INIT;
  abAppend(&ab, "\x1b[?25l", 6);
  abAppend(&ab, "\x1b[H", 3);
  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  char buf[32];
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1, (E.rx - E.coloff) + 1);
  abAppend(&ab, buf, strlen(buf));

  abAppend(&ab, "\x1b[?25h", 6);

  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}


void editorSetStatusMessage(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

/*** input ***/

void message(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}
char *editorPrompt(char *prompt, void (*callback)(char *, int), char *prev) {
  size_t bufsize = 128;
  char *buf = malloc(bufsize);
  size_t buflen = 0;
  buf[0] = '\0';
  while (1) {
    editorSetStatusMessage(prompt, buf);
    editorRefreshScreen();
    int c = editorReadKey();
    if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
      if (buflen != 0) buf[--buflen] = '\0';
    } else if (c == '\x1b') {
      editorSetStatusMessage("");
      if (callback) callback(buf, c);
      free(buf);
      return NULL;
    } else if (c == '\r') {
      if (buflen != 0) {
        editorSetStatusMessage("");
        if (callback) callback(buf, c);
        return buf;
      } else {
        memcpy(buf, prev, bufsize); // It makes bug
      }
    } else if (!iscntrl(c) && c < 128) {
      if (buflen == bufsize - 1) {
        bufsize *= 2;
        buf = realloc(buf, bufsize);
      }
      buf[buflen++] = c;
      buf[buflen] = '\0';
    }

    if (callback) callback(buf, c);
  }
}


void editorMoveCursor(int key) {
  erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
  switch (key) {
    case ARROW_LEFT:
      if (E.cx > 0) {
        E.cx--;
      } else if (E.cy > 0) {
        E.cy--;
        E.cx = E.row[E.cy].size;
      }
      break;
    case ARROW_RIGHT:
      if (row && E.cx < row->size) {
        E.cx++;
      } else if (row && E.cx == row->size) {
        E.cy++;
        E.cx = 0;
      }
      break;
    case ARROW_UP:
      if (E.cy != 0) {
        E.cy--;
      }
      break;
    case ARROW_DOWN:
      if (E.cy < E.numrows) {
        E.cy++;
      }
      break;
  }
  row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
  int rowlen = row ? row->size : 0;
  if (E.cx > rowlen) {
    E.cx = rowlen;
  } else if (E.cx < 0) {
    E.cx = 0;
  }
}

// TODO: fix cursor behaviour
void processKeyNormalMode_d(int num) {
  message("%dd...", num);
  editorRefreshScreen(); // display the message
  int c = editorReadKey();
  switch (c) {
  case 'd':
    editorCopyRow(E.cy);
    for (int i=0;i<num;i++)
      editorDelRow(E.cy);
    message("");
    break;
  default:
    message("%c is undefined", c);
  }
}

// TODO: fix cursor behaviour
void processKeyNormalMode_y(int num) {
  message("%dy...", num);
  editorRefreshScreen(); // display the message
  int c = editorReadKey();
  switch (c) {
  case 'y':
    editorCopyRow(E.cy);
    message("");
    break;
  default:
    message("%c is undefined", c);
  }
}
// TODO: fix cursor behaviour
void processKeyNormalMode_n(int num) {
  message("%d...", num);
  editorRefreshScreen(); // display the message
  int c = editorReadKey();
  switch (c) {
  case 'd':
    processKeyNormalMode_d(num);
    break;
  case 'y':
    processKeyNormalMode_y(num);
    break;
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    processKeyNormalMode_n(c - '0' + num * 10);
    break;
  default:
    message("%c is undefined", c);
  }
}

void editorProcessKeypressNormalMode() {
  char buf[100];
  int c = editorReadKey();
  switch (c) {
  case '1':
  case '2':
  case '3': 
  case '4': 
  case '5': 
  case '6': 
  case '7': 
  case '8': 
  case '9':
    processKeyNormalMode_n(c - '0');
    break; 
  case 'd':
    processKeyNormalMode_d(1);
    break;
  case 'y':
    processKeyNormalMode_y(1);
    break;
  case 'p':
    editorPasteRow();
    break;
  case 'i':
    E.mode = MODE_INSERT;
    break;
  case 'a':
    E.cx++; // TODO: bounds check
    E.mode = MODE_INSERT;
    break;
  case 'A':
    if (E.cy < E.numrows)
      E.cx = E.row[E.cy].size; // move to end of the line
    E.mode = MODE_INSERT;
    break;
  case 'I':
    E.cx = 0;
    E.mode = MODE_INSERT;
    break;
  case 'o':
    E.cx = E.row[E.cy].size; // move to end of the line
    editorInsertNewline();
    E.mode = MODE_INSERT;
    break;
  case ':':
  case '\'':
  case ';':
    editorColon();
    break;
  case 'k':
  case 'j':
  case 'h':
  case 'l':
    editorMoveCursor(c);
    break;
  case 'n':
    if (E.rownumber == 0) {
      E.rownumber = sprintf(buf, "%u", E.numrows) + 1;
    } else {
      E.rownumber = 0;
    }
    for (int i=0;i<E.numrows;i++)
      editorUpdateRow(&E.row[i]);
    break;
  //case 'w':
  //  editorMoveCursorWordForward();
  //  break;
  //case 'b':
  //  editorMoveCursorWordBackward();
  //  break;
  case 'J':
    editorJoinLines();
    break;
  case 'x':
    editorMoveCursor(ARROW_RIGHT);
    editorDelChar();
    break;
  case '$':
    if (E.cy < E.numrows)
      E.cx = E.row[E.cy].size; // move to end of the line
    break;
  case '^':
    E.cx = 0;
    break;
  case '0':
    E.cx = 0;
    break;
  case '/':
    editorFind();
    break;
  //case 'W': {
  //  point p = point_W(*E);
  //  E.cx = p.x;
  //  E.cy = p.y;
  //} break;
  case CTRL_KEY('f'): {
    E.cy = E.rowoff + E.screenrows - 1;
    if (E.cy > E.numrows)
      E.cy = E.numrows; // cap to end of file
    int times = E.screenrows;
    while (times--)
      editorMoveCursor(ARROW_DOWN);
  } break;
  case CTRL_KEY('b'): {
    E.cy = E.rowoff;
    int times = E.screenrows;
    while (times--)
      editorMoveCursor(ARROW_UP);
  } break;
  case 'G':
    E.cy = E.numrows - 1;
    E.cx = E.row[E.cy].size;
    break;
  case 'u': {
    //struct editorConfig E3 = 
    history_push(&E);
    //struct editorConfig E4 = history_undo(&E3);
    struct editorConfig E2 = history_undo(&E);
    memcpy(&E, &E2, sizeof(E2));
    E.mode = MODE_NORMAL;
    for (int i=0;i<E.numrows;i++)
      editorUpdateRow(&E.row[i]);
  } break;
  case CTRL_KEY('r'): {
    struct editorConfig E2 = history_redo(&E);
    memcpy(&E, &E2, sizeof(E));
    E.mode = MODE_NORMAL;
    for (int i=0;i<E.numrows;i++)
      editorUpdateRow(&E.row[i]);
  } break;
  case 'H':
    // temp - manually invoke history
  { E = history_push(&E);}
    break;
  case CTRL_KEY('s'):
    editorSave();
    break;
  case PAGE_UP:
  case PAGE_DOWN:
      {
        if (c == PAGE_UP) {
          E.cy = E.rowoff;
        } else if (c == PAGE_DOWN) {
          E.cy = E.rowoff + E.screenrows - 1;
          if (E.cy > E.numrows) E.cy = E.numrows;
        }
        int times = E.screenrows;
        while (times--)
          editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
      }
      break;
  case ARROW_UP:
  case ARROW_DOWN:
  case ARROW_LEFT:
  case ARROW_RIGHT:
    editorMoveCursor(c);
    break;
  default:
    message("%c is undefined", c);
  }
}

void editorProcessKeypress() {
  static int quit_times = KERO_QUIT_TIMES;
  int c = editorReadKey();
  switch (c) {
    case '\x1b':
      E.mode = MODE_NORMAL;
      break;
    case '\r':
      editorInsertNewline();
      break;
    case CTRL_KEY('q'):
      if (E.dirty && quit_times > 0) {
        editorSetStatusMessage("WARNING!!! File has unsaved changes. "
          "Press Ctrl-Q %d more times to quit.", quit_times);
        quit_times--;
        return;
      }
      editorQuit();
      break;
    case CTRL_KEY('s'):
      editorSave();
      break;
    case HOME_KEY:
      E.cx = 0;
      break;
    case END_KEY:
      if (E.cy < E.numrows)
        E.cx = E.row[E.cy].size;
      break;
    case CTRL_KEY('f'):
      editorFind();
      break;
    case BACKSPACE:
    case CTRL_KEY('h'):
    case DEL_KEY:
      if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
      editorDelChar();
      break;
    case PAGE_UP:
    case PAGE_DOWN:
      {
        if (c == PAGE_UP) {
          E.cy = E.rowoff;
        } else if (c == PAGE_DOWN) {
          E.cy = E.rowoff + E.screenrows - 1;
          if (E.cy > E.numrows) E.cy = E.numrows;
        }
        int times = E.screenrows;
        while (times--)
          editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
      }
      break;

    case ARROW_UP:
    case ARROW_DOWN:
    case ARROW_LEFT:
    case ARROW_RIGHT:
      editorMoveCursor(c);
      break;
    case CTRL_KEY('l'):
      break;
    default:
      editorInsertChar(c);
      break;
  }

  quit_times = KERO_QUIT_TIMES;
}

/*** init ***/
void initEditor() {
  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.rowoff = 0;
  E.coloff = 0;
  E.numrows = 0;
  E.row = NULL;
  E.dirty = 0;
  E.filename = NULL;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;
  E.syntax = NULL;
  E.mode = MODE_NORMAL;
  E.rownumber = 0;
  E.yank = malloc(0);
  E.yanksize = 0;
  E.undo = NULL;
  E.redo = NULL;

  if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
  E.screenrows -= 2;
}

int main(int argc, char *argv[]) {
  enableRawMode();
  initEditor();
  if (argc >= 2) {
    editorOpen(argv[1]);
  }
  E = history_push(&E);

  editorSetStatusMessage(
    "HELP: Ctrl-S = save | Ctrl-Q = quit | / = search | n = line number");
  while (1) {
    editorRefreshScreen();
    switch (E.mode) {
    case MODE_NORMAL:
      editorProcessKeypressNormalMode();
      break;
    case MODE_INSERT:
      editorProcessKeypress();
      break;
    }

    //editorProcessKeypress();
  }
  return 0;
}

/*** history ***/
struct editorConfig *copyEditorConfig(struct editorConfig *old) {
  struct editorConfig *new = malloc(sizeof(struct editorConfig));

  new->cx = old->cx;
  new->cy = old->cy;
  new->rx = old->rx;
  new->rowoff = old->rowoff;
  new->coloff = old->coloff;
  new->screenrows = old->screenrows;
  new->screencols = old->screencols;
  new->dirty = old->dirty;
  new->numrows = old->numrows;
  new->filename = old->filename;
  new->statusmsg_time = old->statusmsg_time;
  new->syntax = old->syntax; // pointer but that's fine.
  new->orig_termios = old->orig_termios;
  new->mode = old->mode;
  new->yanksize = old->yanksize;
  new->rownumber = old->rownumber;
  //new->yank[0] = *old->yank;
  new->yank = malloc(old->yanksize);
  memcpy(new->yank, old->yank, new->yanksize);
  new->statusmsg[0] = *old->statusmsg;
  new->undo = NULL; //old->undo;
  new->redo = old; // old->redo;

  // copy row
  new->row = malloc(sizeof(erow) * (old->numrows));
  int i;
  for (i = 0; i < new->numrows; i++) {
    new->row[i].idx = old->row[i].idx;
    new->row[i].size = old->row[i].size;
    new->row[i].rsize = old->row[i].rsize;
    new->row[i].hl_open_comment = old->row[i].hl_open_comment;

    new->row[i].chars = malloc(old->row[i].size);
    memcpy(new->row[i].chars, old->row[i].chars, old->row[i].size);

    new->row[i].render = malloc(old->row[i].rsize);
    memcpy(new->row[i].render, old->row[i].render, old->row[i].rsize);

    new->row[i].hl = malloc(old->row[i].rsize);
    memcpy(new->row[i].hl, old->row[i].hl, old->row[i].rsize);
    /* memset(new->row[i].hl, 0, old->row[i].rsize); // HL_NORMAL */
  }
  return new;
}

struct editorConfig history_push(struct editorConfig *snapshot) {
  struct editorConfig *new_e = copyEditorConfig(snapshot);
  //snapshot->redo = new_e;
  //new_e->undo = snapshot;
  if (snapshot->undo) {
    new_e->undo = snapshot->undo;
    snapshot->undo->redo = new_e;
    snapshot->undo = new_e;
    new_e->redo = snapshot;
  } else {
    snapshot->undo = new_e;
    new_e->redo = snapshot;
  }
  //return *new_e;
  return *snapshot;
}

struct editorConfig history_undo(struct editorConfig *e) {
  if (e->undo) {
    message("Undo");
    return *e->undo;
  };
  message("No Undo");
  return *e;
}

struct editorConfig history_redo(struct editorConfig *e) {
  if (e->redo) {
    message("Redo");
    return *e->redo;
  };
  message("No Redo");
  return *e;
}
