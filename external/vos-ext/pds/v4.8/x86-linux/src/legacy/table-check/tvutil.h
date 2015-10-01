/*
  TVUTIL.H

  Function declarations for the 'tvutil.c' routines.

  16 March 2001, ACR.

*/



/* Functions: */

int   getline(char *, FILE *, int *);
/*int   breakline(char *line, char *keyword, char *value, int linenum); */
char *copy_of (char *);
void  printerror(int, int, int [], int, COLUMN_INFO *, FILE *);
int   count_column_types(COLUMN_INFO *, int *, int *, int *, int *, int *, int *);
void  writenumsum(COLUMN_INFO *, FILE *);
void  writecharsum(COLUMN_INFO *, FILE *);
int   findprec(double);
void  checkcharfld(char *, int, int, COLUMN_INFO *, int [], FILE *);
void  checkdatefld(char *, int, int, COLUMN_INFO *, int [], FILE *);
void  checktimefld(char *, int, int, COLUMN_INFO *, int [], FILE *);

void  print_table_attributes(TABLE_INFO *, FILE *);
int   trimcmp(char *, char *);
char *trim_copyof(char *);
int   spare_column(COLUMN_INFO *);
CONTAINER_INFO *find_container(TABLE_INFO *, long);

