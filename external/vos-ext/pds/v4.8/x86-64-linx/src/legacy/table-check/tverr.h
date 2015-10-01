/*
   TVERR.H

   Header file to define error code constants for PDSTV and the 'tverr'
   subroutine.

   02 January 2002, ACR.
*/

#ifndef TVERR
#define TVERR

#define VERSION_ID   "1.6"

/* Error codes: */

#define USAGE                    999

/* ...Label Errors... */

#define BAD_COLNUM               101
#define BAD_DER_MAXMIN           102
#define BAD_END_OBJECT           103
#define BAD_REPCOUNT             104
#define BAD_SPARE_TYPE           105
#define COLUMN_MISCOUNT          106
#define CONTAINER_TOO_LONG       107
#define DBLMAX_LT_MIN            108
#define DBLVALMAX_LT_VALMIN      109
#define DERMAX_LT_DERMIN         110
#define EMPTY_CONTAINER          111
#define FIELD_TOO_LONG           112
#define FORMAT_TOO_BIG           113
#define FORMAT_TYPE_MISMATCH     114
#define INT_TYPE_CONFLICT        115
#define INV_CHAR_IN_FN           116
#define INV_FILE_RECORDS         117
#define INV_FORMAT               118
#define INV_INT_SIZE             119
#define INV_REAL_SIZE            120
#define INV_RECORD_BYTES         121
#define INV_RECORD_TYPE          122
#define INV_REP                  123
#define INV_SUBOBJECT            124
#define ITEM_BYTES_MISMATCH      125
#define MAX_TOO_LONG             126
#define MIN_TOO_LONG             127
#define MISSING_POINTER          128
#define MISSING_SCALE            129
#define MIXED_CASE_IN_FN         130
#define MULT_EXT_IN_FN           131
#define NO_EXT_IN_FN             132
#define NO_FILE_RECORDS          133
#define NO_RECORD_BYTES          134
#define NO_RECORD_TYPE           135
#define NO_REPCOUNT              136
#define NO_TABLES                137
#define OBJLBL_MISMATCH          138
#define OBSOLETE_KEYWORD         139
#define OBSOLETE_TYPE            140
#define OPEN_FAIL                141
#define PREC_GT_WIDTH            142
#define QUOTED_UNDERSCORES       143
#define ROWB_NE_RECB             144
#define SCALE_EQ_0               145
#define STARTBYTE_LT_1           146
#define STARTBYTE_GT_PAR_BYTES   147
#define STARTBYTE_GT_ROW_BYTES   148
#define STRMAX_LT_MIN            149
#define STRVALMAX_LT_VALMIN      150
#define UNEXP_END                151
#define UNEXP_EOF                152
#define UNNAMED_COLUMN           153
#define UNNAMED_CONTAINER        154
#define VALUE_TOO_LONG           155
#define WIDTH_GT_BYTES           156
#define WIDTH_GT_MAX_DATE        157
#define WIDTH_GT_MAX_TIME        158
#define WIDTH_LT_BYTES           159
#define WIDTH_LT_MIN_DATE        160
#define WIDTH_LT_MIN_TIME        161
/*#define INV_REAL_TYPE            162*/

/* ...Data (File) Errors... */

#define DATA_FILE_NOT_FOUND      201
#define FILE_SEEK_FAILED         202
#define MAX_ERRORS               203
#define OOPS_NUMBER_SIZE         204
#define TOO_FEW_TABLE_ROWS       205


/* ...Data (Field) Errors... */

#define   GREATER_THAN_MAX             0
#define   LESS_THAN_MIN                1
#define   GREATER_THAN_DMAX            2
#define   LESS_THAN_DMIN               3
#define   GREATER_THAN_VMAX            4
#define   LESS_THAN_VMIN               5
#define   INVALID_INTEGER              6
#define   INVALID_REAL                 7
#define   NONPRINT_CHAR                8
#define   LENGTH_NE_RECORD_BYTES       9
#define   NO_CR                       10
#define   E_FORMAT_MISSING_EXPONENT   11
#define   DECIMAL_NOT_ALIGNED         12
#define   INTEGER_NOT_RIGHT_JUSTIFIED 13
#define   BLANK_FIELD                 14
#define   INVALID_DATE                15
#define   INVALID_TIME                16

#define  ERRORLISTCOUNT     17

static char    *error_msg[]   = 
{ /*  0 */ "Value greater than MAXIMUM",
  /*  1 */ "Value less than MINIMUM",
  /*  2 */ "Adjusted value greater than DERIVED_MAXIMUM",
  /*  3 */ "Adjusted value less than DERIVED_MINIMUM",
  /*  4 */ "Value greater than VALID_MAXIMUM",
  /*  5 */ "Value less than VALID_MINIMUM",
  /*  6 */ "Not a valid integer",
  /*  7 */ "Not a valid real number",
  /*  8 */ "Non-printing character in CHARACTER field",
  /*  9 */ "Record length not equal to RECORD_BYTES",
  /* 10 */ "Record does not have carriage-return (\\r)",
  /* 11 */ "'E' format field does not have an exponent",
  /* 12 */ "'F' format field decimal point does not align with precision value",
  /* 13 */ "'I' format field is not right-justified",
  /* 14 */ "Blank field",
  /* 15 */ "Invalid DATE field (expecting YYYY-MM-DD)",
  /* 16 */ "Invalid TIME format (expecting YYYY-MM-DDThh:mm:ss[.sss][Z])"
 };


/* Subroutine declaration: */

int tverr(int ecode,...);

#endif
