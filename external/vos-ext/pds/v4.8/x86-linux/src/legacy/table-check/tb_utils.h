
#include "oal.h"
#include "lablib3.h"
#include <string.h>
#include <stdlib.h>


typedef int LOGICAL;
#define Lemme_Go(ptr) { if (ptr != NULL) free(ptr); ptr = NULL; }	


#define PDS_MAXLINE  257       /*  Length of a line read from    */
#define EOS  0 /*end of string*/

#define PDS_ERROR               0   /* Routine unable to perform task */
#define PDS_SEARCH_FAIL         -1

	/*------------------------------------------------------------*/
	/*  The String_End macro calculates the address of the last   */
	/*  character in a string.  PLEASE NOTE THAT IF THE STRING IS */
	/*  EMPTY, THIS MACRO WILL RETURN A -1.                       */
	/*------------------------------------------------------------*/

#define String_End(zxz) ((zxz+strlen(zxz)-sizeof(char)))

	/*------------------------------------------------------------*/
	/*  The Replace_String macro frees the old value of a string, */
	/*  allocates space for a new one, and copies it into the new */
	/*  space.                                                    */
	/*------------------------------------------------------------*/

#define Replace_String(zoldz, znewz) \
	{ \
		free(zoldz); \
		New_String(zoldz, znewz); \
	}
	/*------------------------------------------------------------*/
	/*  The Make_Long macro is designed for use with lab routines.*/
	/*  It returns a 0 if the string passed in is NULL, or the    */
	/*  long integer representation of the string otherwise.      */ 
	/*------------------------------------------------------------*/

#define Make_Long(x) ((x == NULL) ? 0 : atol (x))

	/*------------------------------------------------------------*/
	/*  The String_Size macro calculates the number of characters */
	/*  in a string (including the character position used by the */
	/*  EOS character.  This can be used whenever it is necessary */
	/*  to malloc storage for a character string, for example:    */
	/*  new_str = malloc (String_Size(old_str));                  */
	/*------------------------------------------------------------*/

#define String_Size(zxz) (strlen(zxz)+sizeof(char)) 

	/*------------------------------------------------------------*/
	/*  The Check_Malloc macro checks to given pointer to see if  */
	/*  it is NULL. If it is, the macro prints an error message   */
	/*  and exits.                                                */
	/*------------------------------------------------------------*/

#define Check_Malloc(zptrz) if (zptrz == NULL) \
          { \
           printf("Check_Malloc, out of memory and must stop.\n");\
           exit (1);\
          }
	 
	/*------------------------------------------------------------*/
	/*  The Malloc_String macro allocates space for a string.  If */
	/*  no space is available, the program exits.  The first      */
	/*  character of the string is initialized to EOS.            */
	/*------------------------------------------------------------*/

#define Malloc_String(zstrz, zlenz) \
           { \
             int znumz = {zlenz};\
             if (znumz <= 0) znumz = 1;\
             zstrz = (char *) malloc(znumz);\
             Check_Malloc(zstrz);\
             *zstrz = EOS;\
           }

	/*------------------------------------------------------------*/
	/*  The New_String macro allocates storage for a string, then */
	/*  copies it into the newly allocated space.  It does not    */
	/*  free the string pointer first.                            */
	/*------------------------------------------------------------*/

#define New_String(zoldz, znewz)\
         {\
           if (znewz == NULL) \
             Malloc_String(zoldz, 1)\
           else\
             {\
             Malloc_String(zoldz, String_Size(znewz))strcpy (zoldz, znewz);\
             }\
         }

/* 01-24-03 MDC - Added macro definition */ 

#define Close_Me(zfilez) \
    { if (zfilez != NULL) \
       fclose(zfilez); \
       zfilez = NULL; \
    }

#define Read_Char(zcharz) \
         {\
            short zcz = {NULL};\
            zcharz = NULL;\
            while ((zcz = getchar()) != '\n')zcharz = zcz;\
         }


#define GetLineNum(ptr,kwd,result) \
	{ \
		KEYWORD *kp; \
		kp = OdlFindKwd(ptr, kwd, NULL, 1, ODL_THIS_OBJECT); \
		if(kp != NULL) {result = kp->line_number;} \
		else result = ptr->line_number; \
	}

/*------------------------------------------------------------------------*/
/*                     Enums and Enum Typedefs                            */
/*------------------------------------------------------------------------*/

enum     Aggregate_Kind          {KA_OBJECT, KA_GROUP};
typedef  enum Aggregate_Kind     AGGREGATE_KIND;

enum     Parameter_Kind          {KP_ATTRIBUTE, KP_POINTER};
typedef  enum Parameter_Kind     PARAMETER_KIND;

enum     Value_Kind              {KV_UNKNOWN, KV_SCALAR, KV_SEQUENCE, KV_SET};
typedef  enum Value_Kind         VALUE_KIND;

enum     Value_Type              {TV_NULL, TV_INTEGER, TV_REAL, TV_SYMBOL,
				  TV_STRING, TV_DATE, TV_TIME, TV_DATE_TIME};
typedef  enum Value_Type         VALUE_TYPE;



typedef struct
{
   char name [PDS_MAXLINE + 1];
   unsigned long location;
   char file_name [PDS_MAXLINE + 1];
   LOGICAL is_attached;
   LOGICAL has_byte_loc;
} POINTER_INFO;


/* The following structure contains information about a parameter -- an
   attribute of an object or a pointer.  This includes the attribute or
   pointer name and the kind of value associated with the parameter
   (scalar, sequence or set).  For sequences the number of rows and
   columns are given.  Each parameter node contains a pointer to the
   aggregate -- group or object -- node to which it belongs and a list
   of values associated with the parameter.  All of the parameter nodes
   for an aggregat are threaded together in a doubly-linked list.  */

struct Parameter_Node
{
   char                  *name;           /* The name of the parameter      */
   char                  *comment;        /* Annotation                     */
   PARAMETER_KIND         node_kind;      /* Param kind (Attribute or Ptr)  */
   VALUE_KIND             value_kind;     /* Indicates the kind of value    */
   long                   value_count;    /* Total number of values         */
   short                  columns;        /* Number of columns in sequence  */
   short                  rows;           /* Number of rows in sequence     */
   struct Aggregate_Node *owner;          /* Pointer to parameter's owner   */
   struct Parameter_Node *left_sibling;   /* Pointer to parameter to left   */
   struct Parameter_Node *right_sibling;  /* Pointer to parameter to right  */
   struct Value_Node     *first_value;    /* Pointer to first value node    */
   struct Value_Node     *last_value;     /* Pointer to last value node     */



};


/* The following structure contains information about a single object or
   group including its name and (for objects) its class.  The structure
   contains pointers that thread the object/group nodes together and that
   connect an object or group with attributes.  */

struct Aggregate_Node
{
   char                  *name;            /* Name of the object or group   */
   char                  *tb_class;           /* The class of object           */
   char                  *comment;         /* Annotation                    */
   AGGREGATE_KIND         kind;            /* Kind of node (object or group)*/
   long                   appl1;           /* Available for application use */
   long                   appl2;           /* Available for application use */
   struct Aggregate_Node *parent;          /* Pointer to parent node        */
   struct Aggregate_Node *left_sibling;    /* Pointer to sibling on left    */
   struct Aggregate_Node *right_sibling;   /* Pointer to sibling on right   */
   struct Aggregate_Node *first_child;     /* Pointer to first child        */
   struct Aggregate_Node *last_child;      /* Pointer to last child         */
   struct Parameter_Node *first_parameter; /* Pointer to first parameter    */
   struct Parameter_Node *last_parameter;  /* Pointer to last parameter     */
};
typedef struct Aggregate_Node *AGGREGATE;


extern void checkfilename(char *, int);

char *util_strip_lead_and_trail (char *, char);
char *sys_get_path (char *);
LOGICAL lt_get_pointer (OBJDESC *, char *, int, POINTER_INFO *);
char *util_lower_case (char *);
LOGICAL util_string_is_empty (char *);
char *util_create_file_spec(char *, char *);
char *util_remove_char (char *, char);
char *util_compress_char(char *, char);
char *util_upper_case (char *);
long search_string_array (char *[], long , char *);