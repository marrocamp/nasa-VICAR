/* label_demo.c */
#include "pdsdef.h"
#include "pdsglob.h"
#include "label.h"
#include "labutil.h"
#include "fiodef.h"
#include "utildef.h"

#ifndef SUN_UNIX

char *read_string (char *prompt, char *string, LOGICAL upper);
int read_integer (char *prompt);

#else

char *read_string ();
int read_integer ();

#endif


int main ()

{        
char input[3];
int num = 0;
int status = -2;
AGGREGATE label_ptr = {NULL};
AGGREGATE object_ptr = {NULL};
PARAMETER parameter_ptr = {NULL};
VALUE value_ptr = {NULL};
char filename[PDS_MAXLINE];
char *parent_object_class;
char *parent_object_name;
char *new_object_class;
char *object_class;
char *object_name;
char *parameter_name;
char *new_parameter_name;
char *parameter_value;
char *top_ddid;
char *bottom_ddid;
char *label_type;
char *record_type;
char *template_file_name;
char string1 [PDS_MAXLINE];
char string2 [PDS_MAXLINE];
char string3 [PDS_MAXLINE];
char string4 [PDS_MAXLINE];
char string5 [PDS_MAXLINE];
int record_length = 0;
int version_id = 0;
int parent_object_position = 0;
int object_position = 0;
int parameter_position = 0;
STRING_LIST *string_list = NULL;
STRING_LIST *temp_list = NULL;
int record_flag;

   lab_setup ();
   while (num != 1)
   {
      status = PDS_SUCCESS;
      if (num == 0)
      {
          printf("\n\n\n");
          printf("                         Main Menu\n\n\n");          
          printf("  1) Exit\n");          
          printf("\n");
          printf("  2) Read in a template\n");          
          printf("  3) Add a new object\n");          
          printf("  4) Add a new parameter to an object\n");          
          printf("\n");
          printf("  5) Remove a label or template\n");          
          printf("  6) Remove an object\n");          
          printf("  7) Remove a parameter\n");          
          printf("\n");
          printf("  8) Change an object class\n");          
          printf("  9) Change a parameter name\n");          
          printf(" 10) Change the value of a parameter\n");          
          printf("\n");
          printf(" 11) Clear the error messages\n");
          printf(" 12) Display the error messages\n");
          printf(" 13) Display the values(s) of one parameter\n");          
          printf(" 14) Display the PDS label\n"); 
          printf(" 15) Write the completed label\n"); 
          printf("\n");
          printf(" 16) Start a new label\n"); 
          printf("\n");
          printf("Please enter your selection: ");
          gets(input);
          num = atoi(input);
          printf ("\n\n");
      }

      switch(num)
      {

        case 2:  read_string ("Enter the label or template file name: ", filename,0);

                 label_ptr = lab_read_label_or_template(filename);
                 status = (label_ptr != NULL);
                 break;
         
        case 3:  parent_object_class = read_string (
                         "Enter the parent object's class: ", string1,1);
                 parent_object_name = read_string (
                         "Enter the parent object's name: ", string2,1);
                 parent_object_position = read_integer (
                         "Enter the parent object's position: ");
                 new_object_class = read_string (
                         "Enter the new object's class: ", string3,1);

                 object_ptr = lab_add_object(label_ptr, parent_object_class,
                             parent_object_name, parent_object_position, 
                             new_object_class, &status);
                 break;
  
	case 4:  object_class = read_string (
                         "Enter the object's class: ", string1,1);
                 object_name = read_string (
                         "Enter the object's name: ", string2,1);
                 object_position = read_integer (
                         "Enter the object's position: ");
                 parameter_name = read_string (
                         "Enter the name of the parameter: ", string3,1);
                 parameter_value = read_string (
                         "Enter the value of the parameter: ", string4,0);

                 parameter_ptr = lab_add_parameter (label_ptr, object_class, 
                                                    object_name, object_position, 
                                                    parameter_name, parameter_value, 
                                                    &status);
                 break;
               
        case 5:  printf ("Removing the tree ...\n");
                 label_ptr = lab_remove_label_or_template(label_ptr);
                 break;

        case 6:  object_class = read_string (
                         "Enter the object's class: ", string1,1);
                 object_name = read_string (
                         "Enter the object's name: ", string2,1);
                 object_position = read_integer (
                         "Enter the object's position: ");

                 object_ptr = lab_remove_object(label_ptr, object_class,
                                         object_name, object_position, &status);
                 break;

        case 7:  object_class = read_string (
                         "Enter the object's class: ", string1,1);
                 object_name = read_string (
                         "Enter the object's name: ", string2,1);
                 object_position = read_integer (
                         "Enter the object's position: ");
                 parameter_name = read_string (
                         "Enter the name of the parameter: ", string3,1);
                 parameter_position = read_integer (
                         "Enter the parameter's position: ");

                 object_ptr = lab_remove_parameter (label_ptr, object_class, 
                                                       object_name, object_position, 
                                                  parameter_name, parameter_position, 
                                                       &status);
                 break;

        case 8: object_class = read_string (
                         "Enter the object's class: ", string1,1);
                 object_name = read_string (
                         "Enter the object's name: ", string2,1);
                 object_position = read_integer (
                         "Enter the object's position: ");
                 new_object_class = read_string (
                         "Enter the object's new class: ", string3,1);

                 lab_change_object_class (label_ptr, object_class,
                                          object_name, object_position, 
                                          new_object_class, &status);

                 break;

        case  9: object_class = read_string (
                         "Enter the object's class: ", string1,1);
                 object_name = read_string (
                         "Enter the object's name: ", string2,1);
                 object_position = read_integer (
                         "Enter the object's position: ");
                 parameter_name = read_string (
                         "Enter the name of the parameter: ", string3,1);
                 parameter_position = read_integer (
                         "Enter the parameter's position: ");
                 new_parameter_name = read_string (
                         "Enter the name of the new parameter: ", string4,1);

                 lab_change_parameter_name (label_ptr, object_class, object_name, 
                                            object_position, parameter_name, 
                                            parameter_position, new_parameter_name, 
                                            &status);
                 break;

        case 10:  object_class = read_string (
                         "Enter the object's class: ", string1,1);
                 object_name = read_string (
                         "Enter the object's name: ", string2,1);
                 object_position = read_integer (
                         "Enter the object's position: ");
                 parameter_name = read_string (
                         "Enter the name of the parameter: ", string3,1);
                 parameter_position = read_integer (
                         "Enter the parameter's position: ");
                 parameter_value = read_string (
                         "Enter the value of the parameter: ", string4,0);

                 value_ptr = lab_change_value(label_ptr, object_class, object_name, 
                                              object_position, parameter_name, 
                                              parameter_position, parameter_value,
                                              &status);
                 break;

        case 11: printf("Clearing messages ");
                 lab_clear_messages ();
                 break;

        case 12: printf ("The message list contains:\n\n");
                 lab_print_messages();
                 break;

        case 13: object_class = read_string (
                         "Enter the object's class: ", string1,1);
                 object_name = read_string (
                         "Enter the object's name: ", string2,1);
                 object_position = read_integer (
                         "Enter the object's position: ");
                 parameter_name = read_string (
                         "Enter the name of the parameter: ", string3,1);
                 parameter_position = read_integer (
                         "Enter the parameter's position: ");
                 string_list = lab_get_all_values (label_ptr, object_class, 
                                                   object_name, object_position, 
                                                   parameter_name, parameter_position,
                                                   TRUE, &status);
                 printf ("The value is:\n");
                 for (temp_list = string_list; temp_list != NULL;
                      temp_list = temp_list -> next)
                    printf ("%s\n", temp_list -> text);
                 string_list = util_deallocate_string_list (string_list);
                 break;

        case 14: printf("The tree is:\n\n");
                 lab_print_pds_label(label_ptr);
                 break;

        case 15: top_ddid = read_string ("Enter the Top DDID: ", string1,1);

                 if ((strcmp(top_ddid,"NULL")) == 0)
                   top_ddid = NULL;

                 bottom_ddid = read_string("Enter the Bottom DDID: ", string2,1);
   
                 label_type = read_string("Enter the SFDU label type: ", string3,1);

                 record_type = read_string("Enter the file type: ", string4,0);
                 record_type = util_lower_case(record_type);
                 if ((strcmp(record_type,"fixed")) == 0)
                 {
                     record_length = read_integer("Enter the record length: ");
#ifndef MSDOS_TC
		     record_flag = PDS_RF_FIXED_LF;
#else
                     record_flag = PDS_RF_FIXED_CRLF;
#endif
		 }
                 else
#ifndef MSDOS_TC
		     record_flag = PDS_RF_STREAM_LF;
#else
                     record_flag = PDS_RF_STREAM_CRLF;
#endif
                 version_id = read_integer("Enter the SFDU version: ");

                 template_file_name = read_string("Enter the file name: ",string5,0);

                 status = lab_write_product_label (label_ptr, top_ddid,
                                bottom_ddid, version_id, label_type, record_flag,
                                record_length, template_file_name); 

                 break;

        case 16: label_ptr = lab_remove_label_or_template (label_ptr);
                 lab_clear_messages ();
                 label_ptr = lu_append_object (NULL, NULL);                 
                 status = (label_ptr != NULL);
                 break;
  
      }  /* end switch */

      printf ("\n\n");

      if (num != 1)
      {
          switch (status)
          {
              case PDS_SUCCESS : printf ("SUCCESS - Operation completed successfully.\n");
                                 break;

              case PDS_ERROR   : printf ("ERROR - Operation not performed.\n");
                                 break;
   
              case PDS_MULTIPLE_OBJECTS : 
                                 printf ("ERROR - Ambiguous object reference.\n ");
                                 break;

              case PDS_MULTIPLE_PARMS : 
                                 printf ("ERROR - Ambiguous parameter reference.\n ");
                                 break;

              default          : printf ("ERROR - Unknown status.\n");
                                 break;

          }  /* end switch */
       
          printf ("\nPlease press RETURN: ");
          gets(input);
          num = atoi(input);
          printf ("\n");

      } /* end if */

   } /* end while */
   lab_exit ();

}  /*  End:  "test_driver"  */



char *read_string(prompt, string, upper)
char *prompt;
char *string;
LOGICAL upper;
{
  if (string != NULL)
  {
    printf("%s",prompt);
    gets (string); 
    if (string [0] == EOS)
        return (NULL);
  }

  if (upper) util_upper_case (string);
  return(string);
}

int read_integer(prompt)
char *prompt;
{
  char s [PDS_MAXLINE];
  printf("%s",prompt);
  gets (s);
  return(atoi(s));
}
    
