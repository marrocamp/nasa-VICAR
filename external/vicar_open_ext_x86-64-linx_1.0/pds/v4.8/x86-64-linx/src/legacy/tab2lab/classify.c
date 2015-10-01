/*****************************************************************************

  Module: classify.c
 
  Description: Routines for determining the class of objects described in
               a PDS label.

  Author:  Randy Davis, University of Colorado LASP

  Creation Date:  13 March 1991

  History:

    Creation - These routines are part of the first release of the PDSL
               library.

    3/19/03  DWS changed all references to node->class to node->pds_class
*****************************************************************************/

#include "odldef.h"
#include "odlinter.h"

extern int        ODLlinenumber_flag;   /* Flag to control line no. output  */

#define TRUE 1
#define FALSE 0



/*****************************************************************************

  Routine: ClassifyObject

  Description: Classifies the object pointed to by the first argument.
               There are two ways in which classification occurs.  Both
               start with the identifier 'x' given in the OBJECT = x
               part of the object description.  The methods are:
            
                 1) For newer PDS labels, 'x' is the object class
                    and the specific object's name 'y' is given
                    by an attribute assignment NAME = y.
                 2) In older PDS labels, the class is the last word
                    of 'x'.  Such labels can be identified by the
                    fact that they do not have the NAME = y
                    attribute assignment in the object description.

               The program first determines the class using the rules
               above and then compares the class to the known classes
               in the object definition library supplied as the second
               argument.

  Input:
          node        - Pointer to the aggregate node to be classified.
          objdef_tree - Pointer to an ODL tree containing object definitions
                        for use in classification.  This argument is optional:
                        if object definitions are unneeded or unavailable,
                        this argument should be set to NULL.

  Output: The function value is set to indicate status of the operation:
                0  - Couldn't classify the object, or no object to classify
                1  - Object has been classified

          The name and class fields of the aggregate node pointed to
          by the first argument are modified by this routine.

*****************************************************************************/

int ClassifyObject (AGGREGATE node, AGGREGATE objdef_tree)
{
  AGGREGATE  objdef;              /* Current object definition              */
  PARAMETER  name_attribute;      /* Attribute containing object name       */
  VALUE      data;                /* Value of object name attribute         */
  char      *sp;                  /* Pointer to class word in object name   */


  if (node == NULL || node->kind != KA_OBJECT)
    {
      /* There is no object node to classify, so return immediately */

      return (0);
    }
  else if (node->pds_class != NULL && node->pds_class[0] != '\0')
    {
      /* The object is already classified, so return success */

      return (1);
    }

  /* See if the object has an attribute specifying the object name */

  name_attribute = FindParameter (node, "NAME");
  if (name_attribute != NULL && name_attribute->node_kind == KP_ATTRIBUTE)
    {
      /* The object does have a name attribute, so the value that is
         now in the object node's name field must be the object's
         class name. Get the object name from the name attribute */

      data = FirstValue (name_attribute);
      if (data != NULL &&
          (data->item.type == TV_SYMBOL || data->item.type == TV_STRING))
        {
          /* Transfer the class name to the object node's class field
             and then transfer the name specified by the attribute to
             the object node's name field */

          if (node -> pds_class) free (node -> pds_class);
          node->pds_class = node->name;
          node->name = data->item.value.string;

          /* Delete the name attribute from the object (but not
             its value, which is now the object node's name */

          data->item.type = TV_NULL;
          RemoveParameter (name_attribute);

          return (1);
        }
      else
        {
          /* There is something funny about the value of the name
             attribute and we dare not use it */

          return (0);
        }
    }
  else if (objdef_tree != NULL)
    {
      /* There was no name attribute for the object: see if the object
         class is mentioned in the current name.  Start by finding the
         last word in the object name */

      sp = strrchr (node->name, '_');
      if (sp == NULL)
        {
          /* There is no underscore, so the whole name must 
             be a single word */

          sp = node->name;
        }
      else
        {
          /* There was an underscore: point to first character after it,
             which is the first character of the class word */

          sp++;
        }

      /* Compare the class word against the names of all known object
         classes */

      objdef = FindObject (objdef_tree, sp, "OBJECT_DEFINITION");
      if (objdef != NULL)
        {
          /* We found an object definition that matches the class word
             in the object name.  Put this class in the object node's
             class field */
          
          if (node -> pds_class) free (node -> pds_class);
          node->pds_class = (char *) malloc (strlen (objdef->name)+1);
          if (node->pds_class != NULL)
            {
              /* Copy the class name */

              strcpy (node->pds_class, objdef->name);

              return (1);
            }
          else
            {
              /* Couldn't allocate space to hold the class name */
 
              return (0);
            }
        }
      else
        {
          /* Couldn't locate an object class matching the class word */

          return (0);
        }
    }
  else
    {
      /* There class name can't be determined from the object description
         alone, and object definitions aren't available for us to use */

      return (0);
    }
 }




/*****************************************************************************

  Routine: ClassifyAll

  Description: Classifies the objects pointed to by the first argument and
               all of its subobjects.

  Input:
          base_node   - Pointer to the aggregate node to be classified.  This
                        node and all its progeny objects will be classified.
          objdef_tree - Pointer to an ODL tree containing object definitions
                        for use in classification.  This argument is optional:
                        if object definitions are unneeded or unavailable,
                        this argument should be set to NULL.

  Output: None.  The name and class fields in object nodes in the ODL
          tree may be modified by this routine.  Messages identifying
          objects that can't be classified are written using routine
          ODLPrintInfo.

*****************************************************************************/

void ClassifyAll (AGGREGATE base_node, AGGREGATE objdef_tree)
{
  AGGREGATE  node;                /* Aggregate being processed              */
  int        result;              /* Result of classifying object           */
  char       message[120];        /* String to hold information message     */
#ifndef SUN_UNIX

   int ClassifyObject (AGGREGATE node, AGGREGATE objdef_tree);

#else

   int ClassifyObject ();

#endif

  /* Inhibit line number output with warning messages */

  ODLlinenumber_flag = FALSE;

  /* Start with the base node and stop searching when we  have visited all
     of the progeny of the base node  */

  if (base_node == NULL)
    {
      return;
    }
  else if (base_node->kind != KA_OBJECT)
    {
      node = NextSubAggregate (base_node, base_node);
    }
  else
    {
      node = base_node;
    }

  while (node != NULL)
    {
      result = ClassifyObject (node, objdef_tree);
      if (result == 0)
        {
          sprintf (message, "Can't determine class of object %s",
                   node->name);
	  ODLPrintWarning (message);
        }

      node = NextSubObject (base_node, node);
    }

  return;
}




