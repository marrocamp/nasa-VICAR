/*************************************************************************
 lvtool_utils.h


 Change History:
     06-07-05       Original code

 **************************************************************************/

#include "pdsdef.h"
#include "utildef.h"

char *create_start_path(char *, char *);

STRING_LIST *find_pointer_files(char *, char *, STRING_LIST *, char *);

int dir_exists(char *);

extern STRING_LIST *slv_get_volume_list (char *, LOGICAL);
extern STRING_LIST  * dir_walk(char *, char *, STRING_LIST *, LOGICAL, LOGICAL);
extern int   dir_walk_2(char *);
