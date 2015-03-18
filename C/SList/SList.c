#include <stdlib.h>
#include <math.h>
#include "SList.h"

#define MAX_ELEMENT_SIZE	32

SList *slist_append(SList *list, void *data){

      SList *newNode = (SList *)malloc(sizeof(SList));
      SList *node;
      newNode->data = data;
      newNode->next = NULL;
      if (list == NULL) {
         return newNode;
      }
      node = list;
      while (slist_next(node) != NULL) {
            node = slist_next(node);
      }

      node->next = newNode;
      return list;
}

int *slist_get_elem (SList **list){

	if(*list != NULL){
		SList *tmp = *list;
		int *ret = (int *)malloc(sizeof(int));
                *ret = (int)slist_data(*list);
		*list = slist_next(*list);
		slist_next(tmp) = NULL;
		slist_destroy(tmp);
		return ret;
	}
	return NULL;
}

void  slist_destroy(SList *list){

      SList *nodeToDelete;
      while (list != NULL) {
            nodeToDelete = list;
            list = slist_next(list);
            free(nodeToDelete);
      }
}
