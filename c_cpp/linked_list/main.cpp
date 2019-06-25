#include "LinkedList.h"

#include <iostream>
#include <string>

#define EXIT_SUCCESS    0

using std::string;
using std::cout;
using std::endl;

int main(void) {
   LinkedList* list = new LinkedList();
   list->addFront(99);
   list->addFront(-1);
   list->addFront(7);

   cout << "List Size: " << list->size() << endl;
   for (int i = 0; i < list->size(); ++i) {
      cout << "\t Element[" << i << "]: " << list->get(i) << endl;
   }

   list->clear();
   list->addBack(99);
   list->addBack(-1);
   list->addBack(7);
   cout << "List Size: " << list->size() << endl;
   for (int i = 0; i < list->size(); ++i) {
      cout << "\t Element[" << i << "]: " << list->get(i) << endl;
   }

   cout << "contain 99: " << list->contains(99) << endl;
   list->deleteFront();
   cout << "contain 99: " << list->contains(99) << endl;
   cout << "contain 7: " << list->contains(7) << endl;
   list->deleteBack();
   cout << "contain 7: " << list->contains(7) << endl;

   delete list;

   return EXIT_SUCCESS;
}
