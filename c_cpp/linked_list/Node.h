#ifndef NODE_H
#define NODE_H

class Node {
public:

   Node(int data, Node* next);
   Node(Node& other);

   // THIS IS PUBLIC!
   // Question: Is this being public good or bad?
   int   data;
   Node* next;
};

#endif // NODE_H
