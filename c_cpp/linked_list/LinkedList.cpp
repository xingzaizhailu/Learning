#include "LinkedList.h"

#include <stdexcept>
#include <string>

LinkedList::LinkedList() {
    head = NULL;
}

LinkedList::~LinkedList() {
    clear();
    delete head;
}

int LinkedList::size() {
    int count = 0;
    Node* cur = head;

    while(cur) {
        count++;
        cur = cur->next;
    }

    return count;
}

void LinkedList::clear() {
    Node* cur = head;

    while(cur){
        head = head->next;
        delete cur;
        cur = head;
    }

    head = NULL;
}

int LinkedList::get(int i) {
    Node* cur = head;
    int count = 0;

    while(count < i && cur){
        if(count == i){
            break;
        }
        cur = cur->next;
        count++;
    }

    if (count < i){
        return -999;
    } else {
        return cur->data;
    }
}

void LinkedList::addFront(int data) {
    Node* new_node = new Node(data, head);
    head = new_node;
}

void LinkedList::addBack(int data) {
    Node* new_node = new Node(data, NULL);
    Node* cur = head;

    if(!head){
        head = new_node;
    } else {
        while(cur->next){
            cur = cur->next;
        }
        cur->next = new_node;
    }
}

bool LinkedList::contains(int value){
    Node* cur = head;
    bool contain = false;

    while(cur){
        if(cur->data == value){
            contain = true;
            break;
        }
        cur = cur->next;
    }
    return contain;
}

void LinkedList::deleteFront() {
    if(head){
        Node* to_delete = head;
        head = head->next;
        delete to_delete;
    }
}

void LinkedList::deleteBack() {
    Node* cur = head;

    if(!cur){
        return ;
    }

    if(!cur->next){
        head = NULL;
        delete cur;
        return ;
    }

    while(cur->next->next){
        cur = cur->next;
    }
    delete cur->next;
    cur->next = NULL;
}
