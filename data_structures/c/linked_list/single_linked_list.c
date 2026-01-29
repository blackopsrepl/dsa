#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/**
 * Singly Linked List Implementation
 *
 * A linked list where each node contains data and a pointer to the next node.
 * Supports O(1) insertion at head, O(n) insertion at tail, and O(n) deletion.
 */

typedef struct Node {
    int data;
    struct Node* next;
} Node;

typedef struct LinkedList {
    Node* head;
    int size;
} LinkedList;

Node* create_node(int data) {
    Node* node = malloc(sizeof(Node));
    if (node == NULL) {
        return NULL;
    }
    node->data = data;
    node->next = NULL;
    return node;
}

void list_init(LinkedList* list) {
    list->head = NULL;
    list->size = 0;
}

void add_first(LinkedList* list, int data) {
    Node* new_node = create_node(data);
    if (new_node == NULL) return;

    new_node->next = list->head;
    list->head = new_node;
    list->size++;
}

void add_last(LinkedList* list, int data) {
    Node* new_node = create_node(data);
    if (new_node == NULL) return;

    if (list->head == NULL) {
        list->head = new_node;
    } else {
        Node* current = list->head;
        while (current->next != NULL) {
            current = current->next;
        }
        current->next = new_node;
    }
    list->size++;
}

bool insert_at(LinkedList* list, int index, int data) {
    if (index < 0 || index > list->size) {
        return false;  // Index out of range
    }

    if (index == 0) {
        add_first(list, data);
        return true;
    }

    if (index == list->size) {
        add_last(list, data);
        return true;
    }

    Node* new_node = create_node(data);
    if (new_node == NULL) return false;

    Node* current = list->head;
    for (int i = 0; i < index - 1; i++) {
        current = current->next;
    }

    new_node->next = current->next;
    current->next = new_node;
    list->size++;
    return true;
}

bool delete_first(LinkedList* list) {
    if (list->head == NULL) {
        return false;  // List is empty
    }

    Node* to_delete = list->head;
    list->head = list->head->next;
    free(to_delete);
    list->size--;
    return true;
}

bool delete_last(LinkedList* list) {
    if (list->head == NULL) {
        return false;  // List is empty
    }

    if (list->head->next == NULL) {
        free(list->head);
        list->head = NULL;
        list->size--;
        return true;
    }

    Node* current = list->head;
    while (current->next->next != NULL) {
        current = current->next;
    }

    free(current->next);
    current->next = NULL;
    list->size--;
    return true;
}

bool delete_node(LinkedList* list, int data) {
    if (list->head == NULL) {
        return false;  // List is empty
    }

    // If head node contains the data
    if (list->head->data == data) {
        Node* to_delete = list->head;
        list->head = list->head->next;
        free(to_delete);
        list->size--;
        return true;
    }

    // Search for the node
    Node* current = list->head;
    while (current->next != NULL) {
        if (current->next->data == data) {
            Node* to_delete = current->next;
            current->next = current->next->next;
            free(to_delete);
            list->size--;
            return true;
        }
        current = current->next;
    }

    return false;  // Data not found
}

void free_list(LinkedList* list) {
    Node* current = list->head;
    while (current != NULL) {
        Node* next = current->next;
        free(current);
        current = next;
    }
    list->head = NULL;
    list->size = 0;
}

void print_list(LinkedList* list) {
    if (list->head == NULL) {
        printf("[]\n");
        return;
    }

    printf("[");
    Node* current = list->head;
    while (current != NULL) {
        printf("%d", current->data);
        if (current->next != NULL) {
            printf(" -> ");
        }
        current = current->next;
    }
    printf("]\n");
}

int get_at(LinkedList* list, int index) {
    if (index < 0 || index >= list->size) {
        return -1;  // Invalid index (could use a more sophisticated error handling)
    }

    Node* current = list->head;
    for (int i = 0; i < index; i++) {
        current = current->next;
    }
    return current->data;
}

int main() {
    LinkedList list;
    list_init(&list);

    // Test adding elements
    printf("Adding elements:\n");
    add_first(&list, 1);
    add_last(&list, 2);
    add_last(&list, 3);
    add_first(&list, 0);

    printf("List: ");
    print_list(&list);

    // Verify order: 0 -> 1 -> 2 -> 3
    if (get_at(&list, 0) != 0 || get_at(&list, 1) != 1 ||
        get_at(&list, 2) != 2 || get_at(&list, 3) != 3) {
        printf("FAIL: Elements in wrong order\n");
        return 1;
    }
    printf("Order verified: 0 -> 1 -> 2 -> 3\n");

    // Test deletion
    printf("\nDeleting element 2:\n");
    delete_node(&list, 2);
    printf("List: ");
    print_list(&list);

    // Verify order: 0 -> 1 -> 3
    if (get_at(&list, 0) != 0 || get_at(&list, 1) != 1 || get_at(&list, 2) != 3) {
        printf("FAIL: Elements in wrong order after deletion\n");
        return 1;
    }
    printf("Order verified: 0 -> 1 -> 3\n");

    // Test delete first
    printf("\nDeleting first element:\n");
    delete_first(&list);
    printf("List: ");
    print_list(&list);

    if (get_at(&list, 0) != 1 || get_at(&list, 1) != 3) {
        printf("FAIL: Elements in wrong order after delete_first\n");
        return 1;
    }

    // Test delete last
    printf("\nDeleting last element:\n");
    delete_last(&list);
    printf("List: ");
    print_list(&list);

    if (get_at(&list, 0) != 1 || list.size != 1) {
        printf("FAIL: Unexpected state after delete_last\n");
        return 1;
    }

    // Test insert_at
    printf("\nInserting 5 at index 0:\n");
    insert_at(&list, 0, 5);
    printf("List: ");
    print_list(&list);

    printf("Inserting 10 at index 2:\n");
    insert_at(&list, 2, 10);
    printf("List: ");
    print_list(&list);

    // List should be: 5 -> 1 -> 10
    if (get_at(&list, 0) != 5 || get_at(&list, 1) != 1 || get_at(&list, 2) != 10) {
        printf("FAIL: Elements in wrong order after insert_at\n");
        return 1;
    }

    // Test size
    printf("\nList size: %d\n", list.size);
    if (list.size != 3) {
        printf("FAIL: Expected size 3, got %d\n", list.size);
        return 1;
    }

    // Test deleting non-existent element
    printf("\nTrying to delete non-existent element 999:\n");
    if (delete_node(&list, 999)) {
        printf("FAIL: Should not have found element 999\n");
        return 1;
    }
    printf("Correctly returned false for non-existent element\n");

    // Test free_list
    printf("\nFreeing list:\n");
    free_list(&list);
    printf("List: ");
    print_list(&list);

    if (list.size != 0 || list.head != NULL) {
        printf("FAIL: List not properly freed\n");
        return 1;
    }

    // Test operations on empty list
    printf("\nTesting operations on empty list:\n");
    if (delete_first(&list)) {
        printf("FAIL: delete_first should fail on empty list\n");
        return 1;
    }
    printf("delete_first correctly returned false on empty list\n");

    if (delete_last(&list)) {
        printf("FAIL: delete_last should fail on empty list\n");
        return 1;
    }
    printf("delete_last correctly returned false on empty list\n");

    printf("\nAll tests passed!\n");
    return 0;
}
