#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Node Node;

struct Node
{
    int value, balance;
    unsigned long key;
    Node *parent, *left, *right;
};

typedef struct Tree 
{
    Node *root;
} Tree;

Node *tree_find_node(Tree *tree, unsigned long key)
{
    Node *node = tree->root;

    while (node && node->key != key)
    {
        if (key < node->key) node = node->left;
        else node = node->right;
    }
    return node;
}

void tree_insert_node(Tree *tree, unsigned long key, int value)
{
    Node *new_node = (Node*) malloc(sizeof(Node));
    new_node->parent = NULL; new_node->left = NULL; new_node->right = NULL;
    new_node->key = key;
    new_node->value = value;
    new_node->balance = 0;

    if (!tree->root) 
    {
        tree->root = new_node;
        return;
    }
    Node *node = tree->root;
    for (;;)
    {
        if (key < node->key)
        {
            if (node->left == NULL)
            {
                node->left = new_node;
                new_node->parent = node;
                break;
            }
            node = node->left;
        }
        else
        {
            if (node->right == NULL)
            {
                node->right = new_node;
                new_node->parent = node;
                break;
            }
            node = node->right;
        }
    }
    
}

Tree *tree_replace_node(Tree *tree, Node *x, Node *y)
{
    if (x == tree->root)
    {
        tree->root = y;
        if (y != NULL) y->parent = NULL;
    }
    else
    {
        Node *p = x->parent;
        if (y != NULL) y->parent = p;
        if (p->left == x) p->left = y;
        else p->right = y;
    }
    return tree;
}

void tree_clear_node(Node *node)
{
    if(node)
    {
        if(node->left) tree_clear_node(node->left);
        if(node->right) tree_clear_node(node->right);  
        free(node);
    }
}

void tree_clear(Tree *tree)
{
    if(tree->root) 
        tree_clear_node(tree->root);
}

Node *avl_tree_rotate_left(Tree *tree, Node *x)
{
    Node *y = x->right;
    tree_replace_node(tree, x, y);
    Node *b = y->left;

    if(b) b->parent = x;
    x->right = b;
    x->parent = y;
    y->left = x;

    x->balance--;
    if(y->balance > 0)
        x->balance = x->balance - y->balance;

    y->balance--;
    if(x->balance < 0)
        y->balance = y->balance + x->balance;
}

Node *avl_tree_rotate_right(Tree *tree, Node *x)
{
    Node *y = x->left;
    tree_replace_node(tree, x, y);
    Node *b = y->right;

    if(b) b->parent = x;
    x->left = b;
    x->parent = y;
    y->right = x;

    x->balance++;
    if(y->balance < 0)
        x->balance = x->balance - y->balance;

    y->balance++;
    if(x->balance > 0)
        y->balance = y->balance + x->balance;
}

Node *avl_tree_insert_node(Tree *tree, unsigned long key, int value)
{
    tree_insert_node(tree, key, value);
    
    Node *a = tree->root;
    a->balance = 0;
    Node *x = a->parent;

    for (;;)
    {
        x = a->parent;
        if (!x) break;
        if (a == x->left)
        {
            x->balance--;
            if (x->balance == 0) break;
            if (x->balance == -2)
            {
                if (a->balance == 1) avl_tree_rotate_left(tree, a);
                avl_tree_rotate_right(tree, x);
                break;
            }
        }
        else
        {
            x->balance++;
            if (x->balance == 0) break;
            if (x->balance == 2)
            {
                if (a->balance == -1) avl_tree_rotate_right(tree, a);
                avl_tree_rotate_left(tree, x);
                break;
            }
        }
    }
    return a;
}

unsigned long hash(char *str)
{
    unsigned long hash = 5381;
    int c;

    while(c = *str++)
        hash = ((hash << 5) + hash) + c;

    return hash;
}

int SPEC_COUNT = 6;
char SPECS[] = {'+', '-', '*', '/', '(', ')'};

int get_spec_index(char c)
{
    for(int i = 0; i < SPEC_COUNT; i++)
        if(c == SPECS[i]) return i;
    return -1;
}

int is_number(char c)
{
    return c >= '0' && c <= '9';
}

int is_alphabetic(char c)
{
    return c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z';
}

void main()
{
    int n;
    scanf("%d\n", &n);

    char *str = (char *)malloc(sizeof(char) * (n + 1));
    char *str_ptr_copy = str;
    gets(str);

    Tree tree;
    tree.root = NULL;
    int current_ident_id = 0;

    while(*str)
    {
        while(*str == ' ') str++;

        int spec_index = get_spec_index(*str);
        if(spec_index != -1) 
        {
            printf("SPEC %d\n", spec_index);
            str++;
        }
        else if(is_number(*str))
        {

            char const_number[1024];
            int j = 0;
            while (get_spec_index(*str) == -1 && *str && *str != ' ')
                const_number[j++] = *(str++);
            const_number[j] = '\0';

            printf("CONST %s\n", const_number);
        }
        else if(is_alphabetic(*str))
        {
            char *word = (char *)malloc(sizeof(char) * 1024);
            int j = 0;
            while (get_spec_index(*str) == -1 && *str && *str != ' ')
                word[j++] = *(str++);

            word[j] = '\0';
            unsigned long word_hash = hash(word);
            int found = tree_find_node(&tree, word_hash) != NULL;
            if(!found)
                avl_tree_insert_node(&tree, word_hash, current_ident_id++);

            printf("IDENT %d\n", tree_find_node(&tree, word_hash)->value);

            if(found) free(word);
        }
    }

    tree_clear(&tree);
    free(str_ptr_copy);
}