#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Node Node;

struct Node
{
    int key, value;
    Node *parent, *left, *right;
};

typedef struct Tree
{
    Node *root;
} Tree;

Node *tree_find_node(Tree *tree, int key)
{
    Node *node  = tree->root;
    while(node && node->key != key)
    {
        if(key < node->key) node = node->left;
        else node = node->right;
    }
    return node;
}

Node *tree_insert(Tree *tree, int key, int value)
{
    Node *new_node = (Node *)malloc(sizeof(Node));
    new_node->key = key;
    new_node->value = value;
    new_node->parent = NULL; new_node->left = NULL; new_node->right = NULL;
    if(!tree->root)
    {
        tree->root = new_node;
        return tree->root;
    }

    Node *node = tree->root;
    for(;;)
    {
        if(key < node->key)
        {
            if(!node->left) 
            {
                node->left = new_node;
                new_node->parent = node;
                break;
            }
            node = node->left;
        }
        else 
        {
            if(!node->right)
            {
                node->right = new_node;
                new_node->parent = node;
                break;
            }
            node = node->right;
        }

    }

    return new_node;
}

void clear_subtree(Node *node)
{
    if(node->left) clear_subtree(node->left);
    if(node->right) clear_subtree(node->right);
    free(node);
}

void clear_tree(Tree tree)
{
    if(tree.root)
        clear_subtree(tree.root);
}

void main()
{
    int n;
    scanf("%d", &n);

    Tree tree;
    tree.root = NULL;

    int count = 0, sum = 0;

    for(int i = 0; i < n; i++)
    {
        int x;
        scanf("%d", &x);        
        sum ^= x;

        Node *node = tree_find_node(&tree, sum);
        if(!node) node = tree_insert(&tree, sum, 0);
        node->value++;

        if(sum == 0) count += node->value; 
        else count += node->value - 1;
    }

    printf("%d\n", count);
    
    clear_tree(tree);
}