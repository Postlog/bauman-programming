#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 1
#define FALSE 0

typedef struct Node Node;

struct Node
{
    int words_count, is_end;
    Node *parent, **children;
};

typedef struct Tree
{
    Node *root;
} Tree;

const int ALPHABET_LETTERS_COUNT = 26;

void build_node(Node *node)
{
    node->children = (Node **)malloc(sizeof(Node *) * ALPHABET_LETTERS_COUNT);

    for(int i = 0; i < ALPHABET_LETTERS_COUNT; i++)
        node->children[i] = NULL;

    node->parent = NULL;
    node->words_count = 0;
    node->is_end = FALSE;
}

void tree_clear_node(Node *node)
{
    if(node)
    {
        for (int i = 0; i < ALPHABET_LETTERS_COUNT; i++)
            tree_clear_node(node->children[i]);
        free(node->children);
        free(node);
    }
}

void tree_clear(Tree *tree)
{
    tree_clear_node(tree->root);
}

void tree_insert_word(Tree *tree, char *str)
{
    Node *node = tree->root;

    int len = strlen(str);
    for(int i = 0; i < len; i++)
    {
        int idx = str[i] - 'a';

        if(!node->children[idx])
        {
            Node *new_node = (Node *)malloc(sizeof(Node));
            build_node(new_node);
            new_node->parent = node;
            node->children[idx] = new_node;
        }

        node = node->children[idx];
        node->words_count++;
    }
    if(node->is_end) 
        for(; node; node = node->parent) node->words_count--;
    else node->is_end = TRUE;
}

void tree_delete_word(Tree *tree, char *str)
{
    Node *node = tree->root;
    
    int len = strlen(str);
    for(int i = 0; i < len; i++)
    {
        int idx = str[i] - 'a';

        node = node->children[idx];
        node->words_count--;

        if (node->words_count == 0)
        {
            node->parent->children[idx] = NULL;
            tree_clear_node(node);
            break;
        }
    }
}

int tree_prefix_words_count(Tree *tree, char *prefix)
{
    Node *node = tree->root;
    int words_count = 0, len = strlen(prefix);
    for(int i = 0; i < len && node; i++)
    {
        int idx = prefix[i] - 'a';
        node = node->children[idx];
    }
    if(node) words_count = node->words_count;
    return words_count;
}

int ACTIONS_COUNT = 3;
char *ACTIONS[] = {"INSERT", "DELETE", "PREFIX"};

int action_index(char *action)
{
    for(int i = 0; i < ACTIONS_COUNT; i++)
        if(strcmp(action, ACTIONS[i]) == 0) return i;
    return -1;
}

int main() {
    Tree tree;
    Node *node = (Node *)malloc(sizeof(Node));
    build_node(node);
    tree.root = node;
    
    int n;
    scanf("%d", &n);

    for(int i = 0; i < n; i++)
    {
        char command[7];
        scanf("%s", command);

        int index = action_index(command);
        char str[100000];
        scanf("%s", str);
        switch(index)
        {
            case 0:
                tree_insert_word(&tree, str);
                break;
            case 1:
                tree_delete_word(&tree, str);
                break;
            case 2:
                printf("%d\n", tree_prefix_words_count(&tree, str));
                break;
        }
    }

    tree_clear(&tree);
}