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

typedef struct HashTable
{
	Tree *tree_table;
	int size;
} HashTable;

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

void tree_insert_node(Tree *tree, int key, int value)
{
	Node *new_node = (Node *)malloc(sizeof(Node));
	new_node->key = key;
	new_node->value = value;
	new_node->parent = NULL; new_node->left = NULL; new_node->right = NULL;
	if(!tree->root)
	{
		tree->root = new_node;
		return;
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
}

void tree_clear_node(Node *node)
{
	if(node->left) tree_clear_node(node->left);
	if(node->right) tree_clear_node(node->right);
	free(node);
}

void tree_clear(Tree tree)
{
	if(tree.root)
		tree_clear_node(tree.root);
}

void hash_table_init(HashTable *t, int size)
{
	t->size = size;
	t->tree_table = (Tree *)malloc(sizeof(Tree) * size);
	for(int i = 0; i < size; i++) 
	{
		Tree tree;
		tree.root = NULL;
		t->tree_table[i] = tree;
	}
}

void hash_table_clear(HashTable t)
{
	for(int i = 0; i < t.size; i++) tree_clear(t.tree_table[i]);
	free(t.tree_table);
}

int at(HashTable *t, int key)
{
	int hash = key % t->size;
	Tree *tree = &t->tree_table[hash];
	Node *node = tree_find_node(tree, key);
	if(!node) return 0;
	else return node->value;
}

void assign(HashTable *t, int key, int value)
{
	int hash = key % t->size;
	Tree *tree = &t->tree_table[hash];
	Node *node = tree_find_node(tree, key);
	if(node) node->value = value;
    else tree_insert_node(tree, key, value);
}

void main() {
    int n, m;
    scanf("%d %d", &n, &m);
    HashTable table;
    hash_table_init(&table, m);

    for (int i = 0; i < n; i++)
    {
        char action[7];
        scanf("%s", action);

        if(!strcmp(action, "AT"))
        {
            int key;
            scanf("%d", &key);
            printf("%d\n", at(&table, key));
        }
        else if(!strcmp(action, "ASSIGN"))
        {
            int key, val;
            scanf("%d %d", &key, &val);
            assign(&table, key, val);
        }
    }

    hash_table_clear(table);
}