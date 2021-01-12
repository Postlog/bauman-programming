#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Node Node;

struct Node
{
	int key, count;
	char *value;
	Node *parent, *left, *right;
};

typedef struct Tree
{
	Node *root;
} Tree;


Node *tree_find_node(Tree *tree, int key)
{
	Node *node = tree->root;
	while(node && node->key != key)
	{
		if(key < node->key) node = node->left;
		else node = node->right;
	}
	return node;
}

void tree_insert_node(Tree *tree, int key, char *value)
{
	Node *new_node = (Node *)malloc(sizeof(Node));
	new_node->key = key;
	new_node->value = value;
	new_node->count = 0;
	new_node->parent = NULL; new_node->left = NULL; new_node->right = NULL;

	if(!tree->root)
	{
		tree->root = new_node;
		return;
	}

	Node *node = tree->root;
	for(;;)
	{
		node->count++;
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

void tree_replace_node(Tree *tree, Node *x, Node *y)
{
	if(tree->root == x)
	{
		tree->root = y;
		if(y) y->parent = NULL;
	}
	else
	{
		Node *parent = x->parent;
		if(y) y->parent = parent;

		if(parent->left == x) parent->left = y;
		else parent->right = y;
	}
}

Node *minimum(Node *node)
{
	if(!node) return NULL;
	else
	{
		while(node->left)
		{
			node->count--;
			node = node->left;
		}
		return node;
	}
}

Node *succ(Node *node)
{
	if(node->right)
	{
		return minimum(node->right);
	}
	else
	{
		Node *node_ = node->parent;
		while(node_ && node == node_->right)
		{
			node = node_;
			node_ = node_->parent;
		}
		return node_;
	}
}

void tree_increase_branch_count(Tree *tree, int key)
{
	Node *node = tree->root;
	while(node && node->key != key)
	{
		node->count--;
		if(key < node->key) node = node->left;
		else node = node->right;
	}
}

void tree_clear_node(Node *node)
{
	if(node->right)
		tree_clear_node(node->right);
	if(node->left)
		tree_clear_node(node->left);
	free(node->value);
	free(node);
}

void tree_clear(Tree tree)
{
	if(tree.root)
		tree_clear_node(tree.root);
}

void tree_delete_node(Tree *tree, int key)
{
	Node *node = tree_find_node(tree, key);
	tree_increase_branch_count(tree, key);

	if(!node->left && !node->right) tree_replace_node(tree, node, NULL);
	else if(!node->left) tree_replace_node(tree, node, node->right);
	else if(!node->right) tree_replace_node(tree, node, node->left);
	else
	{
		Node *y = succ(node);
		tree_replace_node(tree, y, y->right);
		node->left->parent = y;
		y->left = node->left;
		if(node->right) node->right->parent = y;
		y->right = node->right;
		y->count = node->count - 1;
		tree_replace_node(tree, node, y);
	}
	free(node->value);
	free(node);	
}

Node *tree_search_node_by_rank(Tree *tree, int rank)
{
	Node *node = tree->root;
	while(node)
	{
		if(node->left)
		{
		    if(node->left->count == rank - 1) return node;
			else if(node->left->count > rank - 1) node = node->left;
			else
			{
                rank -= node->left->count + 2;
				node = node->right;
			}
		}
		else
		{
			if(rank)
			{
				rank--;
				node = node->right;
			}
			else return node;
		}
	}
}

int ACTIONS_COUNT = 4;
char *ACTIONS[] = {"INSERT", "tree_search_node_by_rank", "LOOKUP", "tree_delete_node"};

int action_index(char *action)
{
	for(int i = 0; i < ACTIONS_COUNT; i++)
		if(strcmp(action, ACTIONS[i]) == 0) return i;
	return -1;
}

void main()
{
	int n;
	scanf("%d", &n);
	Tree tree;
	tree.root = NULL;

	for(int i = 0; i < n; i++)
	{
		char action[7], *word;
		scanf("%s", action);
		int index = action_index(action), x;
		switch(index)
		{
			case 0:
				scanf("%d", &x);
				word = (char *)malloc(sizeof(char) * 10);
				scanf("%s", word);
				tree_insert_node(&tree, x, word);
				break;
			case 1:
				scanf("%d", &x);
				printf("%s\n", tree_search_node_by_rank(&tree, x)->value);
				break;
			case 2:
				scanf("%d", &x);
				printf("%s\n", tree_find_node(&tree, x)->value);
				break;
			case 3:
				scanf("%d", &x);
				tree_delete_node(&tree, x);
				break;
		}
	}

	tree_clear(tree);
}