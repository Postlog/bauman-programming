#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int max(int a, int b)
{
	return a > b ? a : b;
}

int query(int *T, int l, int r, int i, int a, int b)
{
	if(l == a && r == b)
		return T[i];
	else
	{
		int m = (a + b) / 2;
		if(r <= m)
			return query(T, l, r, 2*i + 1, a, m);
		else if(l > m)
			return query(T, l, r, 2*i + 2, m + 1, b);
		else
			return max(query(T, l, m, 2*i + 1, a, m), query(T, m + 1, r, 2*i + 2, m + 1, b));
	}
}

int segment_tree_query(int *T, int n, int l, int r)
{ 
	return query(T, l, r, 0, 0, n - 1);
}

void update(int j, int v, int i, int a, int b, int *T)
{
	if(a == b) T[i] = v;
	else
	{
		int m = (a + b) / 2;
		if(j <= m)
			update(j, v, 2*i + 1, a, m, T);
		else
			update(j, v, 2*i + 2, m + 1, b, T);
		T[i] = max(T[2*i + 1], T[2*i + 2]);
	}
}

void segment_tree_update(int j, int v, int n, int *T)
{
	update(j, v, 0, 0, n - 1, T);
}

void build(int *v, int i, int a, int b, int *T)
{
	if(a == b)
		T[i] = v[a];
	else
	{
		int m = (a + b) / 2;
		build(v, 2*i + 1, a, m, T);
		build(v, 2*i + 2, m + 1, b, T);
		T[i] = max(T[2*i + 1], T[2*i + 2]);
	}
}

void segment_tree_build(int *v, int n, int **pT)
{
	int *T = (int *)malloc(sizeof(int) * (4 * n));
	build(v, 0, 0, n - 1, T);
	*pT = T;
}

int main()
{
	int n;
	scanf("%i", &n);
	int nums[n];
	for(int i = 0; i < n; i++) scanf("%i", &nums[i]);

	int *T;
	segment_tree_build(nums, n, &T);

	int m;
	scanf("%i", &m);
	for(int i = 0; i < m; i++)
	{
		char action[3];
		int l, r;
		scanf("%s %i %i", action, &l, &r);
		if(strcmp("MAX", action) == 0)
			printf("%i\n", segment_tree_query(T, n, l, r));
		else
			segment_tree_update(l, r, n, T);

	}

	free(T);
	return 0;
}