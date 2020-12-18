#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int peak_count(int *v, int n, int a, int b)
{
	if (n == 1) return 1;
	int c = 0;

	if(a == 0 && v[0] >= v[1]) a = ++c; // <=> { a = 1; c++; }
	if(b == n - 1 && v[n - 1] >= v[n - 2]) b -= c++ ? 1 : 1; // <=> { b = n - 2; c++; }

	for(int i = a; i <= b; i++)
		c += v[i] >= v[i - 1] && v[i] >= v[i + 1]; // <=> if (...) c++;

	return c;
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
			return query(T, l, m, 2*i + 1, a, m) + query(T, m + 1, r, 2*i + 2, m + 1, b);
	}
}

int segment_tree_query(int *T, int n, int l, int r)
{ 
	return query(T, l, r, 0, 0, n - 1);
}

void update(int j, int i, int a, int b, int *v, int n, int *T)
{
	if(a == b) T[i] = peak_count(v, n, a, b);
	else
	{
		int m = (a + b) / 2;
		if(j <= m)
			update(j, 2 * i + 1, a, m, v, n, T);
		else
			update(j, 2 * i + 2, m + 1, b, v, n, T);
		T[i] = T[2*i + 1] + T[2*i + 2];
	}
}

void segment_tree_update(int j, int val, int *v, int n, int *T)
{
	v[j] = val;
	update(j, 0, 0, n - 1, v, n, T);
	if(j != 0) update(j - 1, 0, 0, n - 1, v, n, T);
	if(j != n - 1) update(j + 1, 0, 0, n - 1, v, n, T);
}

void build(int *v, int n, int i, int a, int b, int *T)
{
	if(a == b)
		T[i] = peak_count(v, n, a, b);
	else
	{
		int m = (a + b) / 2;
		build(v, n, 2*i + 1, a, m, T);
		build(v, n, 2*i + 2, m + 1, b, T);
		T[i] = T[2*i + 1] + T[2*i + 2];
	}
}

void segment_tree_build(int *v, int n, int **pT)
{
	int *T = (int *)malloc(sizeof(int) * (4 * n));
	build(v, n, 0, 0, n - 1, T);
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
		char action[4];
		int l, r;
		scanf("%s %i %i", action, &l, &r);
		if(strcmp("PEAK", action) == 0)
			printf("%i\n", segment_tree_query(T, n, l, r));
		else
			segment_tree_update(l, r, nums, n, T);
	}
	free(T);
	return 0;
}