#include <stdio.h>

int abs(int a)
{
	return a < 0 ? -a : a;
}

void insertionsort(int *a, int low, int high)
{
	for(int i = low + 1; i <= high; i++)
	{
		int key = a[i],
			loc = i - 1;

		while(loc >= low && abs(a[loc]) > abs(key))
		{
			a[loc + 1] = a[loc];
			loc--;
		}
		a[loc + 1] = key;
	}
}

void merge(int *a, int low, int mid, int high)
{
	int sorted[high - low + 1],
		i = low, j = mid + 1, c = 0;

	while(c < high - low + 1)
	{
		if(j <= high && (i == mid + 1 || abs(a[j]) < abs(a[i])))
		{
			sorted[c] = a[j];
			j++;
		}
		else
		{
			sorted[c] = a[i];
			i++;
		}
		c++;
	}

	for(int i = low; i <= high; i++) a[i] = sorted[i];
}

void mergesort_rec(int *a, int low, int high)
{
	if(high - low + 1 < 5) insertionsort(a, low, high);
	else 
	{
		int mid = (low + high) / 2;
		mergesort_rec(a, low, mid);
		mergesort_rec(a, mid + 1, high);
		merge(a, low, mid, high);
	}
}

void mergesort(int *a, int n)
{
	mergesort_rec(a, 0, n - 1);
}

int main()
{
	int n;
	scanf("%i", &n);
	int a[n];
	for(int i = 0; i < n; i++) scanf("%i", &a[i]);
	mergesort(a, n);
	for(int i = 0; i < n; i++) printf("%i ", a[i]);
	printf("\n");
	return 0;
}