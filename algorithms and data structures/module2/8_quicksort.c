#include <stdio.h>

void selectsort(int *a, int low, int high)
{
	for(int j = high; j > low; j--)
	{
		int max_idx = j;

		for(int i = j - 1; i >= low; i--)
			if(a[i] > a[max_idx]) max_idx = i;

		int temp = a[j];
		a[j] = a[max_idx];
		a[max_idx] = temp;
	}
}

int partition(int *a, int low, int high)
{
	int i = low, j = low;

	for(; j < high; j++)
	{
		if(a[j] < a[high])
		{
			int temp = a[i];
			a[i] = a[j];
			a[j] = temp;

			i++;
		}
	}

	int temp = a[i];
	a[i] = a[high];
	a[high] = temp;

	return i;
}

void quicksort_rec(int *a, int low, int high, int m)
{
	if(high - low + 1 < m) selectsort(a, low, high);
	else
	{
		while(low < high)
		{
			int q = partition(a, low, high);
			if(q - low < high - q)
			{
				quicksort_rec(a, low, q - 1, m);
				low = q + 1;
			}
			else
			{
				quicksort_rec(a, q + 1, high, m);
				high = q - 1;
			}
		}
	}
}

void quicksort(int *a, int n, int m)
{
	quicksort_rec(a, 0, n - 1, m);
}

int main()
{
	int n, m;
	scanf("%i %i", &n, &m);
	int a[n];
	for(int i = 0; i < n; i++) scanf("%i", &a[i]);
	quicksort(a, n, m);
	for(int i = 0; i < n; i++) printf("%i ", a[i]);
	printf("\n");
	return 0;
}