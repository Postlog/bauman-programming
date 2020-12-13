#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Date
{
	int Year, Month, Day;
};

int IDX;

int key(struct Date date)
{
	int keys[] = {date.Day - 1, date.Month - 1, date.Year - 1970};
	return keys[IDX];
}


void distributionsort(int base, struct Date *dates, int n)
{
	int count[base];

	for(int i = 0; i < base; i++) count[i] = 0;
	
	for(int i = 0; i < n; i++)
	{
		int k = key(dates[i]);
		count[k]++;
	}
	for(int i = 1; i < base; i++)
		count[i] += count[i - 1];

	struct Date sorted_dates[n];

	for(int i = n - 1; i >= 0; i--)
	{
		int k = key(dates[i]);

		int j = --count[k];
		sorted_dates[j] = dates[i];
	}

	for(int i = 0; i < n; i++) dates[i] = sorted_dates[i];
}

void radixsort(struct Date *dates, int n)
{
	int bases[] = {31, 12, 61};
	for(int i = 0; i < 3; i++)
	{
		IDX = i;
		distributionsort(bases[i], dates, n);
	}
}


int main()
{
	int n;
	scanf("%i", &n);
	struct Date dates[n];
	for(int i = 0; i < n; i++)
	{
		scanf("%d %d %d", &dates[i].Year, &dates[i].Month, &dates[i].Day);
	}

	radixsort(dates, n);

	for(int i = 0; i < n; i++)
	{
		printf("%d %d %d\n", dates[i].Year, dates[i].Month, dates[i].Day);
	}
	return 0;
}