#include <stdio.h>

union Int32 { 
    int x; 
    unsigned char bytes[4]; 
};

void distributionsort(int base, int c, union Int32 *nums, int n)
{
	int count[base];
	for(int i = 0; i < base; i++) count[i] = 0;

	for(int i = 0; i < n; i++)
	{
		int key = nums[i].bytes[c];
		if(c == 3) key ^= 128 ;
		count[key]++;
	}
	for(int i = 1; i < base; i++)
		count[i] += count[i - 1];
	union Int32 sorted_nums[n];

	for(int i = n - 1; i >= 0; i--)
	{
		int key = nums[i].bytes[c];
		if(c == 3) key ^= 128 ;
		int j = --count[key];

		sorted_nums[j] = nums[i];
	}

	for(int i = 0; i < n; i++) nums[i] = sorted_nums[i];
}

void radixsort(int base, int c, union Int32 *nums, int n)
{
	for(int i = 0; i < c; i++)
		distributionsort(base, i, nums, n);
}


int main()
{
	int n;
	scanf("%i", &n);
	union Int32 nums[n];
	for(int i = 0; i < n; i++) scanf("%i", &nums[i].x);
	radixsort(256, 4, nums, n);
	for(int i = 0; i < n; i++) printf("%i ", nums[i].x);
	printf("\n");
	return 0;
}