#include <stdio.h>

int main()
{
	int n, k, idx = 0;
	scanf("%i %i", &n, &k);
	long b[k], sum = 0, maxsum = 0;

	for(int i = 0; i < k; i++) 
	{
		scanf("%li", &b[i]);
		sum += b[i];
	}

	maxsum = sum;

	for(int i = k; i < n; i++)
	{
		sum -= b[i % k];
		scanf("%li", &b[i % k]);
		sum += b[i % k];

		if(sum > maxsum)
		{
			maxsum = sum;
			idx = i - k + 1;
		}
	}

	printf("%i\n", idx);
	return 0;
}