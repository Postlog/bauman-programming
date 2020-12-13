#include <stdio.h>
#include <stdlib.h>

int main()
{
	int k, n;
	scanf("%i %i", &k, &n);
	
	int *pds = (int *)malloc(sizeof(int) * (n + 1));
	for(int i = 0; i < n + 1; i++) pds[i] = 1;

	for(int i = 2; i <= n; i++)
	{
		if (pds[i] != 1) continue;
		for(int j = 2; i * j <= n; j++)
		{
			pds[j * i] = pds[j] + 1;
		}
	}

	for(int i = 2; i <= n; i++)
		if(pds[i] == k) printf("%i ", i);
	printf("\n"); 
	free(pds);
	return 0;
}