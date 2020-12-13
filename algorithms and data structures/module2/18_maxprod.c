#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_DENOM 1000000
#define MAX_DIGITS_COUNT 7
#define EPS 0.00001

int main()
{
	int n;
	scanf("%d \n", &n);
	
	int l = 0, r = 0, start = 0;
	float maxsum = -MAX_DENOM, sum = 0;
	for(int i = 0; i < n; i++)
	{
		char snum[MAX_DIGITS_COUNT];

		int j = 0;
		scanf("%c", &snum[j]);
		while(snum[j] != '/') scanf("%c", &snum[++j]);
		snum[j] = '\0';
		int num = atoi(snum);

		j = 0;
		scanf("%c", &snum[j]);
		while(snum[j] != ' ' && snum[j] != '\n') scanf("%c", &snum[++j]);
		snum[j] = '\0';
		int denom = atoi(snum);


		float number;
		if(num == 0)
			number = -MAX_DENOM;
		else
			number = log(num) - log(denom);

		
		if(sum < 0)
		{
			sum = 0;
			start = i;
		}

		sum += number;
		if(sum - maxsum > EPS)
		{
			maxsum = sum;
			l = start;
			r = i;
		}
	}

	printf("%d %d\n", l, r);
	
	return 0;
}