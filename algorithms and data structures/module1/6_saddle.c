#include <stdio.h>

int main()
{
	int n, m, rmax, a;
	scanf("%i %i", &n, &m);
	int rmaxidx[n], cmin[m], cminidx[m];

	for(int i = 0; i < n; i++)
	{
		for(int j = 0; j < m; j++)
		{
			scanf("%i", &a);
			if(j == 0)
			{
				rmax = a;
				rmaxidx[i] = 0;
			}
			else
			{
				if(a > rmax)
				{
					rmax = a;
					rmaxidx[i] = j;
				}
				else if(a == rmax) rmaxidx[i] = -1;
			}

			if(i == 0)
			{
				cmin[j] = a;
				cminidx[j] = 0;
			}
			else
			{
				if(a < cmin[j])
				{
					cmin[j] = a;
					cminidx[j] = i;
				}
				else if(a == cmin[j]) cminidx[j] = -1;
			}
		}
	}

	for(int i = 0; i < n; i++)
	{
		if(rmaxidx[i] != -1 && cminidx[rmaxidx[i]] == i)
		{
			printf("%i %i\n", i, rmaxidx[i]);
			break;
		}

		if(i == n - 1) printf("none\n");
	}

	return 0;
}