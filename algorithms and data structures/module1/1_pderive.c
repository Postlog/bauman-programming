#include <stdio.h>

int main()
{
	int n, k;
	long x0, c, res;
	scanf("%i %i %li", &n, &k, &x0);

	for(int i = 0; i < n - k + 1; i++)
	{
		scanf("%li", &c);
		int _k = k, _n = n - i;
		while(_k--)
			c *= _n--;
		
		res = !i ? c : res * x0 + c;
	}


	printf("%li\n", res);
	return 0;
}