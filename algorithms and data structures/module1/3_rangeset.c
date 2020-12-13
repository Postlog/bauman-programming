#include <stdio.h>

int main()
{
	int n;
	long ls, rs, cls, crs;
	scanf("%i", &n);
	scanf("%li %li", &ls, &rs);
	
	for(int i = 0; i < n - 1; i++)
	{
		scanf("%li %li", &cls, &crs);
		if (cls > rs + 1) 
		{
			printf("%li %li\n", ls, rs);
			ls = cls;
			rs = crs;
		}
		else if (crs > rs) rs = crs;
	}
	printf("%li %li\n", ls, rs);
	return 0;
}
