#include <stdio.h>
#include <stdlib.h>

int main()
{
	int n1, n2, *s1, *s2, p1 = 0, p2 = 0;
	
	scanf("%i", &n1);
	s1 = (int *)malloc(sizeof(int) * n1);
	for (int i = 0; i < n1; i++)
		scanf("%i", &s1[i]);

	scanf("%i", &n2);
	s2 = (int *)malloc(sizeof(int) * n2);
	for (int i = 0; i < n2; i++)
		scanf("%i", &s2[i]);

	while (p1 < n1 && p2 < n2)
	{
		if (s1[p1] < s2[p2])
			printf("%i ", s1[p1++]);
		else
			printf("%i ", s2[p2++]);
	}

	if (p1 < n1)
		for (; p1 < n1; p1++) printf("%i ", s1[p1]);

	if (p2 < n2)
		for (; p2 < n2; p2++) printf("%i ", s2[p2]);
	
	printf("\n");
	free(s1);
	free(s2);
	return 0;
}