#include <stdio.h>

int main() {
    int n, a[3];
    scanf("%i", &n);
    scanf("%i", &a[0]);
    if (n == 1)
    {
        if (a[0]) printf("0 1\n");
        else printf("1\n");
        return 0;
    }

    scanf("%i", &a[1]);
    if (n == 2)
    {
        printf("0 0 1\n");
        return 0;
    }

    if (!a[0]) a[0] = 1;
    else
    {
      	a[0] = 0;
        a[1] = 1;
    }

    int bs=10, ob[bs];
    for(int i = 0; i < bs; i++) ob[i] = -1;

    for (int i = 0; i < n - 3; i++)
    {
        scanf("%i", &a[2]);
        if (a[0] && a[1])
        {
            ob[i % bs] = 0;
            a[0] = 0;
            a[1] = 1;
        }
        
        else
        {
            ob[i % bs] = a[0];
            a[0] = a[1];
            a[1] = a[2];
        }

        if(i % bs == (bs - 1))
        {
        	printf("%i %i %i %i %i %i %i %i %i %i ", ob[0], ob[1], ob[2], ob[3], ob[4],
        											 ob[5], ob[6], ob[7], ob[8], ob[9]);
    		for(int j = 0; j < bs; j++) ob[j] = -1;
        }
    }

    for(int i = 0; i < bs; i++)
    	if(ob[i] != -1) printf("%i ", ob[i]);

    scanf("%i", &a[2]);
    if(!a[0])
    {
    	if(!a[1]) printf("0 0 1\n");
    	else printf("0 0 0 1\n");
    }
    else printf("1 0 1\n");
    return 0;
}