#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void tree_build(int *v, int n, int **pT)
{
    int *T = (int *)malloc(sizeof(int) * n * 2);
    for(int i = 0; i < n; i++)
        T[n + i] = v[i];
    
    for(int i = n - 1; i > 0; i--)
        T[i] = T[i * 2] ^ T[i * 2 + 1];

    *pT = T;
}

int tree_satisfy(int *tree, int n, int a, int b)
{
    int sum = 0;
    if(a == b) sum = tree[a + n];
    else
    {
        a += n;
        b += n;

        while(a < b)
        {
            if(a % 2 != 0 && a + 1 < b)
                sum ^= tree[a++];            
            if(b % 2 == 0 && a < b - 1)
                sum ^= tree[b--];     

            if(b - a <= 1)
            {
                sum ^= tree[b] ^ tree[a];
                break;
            }

            a /= 2;
            b /= 2;
        }  
    }

    int pos_bits_count = 0;
    for(int i = 0; i < 'z' - 'a'; i++)
        pos_bits_count += (sum >> i) % 2;

    return pos_bits_count <= 1;
}

void tree_update(int *tree, int n, int a, char *substr)
{
    int len = strlen(substr);
    for(int i = 0; i < len; i++)
    {
        int val = (1 << substr[i] - 'a'),
            a_ = a + i + n;

        tree[a_] = val;
        while(a_ / 2 > 0)
        {
            a_ /= 2;
            tree[a_] = tree[a_ * 2] ^ tree[a_ * 2 + 1];
        }
    }
}

void main()
{
    char str[1000000];
    scanf("%s", str);

    int len = strlen(str),
        power = 1;

    while(power < len) power *= 2;

    int *v = (int *)malloc(sizeof(int) * power);

    for(int i = 0; i < power; i++)
    {
        v[i] = 0;
        if(i < len) v[i] = (1 << str[i] - 'a');
    }


    int *T;
    tree_build(v, power, &T);

    int m;
    scanf("%d", &m);

    for(int i = 0; i < m; i++)
    {
        char action[4];
        scanf("%s", action);
        
        if(strcmp(action, "HD") == 0)
        {
            int a, b;
            scanf("%d %d", &a, &b);
            printf("%s\n", tree_satisfy(T, power, a, b) ? "YES" : "NO");
        }
        else
        {
            int a;
            scanf("%d", &a);
            char s[1000000];
            scanf("%s", s);
            tree_update(T, power, a, s);
        }
    }
    free(v);
    free(T);
}