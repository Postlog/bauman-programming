#include <stdio.h>

int power2(int a)
{
    return a <= 0 ? 0 : !(a & (a - 1));
}

int power2_count_rec(int *a, int n, int offs, int it_sum)
{
    it_sum += a[offs];
    int c = power2(it_sum);
    for (int j = offs + 1; j < n; j++)
        c += power2_count_rec(a, n, j, it_sum);
    return c;
}

int power2_count(int *a, int n)
{
    int c = 0;
    for(int i = 0; i < n; i++)
        c += power2_count_rec(a, n, i, 0);
    return c;
}

int main()
{
    int n;
    scanf("%d", &n);
    int nums[n];
    for(int i = 0; i < n; i++) scanf("%d", &nums[i]);
    
    printf("%d\n", power2_count(nums, n));
    return 0;
}