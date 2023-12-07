// int d = 38;
// int c = d - 14;
// 
// int main() {
// 	int c = 5 + d;
// 	int k;
// 
// 	k = c / 5;
// 	int j = c % 5;
// 
// 	return 2 * k + j;
// }

// int main() {
// 	if (0 == 0) return 0; else return 1;
// }

// had to change function name to make this program valid

// int fun(int n){
//     if(n == 0) return n;
//     return fun(n-1) + n;
// }
// 
// int main(){
//     int sum = 10;
// 	int n = sum;
//     sum = fun(sum);
// 
//     return sum == n * (n + 1) / 2 ? 0 : 1;
// }

// Find fibonacci by co-recursion
// int f_odd(int n);
// int f_even(int n);
// 
// int fibonacci(int n) {
// 	return (n % 2 == 0)? f_even(n): f_odd(n);
// }
// int f_odd(int n) {
// 	return (n == 1)? 1: f_even(n-1) + f_odd(n-2);
// }
// int f_even(int n) {
// 	return (n == 0)? 0: f_odd(n-1) + f_even(n-2);
// }
// int main() {
// 	int n = 6;
// 	int r;
// 	r = fibonacci(n);
// 	return r;
// }

// int main() {
//     int a = 5;
//     int *num;
//     int *num2 = 10;
//     num2 = num2 + 1;
// 
//     a = -a;
// 
//     if (a != -5) return 1;
// 
//     a = -a;
// 
//     if (a != 5) return 1;
// 
//     a = a;
// 
//     if (a != 5) return 1;
// 
//     int b = num;
// 
//     num = &a;
//     a = *num;
// 
//     if (a != 5) return 1;
//     if (*num != 5) return 1;
// 
//     *num = 10;
//     return a == 10 ? 0 : 1;
// }