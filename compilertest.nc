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

int fun(int n){
    if(n == 0) return n;
    return fun(n-1) + n;
}

int main(){
    int sum = 10;
	int n = sum;
    sum = fun(sum);

    return sum == n * (n + 1) / 2 ? 0 : 1;
}