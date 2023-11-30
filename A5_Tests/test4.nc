int sum(int n){
    if(n == 0) return n;
    return sum(n-1) + n;
}

int main(){
    int sum = 10;
    sum = fun(sum);
    return 0;
}