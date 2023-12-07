int fun(int n){
    if(n == 0) return n;
    // had to rename to fun
    return fun(n-1) + n;
}

int main(){
    int sum = 10;
    sum = fun(sum);
    return 0;
}