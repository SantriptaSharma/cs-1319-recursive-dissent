int d = 99;

// had to move fun2 up, since fun uses it, and declaration must occur before 
// usage
int * fun2(int b){
    int * e = 0;
    return e;
}

int fun(int c){
    if(c%d == 0) return fun2(c);
    else return fun(c+1);
}

int main(){
    int a = 1;
    int * ret = fun(a);
    return 0;
}