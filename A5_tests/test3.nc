void func(int a, int *b);

void func(int a, int * b){
    
}
// added int retval to check return code
/*int*/void main(){
    int a = 1;
    a = a + 1;
    int b = 1 - a;
    int c = 1/b;
    c = c * 2 + 10;
    int * d;
    // this will necessarily segfault, d = 0x0 initially, and trying to write to that is never going to work :(
    // can be rectified by simply pointing d to sth

    // d = &c;

    *d = c * c % a;
    //return c;
}