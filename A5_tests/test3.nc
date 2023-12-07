void func(int a, int *b);

void func(int a, int * b){
    
}
void main(){
    int a = 1;
    a = a + 1;
    int b = 1 - a;
    int c = 1/b;
    c = c * 2 + 10;
    int * d;
    *d = c * c % a;
}