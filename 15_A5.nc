void readInt(int *d);
void printInt(int n);
void printStr(char *str);
int main();

int global_masked = 10;

// Find factorial by iteration and more stuff
int main() {
int global_masked = 5;
int n;
/* garbage 

comment */
int i = 0;
int r = 1;
readInt(&n);
for(i = 1; i <= n; i = i + 1)
r = r * i;
printInt(n);
printStr("! = ");
printInt(*&r);
return 0;

// sidyaj was here
}