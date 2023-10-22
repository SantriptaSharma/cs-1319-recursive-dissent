// Add two numbers from input
int main() {
int x;
int y;
int z;
readInt(&x);
readInt(&y);
z = x + y;
printInt(x);
printStr("+");
printInt(y);
printStr(" = ");
printInt(z);
return 0;
}