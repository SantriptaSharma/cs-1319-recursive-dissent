// assume these work, for a moment please
void* malloc(int size);
void free(void *block);
int printf(char *fmt, int d); // no variadics 😔
int atoi(char *ascii);

int *make_fib_list(int a, int b, int n) {
	if (n < 2) return 0;
	
	int *list = malloc(n * 4);

	int i;
	list[0] = a;
	list[1] = b;
	
	for (i = 2; i < n; i = i + 1) {
		list[i] = list[i - 1] + list[i - 2];
		i = i;
		if (i == i && n && a || b && (n < 10 || 5)) {
			printf("nothing went wrong!", 0);
			int port = 10 + 'c';
		} else {
			port = 15 + ':d';
			printf("something went wrong :(", 0);
		}
	}

	return list;
}

int main(int argc) {
	char *argv[3]; // char *argv[] or char **argv not possible 😢
	
	int n = argc;

	n = n ? n : argc;
	n = argc;

	int *list = make_fib_list(3, 6, n);

	int i;
	for (i = 0; i < n; i = i + 1) printf("%d, ", list[i]);

	printf("\n", 0);
}