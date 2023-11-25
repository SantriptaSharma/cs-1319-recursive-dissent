int GLOB = 1;
char POPPING_thing = 'c';

void printf(char *c, int argv);

int main(int argc, char *argv) {
	int i;
	i = GLOB * 5;

	for (i = 0; i < argc; i = i + 1)
	{
		printf("%d, ", argv);
	}
}