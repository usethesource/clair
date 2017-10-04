
int x = 1;

class C;

int main() {
	int* x = malloc(sizeof(int));
    *x = 1;
    **y = 2;
    free(x);
}
