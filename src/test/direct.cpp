
int main() {
	int *a = new int[10];

	for (int i = 0; i < 10; i++) {
    	  a[i++] = i;
    }

	return a[3];
}
