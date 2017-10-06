
int main() {
	int *a = new int[10];

	for (int i = 0; i < 10; i++) {
      int *j = &i;
    	  a[i++] = *j;
    }

	return a[3];
}
