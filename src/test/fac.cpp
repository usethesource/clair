
int fac(int n) {
    if (n <= 0) {
		return 1;
	}
	else {
		return fac(n-1) * n;
	}
}

int main(char* argv[]) {
  return fac(42);
}
