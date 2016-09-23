//typedef char foobar;

int var;

void main() {

	int i;
	extern int j=2;

//	char* c;

	foo();

	i = (aux(4));

	/*i = (i>0?2:-2);

	do{
		i--;
	} while (i>0);
	int x = 42;

	switch (x) {
	case 1:
		break;
	case 42:
		x = 1;
		break;
	default:
		x = 0;
	}


	/*int x = 42;
	int y;
	if (x>4)
		y=1;
	else
		y=0;


	int x;
	if (true)
		x = 3 + 4;
	else
		x = 107%3;

	int var = 0;
	for (int i=0;i<4;i++)
		var+=1;

	char foo = 0;
	var = (char) foo;

	foobar ch = 57;*/
}

int aux(char c) {
	;
	return c;
}

void foo() {
	var = 'c';
}
