



class Person {
    static int F;
    public:
        static int G;
        static int static_method() {
            return F;
        }
};


Person p;
decltype(p) q = p;

typedef Person* P;


P qq;

decltype(qq) x;

int some_f = decltype(p)::G;

int some_number = 5;
decltype(some_number) double_the_number = some_number * 2;
int a = decltype(p)::static_method();
