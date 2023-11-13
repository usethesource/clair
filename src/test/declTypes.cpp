



class Person {
    static int F;
    int hello() {
        return F;
    }
};

Person p;
decltype(p) q = p;

typedef Person* P;


P qq;

decltype(qq) x;




