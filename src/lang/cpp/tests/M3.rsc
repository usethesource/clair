module lang::cpp::tests::M3

import lang::cpp::M3;
import Set;
import Relation;
import analysis::graphs::Graph;

test bool modelConsistencyAddressBook() {
    tm = createM3AndAstFromCppFile(|project://clair/src/test/phonebook.cpp|);
    m = tm<0>;
    t = tm<1>;
    decls = m.declarations<name>;

    // nothing that is contained here does not not have a declaration, except the outermost translationUnit
    assert m.declarations<name> - m.containment<to> - top(m.containment) == {};
   
    // everything in the containment relation has been declared somewhere
    assert carrier(m.containment) - decls == {};

    // everything in the declarations relation is contained somewhere
    assert decls - carrier(m.containment) == {};

    // all uses point to actual declarations
    assert m.uses<name> - m.declarations<name> - m.implicitDeclarations == {};

    // in this example, all declarations are used at least once
    assert m.declarations<name> - m.uses<name> == {};

    // m.declarations is one-to-one
    assert size(m.declarations<name>) == size(m.declarations);

    // nothing in the AST that has a decl is not declared
    assert all(/node n := t && n.decl? && n.decl in decls);

    // all nodes have a .src attribute
    assert all(/node n := t && loc _ := n.src?|unknown:///|);

    // helper function for getting src location of a node
    loc \loc(node n) = loc f := (n.src?|unknown:///|(0,0)) ? f : |unknown:///|(0,0);
 
    // all sibling ast's are next to each other in the right order
    for(/[*_,node a, node b, *_] := t) {
        assert \loc(a).offset + \loc(a).length <= \loc(b).offset;
    }

   return true;
}
