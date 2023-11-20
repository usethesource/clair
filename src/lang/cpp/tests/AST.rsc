module lang::cpp::tests::AST

extend lang::cpp::AST;
import analysis::m3::AST;
import List;
import util::Monitor;
import IO;

list[loc] testFiles() = |project://clair/src/test|.ls;

// TODO: make the example files satisfy this contract
/*test*/ bool astContract() {
    jobStart("AST contract", totalWork=size(testFiles()));
    for (loc l <- testFiles()) {
        jobStep("AST contract", "<l>");
    println(l);
        assert astNodeSpecification(parseCpp(l), language="cpp");
    }

    jobEnd("AST contact");
    return true;
}

test bool declTypeFeature() {
  t = parseCpp(|project://clair/src/test/declTypes.cpp|);

  /* we expect at two qualified names with a computed type expression in it: */
  computedNames = [q | /q:qualifiedName([decltypeName(Expression _), *_], _) := t];

  /* and we expect three declarations where the type of the declared variable is computed */
  computedTypes = [q | /q:declSpecifier(_, decltype(Expression _)) := t];
  
  return size(computedNames) == 2 && size(computedTypes) == 3;
}

