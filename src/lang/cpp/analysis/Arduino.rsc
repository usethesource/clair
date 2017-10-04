module lang::cpp::Arduino

import util::Math;
import lang::cpp::AST;
import String;

int count(loc p) = (0 | it + toInt(theSize) | /simpleDeclaration(
  _, declSpecifier(_, _, char()),
  [arrayDeclarator(_, _, _, [arrayModifier(_,integerConstant(theSize))])]) := parseCpp(p)) ;