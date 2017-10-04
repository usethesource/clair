module lang::cpp::analysis::CompilerDependentCode

import lang::cpp::AST;
import Message;
import util::ResourceMarkers;
import util::Editors;
import util::ValueUI;
import IO;


set[loc] complexLeftHandSides(loc file) = complexLeftHandSides(parseCpp(file));
set[loc] complexLeftHandSides(Declaration cu) = 
  {lhs.src | /assign(lhs, _) := cu, isComplex(lhs)};
   
bool isComplex(fieldReference(_,_)) = false;
bool isComplex(\fieldReferencePointerDeref(_, _)) = false;  
bool isComplex(star(Expression n)) = false when !(n is star);
bool isComplex(idExpression(_)) = false;
default bool isComplex(Expression lhs) = true;


set[loc] complexLeftHandSidesOldStyle(loc file) = complexLeftHandSidesOldStyle(parseCpp(file));
set[loc] complexLeftHandSidesOldStyle(Declaration cu) {
  result = {};
  
  for (/assign(lhs, _) := cu) {
    if (isComplex(lhs)) { 
      result += lhs.src;
    }
  }

  return result;
} 