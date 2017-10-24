module lang::cpp::analysis::CompilerDependentCode

import lang::cpp::AST;
import Message;
import util::ResourceMarkers;
import util::Editors;
import util::ValueUI;
import IO;

// TOOL: add some messages to Eclipse
void mark(set[loc] positions, str message) {
   addMessageMarkers({warning(message, pos) | pos <- positions});
}   

set[loc] complexLeftHandSidesImperative(loc file) 
  = complexLeftHandSidesImperative(parseCpp(file));

set[loc] complexLeftHandSidesImperative(Declaration tu) {
  set[loc] result = {};
  
  for (/assign(lhs, _) := tu) {
    if (isComplex(lhs)) { 
      result += lhs.src;
    }
  }

  return result;
} 

bool isComplex(arraySubscriptExpression(_, idExpression(_))) = false;
bool isComplex(fieldReference(_,_)) = false;
bool isComplex(\fieldReferencePointerDeref(_, _)) = false;  
bool isComplex(star(Expression n)) = false when !(n is star);
bool isComplex(idExpression(_)) = false;
bool isComplex(bracketed(Expression e)) = isComplex(e);

default bool isComplex(Expression lhs) = true;

set[loc] complexLeftHandSidesFunctional(loc file) 
  = complexLeftHandFunctional(parseCpp(file));

set[loc] complexLeftHandSidesFunctional(Declaration tu) 
  = {lhs.src | /assign(lhs, _) := tu, isComplex(lhs)};
   


/* -------- more precise and more general: direct data dependencies */

set[loc] directDependents(Declaration tu) = 
  { v.src | /assign(/prefixIncr(v:idExpression(_,decl=d)), /idExpression(_,decl=d))  := tu} +
  { v.src | /assign(/postfixIncr(v:idExpression(_,decl=d)), /idExpression(_,decl=d)) := tu};

/* more general but less accurateL indirect dependence */

set[loc] indirectDependents(Declaration tu) {
   brokenDeps = uses(tu) o flow(tu)+ o updates(tu);
   return brokenDeps<1>;
} 

rel[loc from, loc to] flow(Declaration tu) 
  = {<from.decl, to.decl> | /assign(/to:idExpression(_), /from:idExpression(_)) := tu};
  
rel[loc src, loc name] uses(Declaration tu) 
  = {<var.src, var.decl>  | /assign(_, /var:idExpression(_)) := tu};
   
rel[loc name, loc src] updates(Declaration tu) = 
  { <var.decl, var.src> | /assign(/prefixIncr(var), _)  := tu} +
  { <var.decl, var.src> | /assign(/postfixIncr(var), _) := tu};
