module lang::cpp::Analysis

import Exception;
import IO;
import List;
import Relation;
import Set;
import analysis::m3::Registry;
import analysis::m3::Core;
import lang::cpp::AST;
import lang::cpp::TypeSymbol;
import util::FileSystem;

void registerLinks(Declaration p) {
    M3 m = m3(|mm://haai/bye|, declarations={ <x.decl,x.src> | /Declarator x := p});
    registerProject(|mm://haai/bye|, m);
}

void registerLinks(rel[loc, loc] r) {
    M3 m = m3(|foo://bar|, declarations = r);
    registerProject(|foo://bar|, m);
}

set[Declaration] extractTemplates(Declaration ast)
  = { d | /Declaration d := ast, d is template };
  
set[Expression] extractFunctionInstances(Declaration ast)
  = { e | /Expression e := ast, e is idExpression, e.decl.scheme == "cpp+functionInstance" };
set[Expression] extractDeferredFunctions(Declaration ast)
  = { e | /Expression e := ast, e is idExpression, e.decl.scheme == "cpp+deferredFunction" };
  
rel[Expression instance, Declaration template] findFunctionInstances(Declaration ast) = {
  templates = extractTemplates(ast);
  instances = extractFunctionInstances(ast);

  return {<theInstance, theTemplate> | theInstance <- instances, theTemplate <- templates,
    theTemplate.declaration is functionDefinition, theTemplate.declaration.declarator.decl.path == theInstance.decl.path};
};

rel[loc instance, loc template] instantiatedFunctions(Declaration ast) = {
  templates = extractTemplates(ast);
  instances = extractFunctionInstances(ast);

  return { <theInstance.decl, theTemplate.declaration.declarator.decl> | theInstance <- instances, theTemplate <- templates,
    theTemplate.declaration is functionDefinition, theTemplate.declaration.declarator.decl.path == theInstance.decl.path };
};

rel[loc instance, loc template] deferredFunctions(Declaration ast) = {
  templates = extractTemplates(ast);
  deferred = extractDeferredFunctions(ast);

  return { <theDeferred.decl, theTemplate.declaration.declarator.decl> | theDeferred <- deferred, theTemplate <- templates,
    theTemplate.declaration is functionDefinition, theTemplate.declaration.declarator.decl.path == theDeferred.decl.path };
};

rel[loc caller, loc callee] sees(Declaration d, bool handleTemplates = false) = {
  rel[loc caller, loc callee] ret =
    { <caller.declarator.decl, c.decl> | /Declaration caller := d, caller has declarator, /Expression c := caller, c has decl }
    + { <caller.declarator.decl, c.decl> | /Declaration caller := d, caller has declarator, /Statement estmt := caller, estmt is expressionStatement, /Expression c := estmt, c has decl };
  
  if (handleTemplates) {
    for (tuple[loc instance, loc template] t <- instantiatedFunctions(d))
      ret += { <t.instance, callee> | callee <- ret[t.template] };
    for (tuple[loc deferred, loc template] t <- deferredFunctions(d))
      ret += { <t.deferred, callee> | callee <- ret[t.template] };
  }
  return ret;
};

rel[loc caller, loc callee] reaches(Declaration d) = sees(d)+;
rel[loc caller, loc callee] reaches(rel[loc,loc] r) = r+;

set[list[loc]] allSimplePaths(Declaration ast, loc src, loc dest) = allSimplePaths(sees(ast), src, dest);

set[list[&T]] allSimplePaths(rel[&T from, &T to] relation, &T src, &T dest) = {
  if (src notin carrier(relation))
    throw IllegalArgument(src, "Starting node <src> not in provided relation");
  if (dest notin carrier(relation))
    throw IllegalArgument(src, "Destination node <dest> not in provided relation");
  return pathsHelper(relation, relation*, [src], dest);
};

set[list[&T]] pathsHelper(rel[&T from, &T to] relation, rel[&T from, &T to] closure, list[&T] currentPath, &T dest) = {
  &T currentNode = last(currentPath);
  if (dest == currentNode)
    return {currentPath};

  set[list[&T]] ret = {};
  for (&T element <- relation[currentNode], element notin currentPath, dest in closure[element])
    ret += pathsHelper(relation, closure, currentPath+element, dest);

  return ret;
};


lrel[loc caller, loc callee] invocations(Declaration d) 
   = [ <caller.declarator.decl, c> | /Declaration caller := d, caller has declarator, /Expression exp := caller, c := callee(exp), c.scheme != "dunno"]
   ;

loc callee(functionCall(idExpression(_,decl=loc c), _)) = c;
loc callee(functionCall(fieldReference(_,_,decl=loc c), _)) = c;
loc callee(functionCall(fieldReferencePointerDeref(_,_,decl=loc c), _)) = c;
loc callee(pmDot(idExpression(_,decl=loc c),_)) = c;
loc callee(pmArrow(idExpression(_,decl=loc c),_)) = c;

loc callee(functionCall(bracketed(e), as)) = callee(functionCall(e, as));
loc callee(pmDot(bracketed(e1),e2)) = callee(pmDot(e1,e2));
loc callee(pmArrow(bracketed(e1),e2)) = callee(pmArrow(e1,e2));

default loc callee(Expression x) = |dunno:///|;



