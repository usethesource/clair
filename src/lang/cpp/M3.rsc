@license{Copyright (c) 2016-2020, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
All rights reserved. 
 
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
 
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
  
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

}
module lang::cpp::M3

extend analysis::m3::Core;

import IO;
import Node;
import String;
import Set;
import lang::cpp::AST;
import lang::cpp::TypeSymbol;

import Relation;
import analysis::graphs::Graph;
import analysis::m3::Registry;

data M3(
  set[loc] implicitDeclarations = {},
  rel[loc base, loc derived] extends = {},
  rel[loc caller, loc callee] methodInvocations = {},
  rel[loc field, loc accesser] fieldAccess = {},
  rel[loc caller, loc typeName] typeDependency = {},
  rel[loc base, loc override] methodOverrides = {},
  rel[loc file, loc macro] macroExpansions = {},		//not in included files
  rel[loc macro, loc src] macroDefinitions = {},
  rel[loc directive, loc occurrence] includeDirectives = {},
  rel[loc directive, loc occurrence] inactiveIncludes = {},
  rel[loc directive, loc resolved] includeResolution = {},
  rel[loc decl, TypeSymbol typ] declaredType = {},
  rel[loc decl, loc visiblity] memberAccessModifiers = {},
  rel[loc decl, loc src] functionDefinitions = {},
  rel[loc decl, loc src] cFunctionsToNoArgs = {},
  rel[loc decl, loc impl] declarationToDefinition = {},
  rel[loc directive, loc file] unresolvedIncludes = {},
  list[loc] comments = [],
  
  rel[loc includer, loc includee] requires = {},
  rel[loc provider, loc providee] provides = {},
  rel[loc decl, loc file] partOf = {},
  rel[loc caller, loc callee] callGraph = {}
);

/* methodInvocations: functionName is bracketed, !functionName.expression.decl?, !(getName(functionName.expression) in {"pmArrow","pmDot","star"}) : empty
 *                    functionName is functionCall: already in recursion
 * modifiers: probably incomplete
 */
M3 cppASTToM3(Declaration tu, M3 model = m3(tu.src.top)) {
  model.declarations 
    = {<declarator.decl, declarator.src> | /Declarator declarator := tu, declarator has name, !(declarator.name is abstractEmptyName)} 
    + { <declSpec.decl, declSpec.src> | /simpleDeclaration(DeclSpecifier declSpec, _) := tu, declSpec is class}
    + {<tu.decl, tu.src>}
    ;
  model.uses = { <physical,logical> | /Declaration d := tu, /node n := d, n.src?, loc physical := n.src, n.decl?, loc logical := n.decl};
  model.modifiers = {<declarator.decl,unset(modifier)> | /Declaration d := tu, d.declarator?, Declarator declarator := d.declarator, modifier <- d.declSpecifier.modifiers}
               + {<declarator.decl,unset(modifier)> | /Declaration d := tu, d.declarators?, Declarator declarator <- d.declarators, modifier <- d.declSpecifier.modifiers}
               + {<d.decl,unset(modifier)> | /DeclSpecifier d := tu, d.baseSpecifiers?, bs <- d.baseSpecifiers, modifier <- bs.modifiers};
  model.extends = {<base.decl,derived.decl> | /DeclSpecifier derived := tu, derived.baseSpecifiers?, base <- derived.baseSpecifiers};
  model.methodInvocations
    = { // direct function calls 
        *{<declarator.decl, functionName.decl> | /functionCall(Expression functionName, _) := body, functionName.decl?},

        // calls via brackets
        *{<declarator.decl, functionName.expression.decl> | /functionCall(Expression functionName, _) := body, functionName is bracketed, functionName.expression.decl?},
    
        // constructor calls
        *{<declarator.decl, e.decl> | /Expression e := body, e is new || e is newWithArgs || e is globalNew || e is globalNewWithArgs, e.decl?}
      
      | /functionDefinition(_, Declarator declarator, _, Statement body) := tu
      }
      ;
  model.memberAccessModifiers = deriveAccessModifiers(tu);
  model.declarationToDefinition = model.declarations<1,0> o model.functionDefinitions;
  model.cFunctionsToNoArgs = {<function, loseCArgs(function)> | function <- model.functionDefinitions<0>};
  
  model.requires = {<model.id, pretty(resolved)> | resolved <- model.includeResolution<1>};
  model.provides = {<definition.top, declaration.top> | <declaration, definition> <- model.declarationToDefinition};
  model.partOf = {<function, model.id> | <function,_> <- model.functionDefinitions};
  model.callGraph = extractCallGraph(tu);
  
  return model;
}

rel[loc caller, loc callee] extractCallGraph(Declaration ast) = extractCallGraph({ast});

@synopsis{extracts dependencies between every declaration and every name that is used in it, that is not-not a "call"}
rel[loc caller, loc callee] extractCallGraph(set[Declaration] asts)
  = { <caller.declarator.decl, c.decl> | ast <- asts, /Declaration caller := ast, caller has declarator, /Expression c := caller, c has decl,
      c.decl.scheme notin {"cpp+class", "cpp+enumerator", "cpp+field", "cpp+parameter", "cpp+typedef", "cpp+variable", "c+variable", "unknown", "cpp+unknown"} };		//Over-approximation

loc pretty(loc subject) = |<subject.scheme>://<pretty(subject.path)>|;
str pretty(str path) = replaceAll(path, "\\", "/");

loc loseCArgs(loc subject) = subject.scheme=="c+function"?|c+function://<loseCArgs(subject.path)>|:subject;
str loseCArgs(str path) = "<path[..findFirst(path,"(")]>()";

rel[loc, loc] deriveAccessModifiers(Declaration tu) {
  result = {};
  for (/class(_, _, _, members) := tu) {
    result += deriveAccessModifiers(members, false);
  }
  for (/classFinal(_, _, _, members) := tu) {
    result += deriveAccessModifiers(members, false);
  }
  for (/struct(_, _, _, members) := tu) {
    result += deriveAccessModifiers(members, true);
  }
  for (/structFinal(_, _, _, members) := tu) {
    result += deriveAccessModifiers(members, true);
  }
  return result;
}
rel[loc, loc] deriveAccessModifiers(list[Declaration] declarations, bool isStruct) {
  result = {};
  current = isStruct? |public:///| : |private:///|;
  for (declaration <- declarations) {
    if (declaration is visibilityLabel) {
      switch (declaration.visibility) {
        case \public(): current = |public:///|;
        case \protected(): current = |protected:///|;
        case \private(): current = |private:///|;
      }
    } else if (declaration is template || declaration is explicitTemplateSpecialization) {
      result += makeEntry(declaration.declaration, current);
    } else {
      result += makeEntry(declaration, current);
    }
  }
  return result;
}

rel[loc, loc] makeEntry(Declaration declaration, loc current) {
  if (declaration has declarators) {
    return {<declarator.decl, current> | declarator <- declaration.declarators};
  } else if (declaration has declarator) {
    return {<declaration.declarator.decl, current>};
  } else {
    if (!(declaration is \alias) && !(declaration is staticAssert) && !(declaration is usingDeclaration) && !(declaration is problemDeclaration)) {
      println("Missed entry for member visibility?");
      iprintln(declaration);
    }
    return {};
  }
}

M3 createM3FromCFile(loc file, list[loc] stdLib = [], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false) {
  tuple[M3,Declaration] m3AndAst = parseCToM3AndAst(file, stdLib = stdLib, includeDirs = includeDirs, additionalMacros = additionalMacros, includeStdLib = includeStdLib);
  return cppASTToM3(m3AndAst<1>, model = m3AndAst<0>);
}

M3 createM3FromCppFile(loc file, list[loc] stdLib = classPaths["vs12"], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false) {
  tuple[M3,Declaration] m3AndAst = parseCppToM3AndAst(file, stdLib = stdLib, includeDirs = includeDirs, additionalMacros = additionalMacros, includeStdLib = includeStdLib);
  return cppASTToM3(m3AndAst<1>, model = m3AndAst<0>);
}

tuple[M3, Declaration] createM3AndAstFromCFile(loc file, list[loc] stdLib = [], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false) {
  tuple[M3,Declaration] m3AndAst = parseCToM3AndAst(file, stdLib = stdLib, includeDirs = includeDirs, additionalMacros = additionalMacros, includeStdLib = includeStdLib);
  return <cppASTToM3(m3AndAst<1>, model = m3AndAst<0>),m3AndAst<1>>;
}

tuple[M3, Declaration] createM3AndAstFromCppFile(loc file, list[loc] stdLib = classPaths["vs12"], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false) {
  tuple[M3,Declaration] m3AndAst = parseCppToM3AndAst(file, stdLib = stdLib, includeDirs = includeDirs, additionalMacros = additionalMacros, includeStdLib = includeStdLib);
  return <cppASTToM3(m3AndAst<1>, model = m3AndAst<0>),m3AndAst<1>>;
}

@javaClass{lang.cpp.internal.Parser}
java tuple[M3, Declaration] parseCppToM3AndAst(loc file, list[loc] stdLib = classPaths["vs12"], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false);

@javaClass{lang.cpp.internal.Parser}
java tuple[M3, Declaration] parseCToM3AndAst(loc file, list[loc] stdLib = [], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false);

M3 composeCppM3(loc id, set[M3] models) {
  M3 comp = composeM3(id, models); 
  
  comp.implicitDeclarations = {*model.implicitDeclarations | model <- models};
  comp.extends = {*model.extends | model <- models};
  comp.methodInvocations = {*model.methodInvocations | model <- models};
  comp.fieldAccess = {*model.fieldAccess | model <- models};
  comp.typeDependency = {*model.typeDependency | model <- models};
  comp.methodOverrides = {*model.methodOverrides | model <- models};
  comp.macroExpansions = {*model.macroExpansions | model <- models};
  comp.macroDefinitions = {*model.macroDefinitions | model <- models};
  comp.includeDirectives = {*model.includeDirectives | model <- models};
  comp.inactiveIncludes = {*model.inactiveIncludes | model <- models};
  comp.includeResolution = {*model.includeResolution | model <- models};
  comp.memberAccessModifiers = {*model.memberAccessModifiers | model <- models};
  comp.declaredType = {*model.declaredType | model <- models};
  comp.functionDefinitions = {*model.functionDefinitions | model <- models};
  comp.cFunctionsToNoArgs = {*model.cFunctionsToNoArgs | model <- models};
  comp.declarationToDefinition = {*model.declarationToDefinition | model <- models};
  comp.unresolvedIncludes = {*model.unresolvedIncludes | model <- models};
  comp.comments = [*model.comments | model <- models];

  comp.requires = {*model.requires | model <- models};
  comp.provides = {*model.provides | model <- models};
  comp.partOf = {*model.partOf | model <- models};
  comp.callGraph = {*model.callGraph | model <- models};

  return comp;
}

@synopsis{fills out the call graph by adding the tuples for possible actual methods and constructors, and removing the corresponding calls to virtual methods and constructors.}
rel[loc caller, loc callee] closeOverriddenVirtualCalls(M3 comp) {
  return comp.callGraph 
    += comp.callGraph o comp.methodOverrides // add the overridden definitions
    - rangeR(comp.callGraph, comp.methodOverrides<0>); // remove the virtual intermediates
}

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