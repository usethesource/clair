module lang::cpp::M3

extend analysis::m3::Core;

import Node;
import lang::cpp::AST;
import lang::cpp::TypeSymbol;

public data M3(
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
  list[loc] comments = []
);

/* methodInvocations: functionName is bracketed, !functionName.expression.decl?, !(getName(functionName.expression) in {"pmArrow","pmDot","star"}) : empty
 *                    functionName is functionCall: already in recursion
 * modifiers: probably incomplete
 */
M3 javaAstToM3(Declaration tu, M3 model = m3(tu.src.top)) {
  model.declarations = {<declarator.decl, declarator.src> | /Declarator declarator := tu, declarator has name, !(declarator.name is abstractEmptyName)};
  model.uses = { <physical,logical> | /Declaration d := tu, /node n := d, n.src?, loc physical := n.src, n.decl?, loc logical := n.decl};
  model.modifiers = {<declarator.decl,unset(modifier)> | /Declaration d := tu, d.declarator?, Declarator declarator := d.declarator, modifier <- d.declSpecifier.modifiers}
               + {<declarator.decl,unset(modifier)> | /Declaration d := tu, d.declarators?, Declarator declarator <- d.declarators, modifier <- d.declSpecifier.modifiers}
               + {<d.decl,unset(modifier)> | /DeclSpecifier d := tu, d.baseSpecifiers?, bs <- d.baseSpecifiers, modifier <- bs.modifiers};
  model.extends = {<base.decl,derived.decl> | /DeclSpecifier derived := tu, derived.baseSpecifiers?, base <- derived.baseSpecifiers};
  model.methodInvocations
    = {<declarator.decl, functionName.decl> | /functionDefinition(_, Declarator declarator, _, Statement body) := tu, /functionCall(Expression functionName,_) := body,
      functionName.decl?}
    + {<declarator.decl, functionName.expression.decl> | /functionDefinition(_, Declarator declarator, _, Statement body) := tu, /functionCall(Expression functionName,_) := body,
      functionName is bracketed, functionName.expression.decl?}
    ;
  model.memberAccessModifiers = deriveAccessModifiers(tu);
  return model;
}

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
    } else if (declaration is template) {
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
    if (!(declaration is staticAssert)) {
      println("Missed entry for member visibility?");
      iprintln(declaration);
    }
    return {};
  }
}

M3 createM3FromCppFile(loc file, list[loc] stdLib = classPaths["vs12"], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false) {
  tuple[M3,Declaration] m3AndAst = parseCppToM3AndAst(file, stdLib = stdLib, includeDirs = includeDirs, additionalMacros = additionalMacros, includeStdLib = includeStdLib);
  return javaAstToM3(m3AndAst<1>, model = m3AndAst<0>);
}

tuple[M3, Declaration] createM3AndAstFromCppFile(loc file, list[loc] stdLib = classPaths["vs12"], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false) {
  tuple[M3,Declaration] m3AndAst = parseCppToM3AndAst(file, stdLib = stdLib, includeDirs = includeDirs, additionalMacros = additionalMacros, includeStdLib = includeStdLib);
  return <javaAstToM3(m3AndAst<1>, model = m3AndAst<0>),m3AndAst<1>>;
}

@javaClass{lang.cpp.internal.Parser}
@reflect{need access to streams}
java tuple[M3, Declaration] parseCppToM3AndAst(loc file, list[loc] stdLib = classPaths["vs12"], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false);
