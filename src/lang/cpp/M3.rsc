module lang::cpp::M3

extend analysis::m3::Core;

import Node;
import lang::cpp::AST;

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
  rel[loc directive, loc resolved] includeResolution = {}
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
    = {<declarator.decl, functionName.decl> | /functionDefinition(_, _, Declarator declarator, _, Statement body) := tu, /functionCall(Expression functionName,_) := body,
      functionName.decl?}
    + {<declarator.decl, functionName.expression.decl> | /functionDefinition(_, _, Declarator declarator, _, Statement body) := tu, /functionCall(Expression functionName,_) := body,
      functionName is bracketed, functionName.expression.decl?}
    ;
  return model;
}

M3 createM3FromCppFile(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ()) {
  tuple[M3,Declaration] m3AndAst = parseCppToM3AndAst(file, includePaths = includePaths, additionalMacros = additionalMacros);
  return javaAstToM3(m3AndAst<1>, model = m3AndAst<0>);
}

tuple[M3, Declaration] createM3AndAstFromCppFile(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ()) {
  tuple[M3,Declaration] m3AndAst = parseCppToM3AndAst(file, includePaths = includePaths, additionalMacros = additionalMacros);
  return <javaAstToM3(m3AndAst<1>, model = m3AndAst<0>),m3AndAst<1>>;
}

@javaClass{lang.cpp.internal.Parser}
@reflect{need access to streams}
java tuple[M3, Declaration] parseCppToM3AndAst(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ());
