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
  rel[loc file, loc macro] macros = {}
);

M3 javaAstToM3(Declaration tu, M3 model = m3(tu.src.top)) {
  model.declarations = {<declarator.decl, declarator.src> | /Declarator declarator := tu, declarator has name, !(declarator.name is abstractEmptyName)};
  model.uses = { <physical,logical> | /Declaration d := tu, /node n := d, n.src?, loc physical := n.src, n.decl?, loc logical := n.decl};
  model.modifiers = {<declarator.decl,unset(modifier)> | /Declaration d := tu, d.declarator?, Declarator declarator := d.declarator, modifier <- d.declSpecifier.modifiers}
               + {<declarator.decl,unset(modifier)> | /Declaration d := tu, d.declarators?, Declarator declarator <- d.declarators, modifier <- d.declSpecifier.modifiers}
               + {<d.decl,unset(modifier)> | /DeclSpecifier d := tu, d.baseSpecifiers?, bs <- d.baseSpecifiers, modifier <- bs.modifiers};
  model.extends = {<base.decl,derived.decl> | /DeclSpecifier derived := tu, derived.baseSpecifiers?, base <- derived.baseSpecifiers};
  return model;
}

M3 createM3FromCppFile(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ()) {
  M3 model = parseCppToM3(file, includePaths = includePaths, additionalMacros = additionalMacros);
  Declaration ast = parseCpp(file, includePaths = includePaths, additionalMacros = additionalMacros);
  return javaAstToM3(ast, model = model);
}

@javaClass{lang.cpp.internal.Parser}
@reflect{need access to streams}
java M3 parseCppToM3(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ());
