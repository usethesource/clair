module lang::cpp::M3

extend analysis::m3::Core;

import lang::cpp::AST;

data M3(
  rel[loc base, loc derived] extends = {},
  rel[loc caller, loc callee] methodInvocations = {},
  rel[loc field, loc accesser] fieldAccess = {},
  rel[loc caller, loc typeName] typeDependency = {},
  rel[loc base, loc override] methodOverrides = {} 
);

M3 javaAstToM3(Declaration tu) {
  M3 m3 = m3(tu.src.top);
}