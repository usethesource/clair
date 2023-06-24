module lang::cpp::Registry

import analysis::m3::Registry;
import lang::cpp::AST;
import analysis::m3::Core;

@synopsis{This registers logical declarations for C and C++ programs directly from the AST.}
void registerTU(Declaration tu) {
  m = m3(tu.src.top, declarations={ <d, s> | /Declarator n := tu, n.name?, loc d := n.decl, loc s := n.name.src, d.scheme != "unknown"});
  registerProject(tu.src.top, m);
}