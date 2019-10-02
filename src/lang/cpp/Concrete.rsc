@license{Copyright (c) 2016-2018, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
All rights reserved. 
 
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
 
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
  
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

}
module lang::cpp::Concrete

import IO;
import Node;

import lang::cpp::AST;

&T <: node cleanup(&T <: node x) = unsetRec(x, {"decl", "typ"});

&T <: node adjustOffsets(&T <: node x, loc base, int pre) {
  return visit(x) {
  case loc l : {
    if (l.scheme in {"file", "project", "home", "std", "prompt"} && l.offset >= pre) {
      insert base.top[offset = base.offset + l.offset - pre][length = l.length];
    }
  }
  }
}

@concreteSyntax{Statement}
Statement parseStatement(str code, loc l) {
  str context = "void parse() {
                '  <code>
                '}";
  Declaration tu = adjustOffsets(parseString(context, l), l, 17);
  return cleanup(tu.declarations[0].body.statements[0]); 
}

@concreteSyntax{Statement*}
list[Statement] parseStatements(str code, loc l) {
  str context = "void parse() {
                '  <code>
                '}";
  Declaration tu = adjustOffsets(parseString(context, l), l, 17);
  return [cleanup(s) | s <- tu.declarations[0].body.statements];
}

@concreteHole{Statement}
str makeStatementHole(int id) = "$$$$$clairStmt$<id>$$$$$();";

@concreteSyntax{Expression}
Expression parseExpression(str code, loc l) {
  str context = "void parse() {
                '  decltype(<code>) x;
                '}";
  Declaration tu = adjustOffsets(parseString(context), l, 26);
  return cleanup(tu.declarations[0].body.statements[0].declaration.declSpecifier.expression);
}

@concreteSyntax{Expression*}
list[Expression] parseExpressions(str code, loc l) {
  str context = "void parse() {
                '  x = f(<code>);
                '}";
  Declaration tu = adjustOffsets(parseString(context), l, 23);
  return [cleanup(e) | e <- tu.declarations[0].body.statements[0].expression.rhs.arguments];
}

@concreteHole{Expression}
str makeExpressionHole(int id) = "$$$$$clairExpr$<id>$$$$$";

@concreteSyntax{Name}
Name parseName(str code, loc l) {
  str context = "void <code>() {}";
  Declaration tu = adjustOffsets(parseString(context), l, 5);
  return cleanup(tu.declarations[0].declarator.name);
}

@concreteHole{Name}
str makeNameHole(int id) = "_name$$<id>$$end";

@concreteSyntax{Declaration}
Declaration parseDeclaration(str code, loc l) {
  //iprintln(cleanup(parseString("class C { <code> };")));
  Declaration tu = adjustOffsets(parseString("class C { <code> };"), l, 10);
  iprintln(tu);
  Declaration ret = tu.declarations[0].declSpecifier.members[0];
  if (ret is problemDeclaration)
    throw "Invalid input for external parser";
  return cleanup(ret);
}

@concreteSyntax{Declaration*}
list[Declaration] parseDeclarations(str code, loc l) {
  Declaration tu = adjustOffsets(parseString("class C { <code> };"), l, 10);
  iprintln(tu);
  return [cleanup(declaration) | declaration <- tu.declarations[0].declSpecifier.members];
}

@concreteHole{Declaration}
str makeDeclarationHole(int id) = "$clairDecl$<id>$ ClaiR {};";

@concreteSyntax{DeclSpecifier}
DeclSpecifier parseDeclSpecifier(str code, loc l) {
  Declaration tu = adjustOffsets(parseString("<code> myVariable;"), l, 5);
  return cleanup(tu.declarations[0].declSpecifier);
}

@concreteHole{DeclSpecifier}
str makeDeclSpecifierHole(int id) = "myType<id>EndType";
