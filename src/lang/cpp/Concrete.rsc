module lang::cpp::Concrete

import IO;
import List;
import Node;

import lang::cpp::AST;
import lang::rascal::\syntax::Rascal;
import util::ValueUI;

Statement parseStmt(str code) {
  str context = "void parse() {
                '  <code>
                '}";
  Declaration tu = parseString(context);
  if (translationUnit([functionDefinition(
        [],
        declSpecifier([],[],\void()),
        //functionDeclarator([],[],[],name("parse"),[],[],decl=|cpp+function:///parse()|), gives error
        functionDeclarator([],[],[],name("parse"),[],[],decl=loc d),
        [],
        compoundStatement(_,[Statement s]))]) := tu && d == |cpp+function:///parse()|)
    return unsetRec(s);
  throw "Unexpected AST in parseStmt: <tu>"; 
}

Expression parseExpr(str code) {
  str context = "void parse() {
                '  sizeof(<code>);
                '}";
  Declaration tu = parseString(context);
  if (translationUnit([functionDefinition([],
        declSpecifier([],[],\void()),
        functionDeclarator([],[],[],name("parse"),[],[],decl=loc d),
        [],
        compoundStatement([],
          [expressionStatement([],
            sizeof(bracketed(Expression e)))]))]) := tu && d == |cpp+function:///parse()|)
    return unsetRec(e);
  throw "Unexpected AST in parseExpr: <tu.declarations[0]>";
}

