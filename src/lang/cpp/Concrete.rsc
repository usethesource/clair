module lang::cpp::Concrete

import IO;
import List;
import Node;

import lang::cpp::AST;
import lang::rascal::\syntax::Rascal;
import util::ValueUI;

public &T <: node unsetRec(&T <: node x, str label) = visit(x) { 
  case node n => unset(n, label) 
};

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
                '  decltype(<code>) x;
                '}";
  Declaration tu = parseString(context);
  if (translationUnit([functionDefinition([],
        declSpecifier([],[],\void()),
        functionDeclarator([],[],[],name("parse"),[],[],decl=loc d1),[],
        compoundStatement([],
          [declarationStatement([],
            simpleDeclaration([],
              declSpecifier([],[],decltype(),
                Expression e),
              [declarator([],[],name("x"),decl=loc d2)]))]))]) := tu
      && d1 == |cpp+function:///parse()| && d2 == |cpp+variable:///parse()/x|)
    return unsetRec(e);
  throw "Unexpected AST in parseExpr: <tu>";
}

