module lang::cpp::Concrete

import IO;
import List;
import Node;

import lang::cpp::AST;
import util::ValueUI;

public &T <: node unsetRec(&T <: node x, str label) = visit(x) { 
  case node n => unset(n, label) 
};

@concreteSyntax
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
        compoundStatement([],[Statement s]))]) := tu && d == |cpp+function:///parse()|)
    return unsetRec(s);
  throw "Unexpected AST in parseStmt: <tu>"; 
}

@concreteSyntax
list[Statement] parseStmts(str code) {
  str context = "void parse() {
                '  <code>
                '}";
  Declaration tu = parseString(context);
  if (translationUnit([functionDefinition(
        [],
        declSpecifier([],[],\void()),
        functionDeclarator([],[],[],name("parse"),[],[],decl=loc d),
        [],
        compoundStatement([],list[Statement] statements))]) := tu && d == |cpp+function:///parse()|)
    return [unsetRec(s) | s <- statements];
  throw "Unexpected AST in parseStmts: <tu>";
}

@concreteSyntax
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

@concreteSyntax
Declaration parseDecl(str code) {
  Declaration tu = parseString(code);
  if (translationUnit([Declaration d]) := tu) return unsetRec(d);
  throw "Unexpected AST in parseDecl: <tu>"; 
}

@concreteSyntax
Expression parseType(str code) {
  str context = "auto fun() -\> <code>;";
  Declaration tu = parseString(context);
  if (translationUnit([simpleDeclaration([],
      declSpecifier([],[],auto()),
      [functionDeclarator([],[],[],name("fun"),[],[],
          Expression t,
          decl=loc d)])]) := tu && t is typeId && d == |cpp+function:///fun()|) 
    return unsetRec(t);
  throw "Unexpected AST in parseType: <tu>";
}

@concreteAntiquoteToHole
str createHoleForAntiquote(Expr _, str id) = "$$$$$clairExpr$<id>$$$$$";

@concreteAntiquoteToHole
str createHoleForAntiquote(Stmt _, str id) = "$$$$$clairStmt$<id>$$$$$()";

void foo(Declaration ast) {
  list[Statement] stmts = {s | /s:expressionStatement(
            [],
            functionCall(
              idExpression(
                name(
                  "$$$$$clairStmt$1$$$$$",
                  src=|file:///|(13,21)),
                src=|file:///|(13,21),
                decl=|cpp+problem://Attempt%20to%20use%20symbol%20failed:%20$$$$$clairStmt$1$$$$$|,
                typ=problemType("Failure to determine type of expression")),
              [],
              src=|file:///|(13,23),
              typ=problemBinding()),
            src=|file:///|(13,24)) := ast };
}

void bar(Declaration ast) {
  list[Expression] exprs = {e | /e:idExpression(
              name(
                "$$$$$clairExpr$1$$$$$",
                src=|project://clair/src/test/test.cpp|(485,21)),
              src=|project://clair/src/test/test.cpp|(485,21),
              decl=|cpp+problem://Attempt%20to%20use%20symbol%20failed:%20$$$$$clairExpr$1$$$$$|,
              typ=problemType("Failure to determine type of expression")) := ast};
}
