module lang::cpp::SConcrete

import IO;
import List;
import Node;
import String;
import Type;

import lang::cpp::AST;
import lang::cpp::Concrete;
import lang::cpp::ST;

int counter = 0;
map[loc, str] sourceCache = ();

@javaClass{lang.cpp.internal.STHelper}
@reflect{need access to streams}
java node findSimpleVariable(str name);

@javaClass{lang.cpp.internal.STHelper}
@reflect{need access to streams}
java list[&T] findListVariable(str name);

data Part = strPart(str src) | varPart(str var) | lvarPart(str var);

list[Part] toParts(str s) {
  inVariable = false;
  parts = [];
  while (s != "") {
    if (!inVariable) {
      i = findFirst(s, "[");
      if (i == -1) {
        parts += strPart(s);
        break;
      }
      parts += strPart(s[0..i]);
      s = s[i..];
      inVariable = true;
    } else {
      i = findFirst(s, "]");
      var = s[1..i];
      if (var[-1] == "*") {
        parts += lvarPart(var[..-1]);
      } else {
        parts += varPart(var);
      }
      s = s[i+1..];
      inVariable = false;
    }
  }
  return parts;
}

MyList[&T] findSurroundingList(list[&T] l, node ast) {
  if (/MyList[&T] ml := ast, bla:[*pre, *target, *post] := ml.elts, target == l) {
    if (lval(bla, seps=lSeps, sep=lSep) := ml) {//workaround for keyword parameters on parameterized ADTs
      return lval(target, seps=lSeps[size(pre)..size(pre)+size(target)-1], sep=lSep);
    }
  }
  throw "Impossible";
}

str flatten(list[Part] parts, node ast) {
  result = "";
  for (part <- parts) {
    if (strPart(src) := part) {
      result += src;
    } else if (varPart(var) := part) {
      node n = findSimpleVariable(var);
      result += yield(n);
    } else if (lvarPart(var) := part) {
      l = findListVariable(var);
      ml = findSurroundingList(l, ast);
      result += yield(ml);
    }
  }
  return result;
}

&T <: node substitute(type[&T <: node] typ, node ast, str source) {
  parts = toParts(source);
  foo = flatten(parts, ast);
  loc cacheLoc = |cache:///<"<counter>">|;
  sourceCache += (cacheLoc:foo);
  counter = counter + 1;
  switch(typ.symbol) {
    case adt("SDeclaration",[]) : return parseDeclaration(foo, cacheLoc);
    case adt("SDeclSpecifier",[]) : return parseDeclSpecifier(foo, cacheLoc);
    case adt("SStatement",[]) : return parseStatement(foo, cacheLoc);
    case adt("SExpression",[]) : return parseExpression(foo, cacheLoc);
    case adt("SName",[]) : return parseName(foo, cacheLoc);
    default: throw "Missed adt <typ>";
  }
}

&T <: node removeDeclAndType(&T <: node tree) = unsetRec(tree, {"decl", "typ"});

&T <: node adjustOffsets(&T <: node tree, loc base, int offset) =
  visit(removeDeclAndType(tree)) {
    case loc l : {
      if (l.scheme in {"file", "project", "home", "std", "prompt", "cache"}) {
        if (base.offset?) {
          insert base[offset=l.offset+base.offset-offset][length=l.length];
        } else {
          insert base[offset=l.offset-offset][length=l.length];
        }
      }
    }
  };

@concreteSyntax{SStatement}
SStatement parseStatement(str code, loc l) {
  str context = "void parse() {
                '  <code>
                '}";
  Declaration tu = parseString(context, l);
  adjusted = adjustOffsets(tu.declarations[0].body.statements[0], l, 18);
  stt = toST(adjusted, context, sourceCache);
  if (SStatement ss := stt) {
    return ss;
  }
  throw "Impossible"; 
}

@concreteHole{SStatement}
str makeStatementHole(int id) = "$$$$$clairStmt$<id>$$$$$();";

@concreteSyntax{SExpression}
SExpression parseExpression(str code, loc l) {
  str context = "void parse() {
                '  decltype(<code>) x;
                '}";
  Declaration tu = parseString(context, l);
  adjusted = adjustOffsets(tu.declarations[0].body.statements[0].declaration.declSpecifier.expression, l, 27);
  st = toST(adjusted, context, sourceCache);
  if (SExpression e := st) {
    return e;
  }
  throw "Impossible";
}

@concreteHole{SExpression}
str makeExpressionHole(int id) = "$$$$$clairExpr$<id>$$$$$";

@concreteSyntax{SName}
SName parseName(str code, loc l) {
  str context = "void <code>() {}";
  Declaration tu = parseString(context, l);
  if (SName n := toST(adjustOffsets(tu.declarations[0].declarator.name, l, 5), context, sourceCache)) {
    return n;
  }
  throw "Impossible";
}

@concreteHole{SName}
str makeNameHole(int id) = "_name$$<id>$$end";

@concreteSyntax{SDeclaration}
SDeclaration parseDeclaration(str code, loc l) {
  str context = "class C { <code> };";
  Declaration tu = parseString(context, l);
  adjusted = adjustOffsets(tu.declarations[0].declSpecifier.members[0], l, 10);
  st = toST(adjusted, context, sourceCache);
  if (SDeclaration ret := st) {
    return ret;
  }
  throw "Impossible";
}

@concreteHole{SDeclaration}
str makeDeclarationHole(int id) = "$clairDecl$<id>$ ClaiR {};";

@concreteSyntax{SDeclSpecifier}
SDeclSpecifier parseDeclSpecifier(str code, loc l) {
  Declaration tu = parseString("<code> myVariable;", l);
  fooz = tu.declarations[0].declSpecifier;
  bla = adjustOffsets(fooz, l, 0);
  node n = toST(bla);
  if (SDeclSpecifier ds := n) {
    return ds;
  }
  throw "Impossible";
}

@concreteHole{SDeclSpecifier}
str makeDeclSpecifierHole(int id) = "myType<id>EndType";