module lang::cpp::SConcrete

import IO;
import Node;
import String;

import lang::cpp::AST;
import lang::cpp::Concrete;
import lang::cpp::ST;

int counter = 0;
map[loc, str] sourceCache = ();

@javaClass{lang.cpp.internal.STHelper}
@reflect{need access to streams}
java node findVariable(str name);

@javaClass{lang.cpp.internal.STHelper}
@reflect{need access to streams}
java str substitute(str source);

data Part = strPart(str src) | varPart(str var);

list[Part] toParts(str s) {
  inVariable = false;
  parts = [];
  while (s != "") {
    if (!inVariable) {
      index = findFirst(s, "[");
      if (index == -1) {
        parts += strPart(s);
        break;
      }
      parts += strPart(s[0..index]);
      s = s[index..];
      inVariable = true;
    } else {
      index = findFirst(s, "]");
      parts += varPart(s[1..index]);
      s = s[index+1..];
      inVariable = false;
    }
  }
  //iprintln(parts);
  return parts;
}

str flatten(list[Part] parts) {
  result = "";
  for (part <- parts) {
    if (strPart(src) := part) {
      result += src;
    } else if (varPart(var) := part) {
      node n = findVariable(var);
      str y = yield(n);
      result += y;
    }
  }
  return result;
}

node substitute2(type[&T] typ, str source) {
  parts = toParts(source);
  foo = flatten(parts);
  loc cacheLoc = |cache:///<"<counter>">|;
  sourceCache += (cacheLoc:foo);
  tu = parseDeclaration(foo, cacheLoc);
  counter = counter + 1;
  return tu;
  //
  //println("substitute2: \"<source>\"");
  //str result = substitute(source);
  //println("result: <result>");
  //loc cacheLoc = |cache:///<"<counter>">|;
  //sourceCache += (cacheLoc:result);
  //counter = counter + 1;
  //Declaration tu = parseDeclaration(result, cacheLoc);
  //return toST(tu, result);
}

&T <: node removeDeclAndType(&T <: node tree) = unsetRec(tree, {"decl", "typ"});

&T <: node adjustOffsets(&T <: node tree, loc base, int offset) =
  visit(removeDeclAndType(tree)) {
    case loc l : {
      if (l.scheme in {"file", "project", "home", "std", "prompt", "cache"} /*&& l.offset >= offset*/) {
        //println("---");
        //println(l);
        //println( base[offset=l.offset+base.offset-offset][length=l.length]);
        if (base.offset?) {
          insert base[offset=l.offset+base.offset-offset][length=l.length];
        } else {
          insert base[offset=l.offset-offset][length=l.length];
        }
      }
    }
      //when (l.scheme in {"file", "project", "home", "std", "prompt"} && l.offset >= offset)
  };

@concreteSyntax{SStatement}
SStatement parseStatement(str code, loc l) {
  str context = "void parse() {
                '  <code>
                '}";
  Declaration tu = parseString(context, l);
  if (SStatement s := toST(adjustOffsets(tu.declarations[0].body.statements[0], l, 18))) {
    return s;
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
  if (SExpression e := toST(adjustOffsets(tu.declarations[0].body.statements[0].declaration.declSpecifier.expression, l, 27))) {
    return e;
  }
  throw "Impossible";
}

@concreteHole{Expression}
str makeExpressionHole(int id) = "$$$$$clairExpr$<id>$$$$$";

@concreteSyntax{SName}
SName parseName(str code, loc l) {
  str context = "void <code>() {}";
  Declaration tu = parseString(context, l);
  if (SName n := toST(adjustOffsets(tu.declarations[0].declarator.name, l, 5))) {
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