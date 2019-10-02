module TreeRewriter

import IO;
import List;
import Node;
import ParseTree;
import String;
import util::ValueUI;

import lang::cpp::AST;
import lang::cpp::Concrete;

//Declaration getAst() = parseCpp(|project://clair/src/test/rewriteSmall.cpp|);
Declaration getAst() = parseCpp(|project://clair/src/test/encapsulate.cpp|);

bool biprintln(value arg) {
  iprintln(arg);
  return true;
}
//
public list[Expression] ee = (Expression*)`5,6`;
list[Statement] ss() = (Statement*)`int x;
'x = f(<Expression* ee>, 1);
'return;`;
Declaration foobar() = (Declaration)`class C {
                                    'int x;
                                    '};`; 
list[Declaration] foobar2() = (Declaration*)`class C {
                                            'int x;
                                            '  char c;
                                            '};`;


Edits transform() = transform(getAst());
Edits transform(node ast) {
  newAst = visit(ast) {
    //case (Expression)`<Name n>(<Expression e1>, <Expression e2>)` => (Expression)`<Name n>(<Expression e2>)`
    //case (Statement)`int <Name n> = <Expression e>;` => (Statement)`int <Name n> = <Expression e> * <Expression e>;`
    //case (Expression*)`<Expression* es>, 1` => (Expression*)`<Expression* es>,  2`
    //case (Statement*)`<Statement* pre>
    //                 'x = <Expression e>;
    //                 '<Statement* post>`
    //  => (Statement*)`<Statement* pre>
    //                 'int x = 3;
    //                 'y = x + <Expression e>;
    //                 '<Statement* post>
    //                 '<Statement* post>`
   //case 0:;
    case (Declaration)`class <Name c> {
                      '  <Declaration* pre>
                      '  public:
                      '  <Declaration* inbetween>
                      '  <DeclSpecifier ds> <Name n>;
                      '  <Declaration* post>
                      '};`
      => (Declaration)`class <Name c> {
                      '  <Declaration* pre>
                      '  public: 
                      '  <Declaration* inbetween>
                      '  private: 
                      '  <DeclSpecifier ds> <Name n>;
                      '  public:
                      '  void setn(ds _n) { n = _n; }
                      '  ds getn() { return n; }
                      '  <Declaration* post>
                      '};`
         when !(unsetRec((Declaration)`private:`) <- inbetween || unsetRec((Declaration)`protected:`) <- inbetween)// && biprintln(ast)
           //Declaration setter := parseDeclaration("void set<readFile(n.src)>(<readFile(ds.src)> val) { <readFile(n.src)> = val; }"),
           //Declaration setter := parseDeclaration("<readFile(ds.src)> get<readFile(n.src)>() { return <readFile(n.src)>; }")
           //&& Declaration getter := parseDeclaration("<type> get<n>() { return <n>; }")
           //&& Declaration setter := (Declaration)`void set<
           //case (Expression*)`<Expression* es>, 1` => (Expression*)`<Expression* es>, 2`
    //case (Statement)`z = f(<Expression* es>, 1);` => (Statement)`z = f(<Expression* es>, 0);`
    //case (Expression)`foo(<Expression l>, <Expression r>)`
    //  => (Expression)`foo(<Expression r>, 
    //                 '               5, <Expression r>, <Expression l>)`
    //case (Expression*)`<Expression* e>, <Expression f>`
    //  => (Expression*)`<Expression f>,  <Expression* e>`
    //case (Expression)`<Expression e> + <Expression f>`
    //  => (Expression)`2 * <Expression e>` when e := f
    //case (Expression)`f(<Expression* es>, <Expression* fs>)`
    //  => (Expression)`f(<Expression* fs>, <Expression* es>)` when size(es) == 4
    //case (Expression)`<Expression _> + <Expression e>` => e
    //case (Expression)`<Expression _> + <Expression e>` => (Expression)`<Expression e>`
    //case (Expression)`<Expression e1> + <Expression e2>` => (Expression)`<Expression e2> * <Expression e1>`
    //case (Expression)`<Expression e> + <Expression e>` => (Expression)`2 * <Expression e>`
    //case (Expression)`4` => (Expression)`42`
    //case (Declaration)`<DeclSpecifier ds> <Name n>() { <Statement pre> x = <Expression xexpr>; <Statement post> }`
    //  => (Declaration)`<DeclSpecifier ds> <Name n>() { <Statement pre> <Statement post> }`
    //case (Declaration)`<DeclSpecifier ds> <Name n>() { <Statement* foo><Statement* bar> }`
    //  => (Declaration)`<DeclSpecifier ds> <Name n>() { <Statement* bar><Statement* foo> }`
    //case (Statement*)`<Statement* as><Statement b>` => (Statement*)`<Statement b><Statement* as>` when (Statement)`int y = <Expression _>;` := b
    //case (Statement*)`<Statement* as><Statement* bs>` => (Statement*)`<Statement* bs><Statement* as>` when as!:=(Statement*)`` && bs!:=(Statement*)``
    //case (Declaration)`<DeclSpecifier ds> <Name n>() { <Statement* foo> return <Expression ret>; <Statement* bar>} `
    //  => (Declaration)`<DeclSpecifier ds> <Name n>() { <Statement* bar> <Statement* bar> int z = 42; return <Expression ret>; <Statement* foo>}`
  };
  //text(unsetRec(newAst, {"typ", "decl"}));
  //text(unsetRec(newAst));
  return diff(ast, newAst, ());
}

//TODO: remove whitespace/delimiter on empty list variable

set[node] astStack = {};
void addToStack(node n) {
  if (/n !:= astStack) {
    astStack += n;
  }
}
node findAst(loc l, node _) {
  if (/node n := astStack, n has src, n.src == l) {
    return n;
  }
  throw "Did not find node with loc <l>"; 
}
list[node] findAst(list[node] nodes) {
  return [];
}

data Edit
  = delete(loc where, loc prev = |unset:///|, loc post = |unset:///|)
  | edit(loc where, loc what, Edits edits)
  ;

alias Edits = list[Edit];

str unescape(str raw, str prefix) {
  solve(raw) {
    singleQuote = findFirst(raw, "\'");
    if (singleQuote != -1) {
      newLine = findLast(raw[0..singleQuote], "\n");
      raw = raw[0..newLine+1] + prefix + raw[singleQuote+1..];
    }
  }
  raw = replaceAll(raw, "\\\<", "\<");
  raw = replaceAll(raw, "\\\>", "\>");
  return raw;
}

void saveDiffs(Edits edits) {
  for (<where, what> <- processEdits(readCode(edits), edits)) {
    writeFile(where, what);
  }
}

lrel[loc, str] processEdits(map[loc, str] fragments, Edits edits)
  = [<edit.where, processEdit(fragments, edit)> | edit <- sort(edits, bool(Edit e1, Edit e2) { return e1.where.offset > e2.where.offset; })];

str readPrefix(loc l) {
  str file = readFile(l.top);
  i = findLast(file[..l.offset], "\n");
  return file[i+1..l.offset];
}

str processEdit(map[loc, str] fragments, delete(loc where)) = "";
str processEdit(map[loc, str] fragments, edit(loc where, loc what, Edits edits)) {
  ret = fragments[what];
  if (size(edits) == 0) {
    return ret; 
  }
  for (e <- sort(edits, bool(Edit e1, Edit e2) { return e1.where.offset > e2.where.offset; })) {
    ins = processEdit(fragments, e);
    offset = e.where.offset - what.offset;
    ret = ret[..offset] + ins + ret[offset + e.where.length..];
  }
  return unescape(ret, readPrefix(where));
}

map[loc, str] readCode(Edits edits) = (l:readFile(l) | /loc l := edits);

str getVariableName(str holeLit) = trim(substring(holeLit, findFirst(holeLit, "*") + 1, findFirst(holeLit, "\>")));
str getVariableName(node n) = getVariableName("<n>");

bool isListVariable(node n) = "loc" in getAnnotations(n) && contains("<n>", "*");
bool hasListVariables(list[node] pattern) = (false | it || isListVariable(n) | n <- pattern);
default bool hasListVariables(value _) = false;

map[str, int] decodeFragment(str fragment) = (elems[0]:toInt(elems[1]) | s <- split(",", fragment), elems := split(":", s), size(elems) == 2);

//bool sameLocs(loc l, loc r) = l.scheme == r.scheme && l.authority == r.authority && l.path == r.path && l.offset == r.offset && l.length == r.length;
bool sameLocs(loc l, loc r) = l[fragment=""]==r[fragment=""];

loc asLoc(value v) {
  if (loc l := v) {
    return l;
  }
  throw "No loc: <v>";
}

node asNode(value v) {
  if (node n := v) {
    return n;
  }
  throw "No node: <v>";
}

list[node] asList(value v) {
  if (list[node] lst := v) {
    return lst;
  }
  throw "Unexpected value in asList: <v>";
}

loc getLocation(node n) = asLoc(getAnnotations(n)["loc"]?n.src);
  
Edits concreteDiff(Tree pattern, node instance, map[str, int] listVarLengths) {
  d = diff(findAst(instance.src, instance), instance, listVarLengths);
  if (d != []) {
    println("nested diff: <d>");
  }
  return [edit(pattern@\loc, asLoc(instance.src), [])];
}

Edits concreteDiff(&T <: node pattern, &T <: node instance, map[str, int] listVarLengths) {
  if (pattern == instance) { //trees are equal
    return [];
  }
  patternChildren = getChildren(pattern);
  instanceChildren = getChildren(instance);
  assert size(patternChildren) == size(instanceChildren);
  return [*concreteDiff(patternChildren[i], instanceChildren[i], listVarLengths) | i <- [0..size(patternChildren)]];
}

Edits concreteDiff(list[node] pattern, list[node] instance, map[str, int] listVarLengths) {
  if (!hasListVariables(pattern)) {//No list variables, recurse on children
    return [*concreteDiff(pattern[i], instance[i], listVarLengths) | i <- [0..size(pattern)]];
  }
  
  edits = [];
  offset = 0;
  removeLeadingChars = false;//do not remove layout before first list element
  for (i <- [0..size(pattern)]) {
    patVar = pattern[i];
    if (isListVariable(patVar)) {
      holeLoc = getLocation(patVar);
      varImage = instance[i+offset..i+offset+listVarLengths[getVariableName(patVar)]];
      if (size(varImage) == 0) {//list variable bound to [], fix surrounding layout (separators/whitespace)
        //edits += delete(holeLoc);
        if (i < size(pattern) - 1) {//non-final list element, remove trailing layout
          edits += delete(holeLoc, post = getLocation(pattern[i+1]));
        } else {//last list element, remove leading layout if not removed already
          if (removeLeadingChars) {
            edits += delete(holeLoc, prev = getLocation(pattern[i-1]));
          } else {
            edits += delete(holeLoc);
          }
        }
        removeLeadingChars = false;
      } else {
        loc srcLoc = asLoc(varImage[0].src);
        if (tail(varImage) != [] && loc last := varImage[-1].src) {
          println("last.offset: <last.offset>");
          println("srcLoc.offset: <srcLoc.offset>");
          println("last.length: <last.length>");
          println("foo: <last.offset - srcLoc.offset + last.length>");
          srcLoc.length = last.offset - srcLoc.offset + last.length;
          //TODO: check for trailing whitespace/delimiter when we're not in the last list element
        }
        foo = findAst(varImage);
        println(foo);
        edits += edit(holeLoc, srcLoc, []);
        removeLeadingChars = true;
      }
      offset += size(varImage) - 1;
    } else {
      edits += concreteDiff(patVar, instance[i+offset], listVarLengths);
    }
  }
  return edits;
}

//Statement fooo() {
//  //iprintln((Statement)`x = g(1,44);`);
//  //iprintln((Expression*)`1,2,33`);
//  //if ( /(Expression*)`1,2` := (Statement)`f(1,2);`) println("FOOOOOO");
//  //iprintln(visit((Statement)`y = f(1,2,3);`) {
//  //  case bla:(Expression)`f(<Expression* pre>, 3)`  => (Expression)`f(x,<Expression* pre>)`
//  //});
//  iprintln(visit((Statement)`y = x + 4;`) {
//    case (Expression)`x +  <Expression e>` => (Expression)`<Expression e> - 1`
//  });
//  return (Statement)`y = f(11,222,     3333);`;
//}

Edits diff(&T <: node old, &T <: node new, map[str, int] listVarLengths) {
  addToStack(old);
  if (old == new) {//trees are equal, no diff
    return [];
  }
  if (sameLocs(old.src, new.src)) {//same node, checking children
    oldChildren = getChildren(old);
    newChildren = getChildren(new);
    return [*diff(oldChildren[i], newChildren[i], decodeFragment(asLoc(new.src).fragment)) | i <- [0..size(oldChildren)]];
  }
  oldSrc = asLoc(old.src);
  newSrc = asLoc(new.src);
  
  if (isConcreteSyntaxPattern(newSrc)) { //new node is concrete syntax pattern
    return [edit(oldSrc, newSrc, concreteDiff(getConcreteSyntaxImage(newSrc), new, listVarLengths))];
  }
  return [edit(oldSrc, newSrc, [])];
}

Edits diff(list[value] old, list[value] new, map[str, int] listVarLengths) {
  if (node elem <- new, elemSrc := asLoc(elem.src), isConcreteSyntaxPattern(elemSrc), img := getConcreteSyntaxImage(elemSrc), hasListVariables(img)){
    //Found a list element originating from a list variable in a concrete syntax pattern (assumption: whole list is a concrete syntax pattern)
    toReplace = asLoc(asNode(old[0]).src);
    if (tail(old) != []) {
      lstLoc = asLoc(asNode(tail(old)[-1]).src);
      toReplace.length = lstLoc.offset - toReplace.offset + lstLoc.length;
    }
    return [edit(toReplace, elemSrc, concreteDiff(asList(img), new, listVarLengths))];
  }
  return [*diff(old[i], new[i], listVarLengths) | i <- [0..size(old)]];
}

@javaClass{TreeRewriterHelper}
@reflect{Need access to environment}
java bool isConcreteSyntaxPattern(loc l);

@javaClass{TreeRewriterHelper}
@reflect{Need access to environment}
java value getConcreteSyntaxImage(loc l);
