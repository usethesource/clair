module TreeRewriter

import IO;
import List;
import Map;
import Node;
import ParseTree;
import Set;

import util::Math;
import util::ValueUI;

import lang::cpp::AST;
import lang::cpp::Concrete;

data Edit
  = del(loc where)
  | ins(loc where, loc what)
  | ins(loc where, str lex)
  | replaceLoc(loc where, loc what, Edits metaVariables= [])
  | replaceStr(loc where, str lex, str prefix =  "", str postfix = "")
  | metaVar(loc where, loc what)
  | metaVar(loc where, str lex)
  ;

alias Edits = list[Edit];

void saveDiffs(Edits edits) {
  edits = sort(edits, bool(Edit e1, Edit e2) { return e1.where.file == e2.where.file? e1.where.offset > e2.where.offset : e1.where > e2.where; });
  for (Edit edit <- edits) {
    switch(edit) {
      case replaceLoc(where, what, metaVariables = metaVars): {
        if (size(metaVars) == 0) {
          writeFile(where, readFile(edit.what));
        } else {
          str lex = readFile(edit.what);
          for (Edit edit <- sort(metaVars, bool(Edit e1, Edit e2) { return e1.where.offset > e2.where.offset; })) {
            if (metaVar(loc metaWhere, loc metaWhat) := edit) {
              int startPos = metaWhere.offset - what.offset;
              int endPos = startPos + metaWhere.length;
              lex = lex[0..startPos] + readFile(metaWhat) + lex[endPos..];
            } else {
              throw "Unexpected edit <edit>";
            }
          }
          writeFile(where, lex);
        }
      }
      default:
        throw "NYI: <edit>";
    }
  }
}

Edits concreteDiff(Tree pattern, node instance) {
  if (loc where := pattern@\loc && loc what := instance.src) {
    return [metaVar(where, what)];
  }
  throw "Unexpected arguments in concreteDiff: <pattern> vs <instance>";
}

Edits concreteDiff(&T <: node pattern, &T <: node instance) {
  if (pattern == instance) {
    return [];
  }
  if (!pattern.src? && !instance.src?) { //literal from concrete syntax
    return [];
  }
  if (pattern.src == instance.src) {//check children
    patternChildren = getChildren(pattern);
    instanceChildren = getChildren(instance);
    return [*concreteDiff(patternChildren[i], instanceChildren[i]) | i <- [0..size(patternChildren)]];
  }
  throw "Unexpected arguments in concreteDiff: <pattern> vs <instance>";
}

Edits concreteDiff(list[value] pattern, list[value] instance) = [*concreteDiff(pattern[i], instance[i]) | i <- [0..size(pattern)]];
Edits concreteDiff(str _, str _) = [];
Edits concreteDiff(int _, int _) = [];

Edits diff(&T <: node old, &T <: node new) {
  if (old == new) {//trees are equal, no diff
    return [];
  }
  if (old.src == new.src) {//same node, checking children
    oldChildren = getChildren(old);
    newChildren = getChildren(new);
    return [*diff(oldChildren[i], newChildren[i]) | i <- [0..size(oldChildren)]];
  }
  if (loc newSrc := new.src && isConcreteSyntaxPattern(newSrc)) { //new node is concrete syntax pattern
    return [replaceLoc(oldSrc, newSrc, metaVariables=concreteDiff(getConcreteSyntaxImage(newSrc), new)) | loc oldSrc := old.src];
  } //new node is regular node
  return [replaceLoc(oldLoc, newLoc) | loc oldLoc := old.src, loc newLoc := new.src];
}

Edits diff(list[value] old, list[value] new) = [*diff(old[i], new[i])|i<-[0..size(old)]];
Edits diff(str old, str new) = [];
Edits diff(int old, int new) = [];

@javaClass{TreeRewriterHelper}
@reflect{Need access to environment}
java bool isConcreteSyntaxPattern(loc l);

@javaClass{TreeRewriterHelper}
@reflect{Need access to environment}
java node getConcreteSyntaxImage(loc l);

//Below: adapted from lang::ecore::diff::LCS
data Diff[&T <: node]
  = same(&T t1, &T t2)
  | add(&T t, int pos)
  | rremove(&T t, int pos)
  ;

list[Diff[&T <: node]] getDiff(map[int,map[int,int]] c, list[&T <: node] x, list[&T <: node] y, int i, int j, bool(&T <: node, &T <: node) equals) {
  if (i > 0, j > 0,  equals(x[i-1], y[j-1])) {
    return getDiff(c, x, y, i - 1, j - 1, equals) + [same(x[i-1], y[j-1])];
  }
  if (j > 0, (i == 0 || c[i][j-1] >= c[i-1][j])) {
    return getDiff(c, x, y, i, j-1, equals) + [add(y[j-1], j-1)];
  }
  if (i > 0, (j == 0 || c[i][j-1] < c[i-1][j])) {
    return getDiff(c, x, y, i-1, j, equals) + [rremove(x[i-1], i-1)];
  }
  return [];
}

map[int,map[int,int]] lcsMatrix(list[&T] x, list[&T] y, bool (&T,&T) equals) {
  map[int,map[int,int]] c = ();
  
  m = size(x);
  n = size(y);
  
  for (int i <- [0..m + 1]) {
    c[i] = ();
    c[i][0] = 0;
  }
  
  for (int j <- [0..n + 1]) {
    c[0][j] = 0;
  }
  
  for (int i <- [1..m + 1], int j <- [1.. n + 1]) {
    if (equals(x[i - 1], y[j - 1])) {
      c[i][j] = c[i-1][j-1] + 1;
    }
    else {
      c[i][j] = max(c[i][j-1], c[i-1][j]);
    }
  }
  
  return c;  
}
