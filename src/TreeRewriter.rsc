module TreeRewriter

import IO;
import List;
import Map;
import Node;
import ParseTree;
import Set;
import String;

import util::ValueUI;

import lang::cpp::AST;

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

str unescape(str raw) {
  solve(raw) {
    singleQuote = findFirst(raw, "\'");
    if (singleQuote != -1) {
      newLine = findLast(raw[0..singleQuote], "\n");
      raw = raw[0..newLine+1] + raw[singleQuote+1..];
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

lrel[loc, str] processEdits(map[loc, str] fragments, Edits edits) {
  ret = [];
  edits = sort(edits, bool(Edit e1, Edit e2) { return e1.where.file == e2.where.file? e1.where.offset > e2.where.offset : e1.where > e2.where; });
  for (Edit edit <- edits) {
    switch(edit) {
      case replaceLoc(where, what, metaVariables = metaVars): {
        if (size(metaVars) == 0) {
          ret += <where, fragments[edit.what]>;
        } else {
          str lex = fragments[edit.what];
          for (Edit edit <- sort(metaVars, bool(Edit e1, Edit e2) { return e1.where.offset > e2.where.offset; })) {
            if (metaVar(loc metaWhere, value metaWhat) := edit) {
              int startPos = metaWhere.offset - what.offset;
              int endPos = startPos + metaWhere.length;
              if (loc l := metaWhat) {
                lex = lex[0..startPos] + fragments[l] + lex[endPos..];
              } else if (str s := metaWhat) {
                lex = lex[0..startPos] + s + lex[endPos..];
              }
            } else {
              throw "Unexpected edit <edit>";
            }
          }
          ret += <where, unescape(lex)>;
        }
      }
      default:
        throw "NYI: <edit>";
    }
  }
  return ret;
}

map[loc, str] readCode(Edits edits) = (l:readFile(l) | /loc l := edits);

bool isVariable(node n) = "loc" in getAnnotations(n);
bool isListVariable(node n) = isVariable(n) && contains("<n>", "*");

str getVariableName(str holeLit) = trim(substring(holeLit, findFirst(holeLit, "*") + 1, findFirst(holeLit, "\>")));
str getVariableName(node n) = getVariableName("<n>");
bool hasListVariables(list[node] pattern) = (false | it || isListVariable(n) | n <- pattern);
default bool hasListVariables(value _) = false;

map[str, int] decodeFragment(str fragment) = (elems[0]:toInt(elems[1]) | s <- split(",", fragment), elems := split(":", s), size(elems) == 2);

//bool sameLocs(loc l, loc r) = l.scheme == r.scheme && l.authority == r.authority && l.path == r.path && l.offset == r.offset && l.length == r.length;
bool sameLocs(loc l, loc r) {
  loc tmp = l;
  tmp.fragment = r.fragment;
  return tmp == r;
}

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
  
loc extractLocFromPattern(list[node] pattern) {
  if (v <- pattern, isListVariable(v)) {
    return asLoc(getAnnotations(v)["loc"]);
  }
  throw "Did not find loc in pattern";
}

Edits concreteDiff(Tree pattern, node instance, map[str, int] listVarLengths) = [metaVar(pattern@\loc, asLoc(instance.src))];

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
  for (i <- [0..size(pattern)]) {
    patVar = pattern[i];
    if (isListVariable(patVar)) {
      holeLoc = getAnnotations(patVar)["loc"];
      varImage = instance[i+offset..i+offset+listVarLengths[getVariableName(patVar)]];
      if (size(varImage) == 0) {
        edits += metaVar(holeLoc, "");
      } else {
        loc srcLoc = asLoc(varImage[0].src);
        if (tail(varImage) != [] && loc last := varImage[-1].src) {
          srcLoc.length = last.offset - srcLoc.offset + last.length;
          //TODO: check for trailing whitespace/delimiter when we're not in the last list element
        }
        edits += metaVar(holeLoc, srcLoc);
      }
      offset += size(varImage) - 1;
    } else {
      edits += concreteDiff(patVar, instance[i+offset], listVarLengths);
    }
  }
  return edits;
}

Edits diff(&T <: node old, &T <: node new, map[str, int] listVarLengths) {
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
    return [replaceLoc(oldSrc, newSrc, metaVariables=concreteDiff(getConcreteSyntaxImage(newSrc), new, listVarLengths))];
  }
  return [replaceLoc(oldSrc, newSrc)];
}

Edits diff(list[value] old, list[value] new, map[str, int] listVarLengths) {
  if (node elem <- new, elemSrc := asLoc(elem.src), isConcreteSyntaxPattern(elemSrc), img := getConcreteSyntaxImage(elemSrc), hasListVariables(img)){
    //Found a list element originating from a list variable in a concrete syntax pattern (assumption: whole list is a concrete syntax pattern)
    toReplace = asLoc(asNode(old[0]).src);
    if (tail(old) != []) {
      lstLoc = asLoc(asNode(tail(old)[-1]).src);
      toReplace.length = lstLoc.offset - toReplace.offset + lstLoc.length;
    }
    return [replaceLoc(toReplace, elemSrc, metaVariables=concreteDiff(asList(img), new, listVarLengths))];
  }
  return [*diff(old[i], new[i], listVarLengths) | i <- [0..size(old)]];
}

@javaClass{TreeRewriterHelper}
@reflect{Need access to environment}
java bool isConcreteSyntaxPattern(loc l);

@javaClass{TreeRewriterHelper}
@reflect{Need access to environment}
java value getConcreteSyntaxImage(loc l);

@javaClass{TreeRewriterHelper}
@reflect{Need access to environment}
java set[map[str, value]] getMatchBindings(loc l);

