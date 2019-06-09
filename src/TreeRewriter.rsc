module TreeRewriter

import IO;
import List;
import Node;
import ParseTree;
import String;


import lang::cpp::AST;

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
          srcLoc.length = last.offset - srcLoc.offset + last.length;
          //TODO: check for trailing whitespace/delimiter when we're not in the last list element
        }
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
