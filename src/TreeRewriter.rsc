module TreeRewriter

import IO;
import List;
import Map;
import Node;
import ParseTree;
import Set;
import String;

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
            if (metaVar(loc metaWhere, value metaWhat) := edit) {
              int startPos = metaWhere.offset - what.offset;
              int endPos = startPos + metaWhere.length;
              if (loc l := metaWhat) {
                lex = lex[0..startPos] + readFile(l) + lex[endPos..];
              } else if (str s := metaWhat) {
                lex = lex[0..startPos] + s + lex[endPos..];
              }
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
  if (loc what := instance.src) {
    return [metaVar(pattern@\loc, what)];
  }
  throw "Unexpected arguments in concreteDiff: <pattern> vs <instance>";
}

Edits concreteDiff(&T <: node pattern, &T <: node instance) {
  if (pattern == instance) { //trees are equal
    return [];
  }
  if (!pattern.src? && !instance.src?) {
    if (/Tree _ !:= pattern) { //literal from concrete syntax
      return [];
    } //check children of concrete syntax fragment
    patternChildren = getChildren(pattern);
    instanceChildren = getChildren(instance);
    assert size(patternChildren) == size(instanceChildren);
    return [*concreteDiff(patternChildren[i], instanceChildren[i]) | i <- [0..size(patternChildren)]];
  }
  if (!pattern.src? && instance.src?) {
    //println("<pattern>");
    //println();
    //println(instance);
    //println("----");
    //println("patternName: <getName(pattern)>");
    //println("instanceName: <getName(instance)>");
    ;
  }
  if (pattern.src == instance.src) { //check children of original source code fragment
    patternChildren = getChildren(pattern);
    instanceChildren = getChildren(instance);
    assert size(patternChildren) == size(instanceChildren);
    return [*concreteDiff(patternChildren[i], instanceChildren[i]) | i <- [0..size(patternChildren)]];
  }
  throw "Unexpected arguments in concreteDiff:\n\n<pattern>\n\nvs\n\n<instance>";
}

bool isVariable(node n) = "loc" in getAnnotations(n);
bool isListVariable(node n) = isVariable(n) && contains("<n>", "*");

str getVariableName(str holeLit) = trim(substring(holeLit, findFirst(holeLit, "*") + 1, findFirst(holeLit, "\>")));
str getVariableName(node n) = getVariableName("<n>");

list[str] getListVariables(list[node] pattern) =  [getVariableName("<element>") | element <- pattern, isListVariable(element)];
list[str] getVariableNames(list[node] pattern) = [getVariableName("<element>") | element <- pattern, isVariable(element)];
bool hasListVariables(list[node] pattern) = (false | it || isListVariable(n) | n <- pattern);

loc asLoc(value v) {
  if (loc l := v) {
    return l;
  }
  throw "No loc: <v>";
}

loc extractLocFromPattern(list[node] pattern) {
  if (v <- pattern, isListVariable(v), loc l := getAnnotations(v)["loc"]) {
    return l;
  }
  throw "Did not find loc in pattern";
}

Edits concreteDiff(list[node] pattern, list[node] instance) {
  if (!hasListVariables(pattern)) {//No list variables, recurse on children
    return [*concreteDiff(pattern[i], instance[i]) | i <- [0..size(pattern)]];
  }
  
  //Get match bindings for corresponding module
  loc l = extractLocFromPattern(pattern);
  set[map[str, value]] matchBindings = getMatchBindings(l.top);
  variables = getVariableNames(pattern);
  
  //utility function to get a sublist skipping certain elements
  list[node] filt(list[node] lst, list[int] doMatch) = [lst[i] | i <- [0..size(lst)], i in doMatch];
  
  //utility function to recursively try and find appropriate match bindings for pattern variables
  tuple[set[map[str, value]] bindings, list[node] pattern] bindAndMatch(list[node] currentPattern, list[node] instance, set[map[str, value]] bindings, set[map[str, value]] actualBindings, list[int] doMatch) {
    if (!hasListVariables(currentPattern)) {
      if (size(currentPattern) != size(instance)) {
        throw "Backtrack";
      }
      skippedPattern = filt(currentPattern, doMatch);
      skippedInstance = filt(instance, doMatch);
      if (skippedPattern := skippedInstance) {
        return <actualBindings, currentPattern>;
      }
      throw "Backtrack";
    }
    //println("Variables: <getVariableNames(currentPattern)>");
    for (i <- [0..size(currentPattern)]) {
      if (isVariable(currentPattern[i])) {
        //println("Trying variable <currentPattern[i]>");
        variableName = getVariableName(currentPattern[i]);
        optionalBindings = {m | m <- matchBindings, variableName <- m};
        //println("\t<size(optionalBindings)> options");
        //k = 1;
        for (binding <- optionalBindings) {
          //println("\tOption <k>");
          //k = k + 1;
          try {
            if (list[node] var := binding[variableName]) {
              list[node] nextTry = currentPattern[0..i] + var + currentPattern[i+1..];
              return bindAndMatch(nextTry, instance, bindings, actualBindings + binding, doMatch + [i..i+size(var)]);
            } else  {
              throw "Unexpected";
            }
          } catch "Backtrack": {
            println("Backtracking");
          }
        }
        throw "Match failed";
      } else {
        skipOnMatch += i;
        skipFrom += 1;
      }
    }
    throw "Shouldn not reach here";
  }
  
  <actualBindings, boundPattern> = bindAndMatch(pattern, instance, matchBindings, {}, []);
  
  list[node] asList(value v) {
    if (list[node] n := v) {
      return n;
    }
    throw "Unexpected value in asList: <v>";
  }
  
  edits = [];
  offset = 0;
  for (i <- [0..size(pattern)]) {
    patVar = pattern[i];
    if (isListVariable(patVar)) {
      holeLoc = getAnnotations(patVar)["loc"];
      varImage = [*asList(binding[varName]) | binding <- actualBindings, str varName := getVariableName(patVar), varName <- binding];
      if (size(varImage) == 0) {
        edits += metaVar(holeLoc, "");
        offset -= 1;
      } else {
        loc srcLoc = asLoc(varImage[0].src);
        if (tail(varImage) != [] && loc last := varImage[-1].src) {
          srcLoc.length = last.offset - srcLoc.offset + last.length;
          //TODO: check for trailing whitespace/delimiter when we're not in the last list element
        }
        edits += metaVar(holeLoc, srcLoc);
        offset += size(varImage) - 1;
      }
    } else {
      edits += concreteDiff(patVar, instance[i+offset]);
    }
  }
  return edits;
}

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
  //TODO: does this happen?
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

@javaClass{TreeRewriterHelper}
@reflect{Need access to environment}
java set[map[str, value]] getMatchBindings(loc l);

