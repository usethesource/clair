module analysis::m3::LearnPrettyPrinter

import IO;
import Node;
import Type;
import List;
import Location;
import String;
import Set;

import util::FileSystem;
import lang::cpp::AST;

@synopsis{this is a demo/test function that learns a C pretty printer from the C files in the `example` folder.}
void generateFormatterForC() {
    writeFile(|project://tree-pattern-generator/src/main/rascal/analysis/diff/GeneratedFormatter.rsc|,
    "module analysis::diff::GeneratedFormatter
    '
    'import lang::cpp::AST;
    '
    '<filesToPrettyPrinter(find(|project://tree-pattern-generator/examples|, "c"), #Declaration, parseC)>
    '");
}

@synopsis{Takes a corpus of example files and an (external) parser for the given language and produces a pretty printing function as Rascal code.}
@description{
If the external parser satisfies the specification in ((analysis::m3::AST)) then this function will learn a basic
`unparser` (a.k.a. pretty printer) from the corpus of files. 

The code that is generated is Rascal code which can be expanded into a module that imports the right AST format, and nothing else.

The "pretty" aspect should not be over-estimated:
* The generated formatter does not deal with nested indentation. 
* It has one rule for every unique AST node kind (constructor), so special combinations do not get special treatment
* It is not able to generate syntactically correct files for layout-sensitive languages
* If the core example for a node contains source code comments, these will end up in the output

This function is typically used in source-to-source transformations where an intermediate AST to AST transformation
takes place. To derive source code from fresh syntax nodes, the format function is required.
}
str filesToPrettyPrinter(set[loc] files, type[node] grammar, node (loc) parser) 
    = treesToPrettyPrinter({parser(f) | f <- files}, grammar);

@synopsis{Takes a corpus of syntax trees (for the same language!) and produces Rascal code that can generate code back from ASTs.}
str treesToPrettyPrinter(set[node] asts, type[node] grammar) {
    // First we collect some general information for quick and easy reference later
    // This the string contents of each file 
    rel[loc, str] files  = {<l.top, readFile(l.top)> | n <- asts, loc l := n.src};

    // These are _all_ the sub-trees and their location in the input file 
    rel[loc, node] nodes = {<l, n> | /node n := asts, n.src?, loc l := n.src};

    // these are all the sub-strings for all the sub-strees, indexed by location in the input file
    rel[loc, str] yields = {<l, content[l.offset..l.offset+l.length]> | <l, _> <- nodes, str content <- files[l.top]};

    // now we have a function generated for each node type in the language. This also removes duplicates.
    rel[str, str] formatters = { <"<getName(n)>(<"<for (i <- [0..arity(n)]) {><\type(n[i])> arg_<i+1>, <}>"[..-2]>)", formatFunction(y, n)> | <l,y> <- yields, n <- nodes[l]};

    // we prefer the longest alternatives, given that those would retain more information from the example code.
    // different lengths my appear when some constructors do not use optionals or have empty lists in the corpus
    rel[str, str] bestFormatters = {<key, sort(formatters[key], bool (str x, str y) { return x > y; })[-1]> | key <- formatters<0> };

    return
        "@synopsis{Formats abstract syntax trees back to source code.}
        '<for (<_, f> <- bestFormatters) {><f>
        '
        '<}>";
}

str formatFunction(str yield, node n)
    = "str format(<getName(n)>(" + "<for (i <- [0..arity(n)]) {><\type(n[i])> arg_<i+1>, <}>"[..-2] + ")) =
      '\"<cutOut(yield, n)>\";";

private data Template 
    = hole(int n)
    | sep(str chars)
    | \list(str sep, int n)
    | const(str chars, int n)
    ;

@synopsis{get a template out of an example node by replacing the children with holes}
str cutOut(str yield, node n) {
    children  = getChildren(n);
    positions = position(\loc(n), children);
    ind       = index(children);
    begin     = \loc(n).offset;

    pattern = for (<value ch, loc pos, int i> <- reverse(zip3(children, positions, ind))) {
        append sep(yield[-begin + pos.offset + pos.length ..]);
        append template(begin, ch, i+1, yield);
        yield = yield[..-begin + pos.offset];
    }
    
    if (positions[0]?) {
        pattern += sep(yield[..positions[0].offset]);
    }
    else {
        pattern += sep(yield);
    }

    // println("PATTERN for <n.src>");
    // iprintln(pattern);
    return "<for (p <- reverse(pattern)) {><print(p)><}>";
}

@synopsis{Turns a child into a template element}
Template template(int _begin, int _, int i, str _yield)               = const("<i>", i); // normal nodes become holes
Template template(int _begin, str s, int i, str _yield)               = const(s, i); // normal nodes become holes
Template template(int _begin, node _, int i, str _yield)              = hole(i); // normal nodes become holes
Template template(int _begin, [], int i, str _yield)                  = sep(""); // empty lists are empty separators
Template template(int _begin, [node a], int i, str yield)             = \list("", i); // singletons do not have separators
Template template(int  begin, [node a, node b, *_], int i, str yield) = \list(yield[-begin+\loc(a).offset+\loc(a).length..-begin+\loc(b).offset], i); // we learn a separator from the non-empty lists
    
@synopsis{Prints a rascal code template part, including recursive calls.}    
str print(hole(int i)) = "\<format(arg_<i>)\>";
str print(sep(str chars)) = escape(chars);
str print(const(str _chars, int i)) = "\<arg_<i>\>";
str print(\list(str sep, int i)) = "\<\"\<for (value v \<- arg_<i>) {\>\<format(v)\><escape(sep)>\<}\>\"<if (sep != "") {>[..-<size(sep)>]<}>\>";

@synopsis{Give every element a true location for later processing.}
list[loc] position(loc span, list[value] l) = infer(span, [pos(span, x) | x <- l]);

@synsopsis{An element either knows its position, or it does not.}
loc pos(loc span, int _)                 = span;
loc pos(loc span, str _)                 = span;
loc pos(loc _span, [])                   = |empty:///|;
loc pos(loc _span, node n)               = \loc(n);
loc pos(loc _span, [node n])             = \loc(n);
loc pos(loc _span, [node a, *_, node b]) = cover([\loc(a), \loc(b)]);

@synopsis{Replaces all |empty:///| with a correct loc inferred from the surroundings}
list[loc] infer(loc span, [loc l, *loc rest])                       = infer(span, [span[length=0], *rest]) when l == |empty:///|;
list[loc] infer(loc span, [*loc rest, loc l])                       = infer(span, [*rest, span[offset=span.offset+span.length-1][length=0]]) when l == |empty:///|;
list[loc] infer(loc span, [*loc pre, loc before, loc l, *loc post]) = infer(span, [*pre, before, before[offset=before.offset+before.length][length = 0], *post]) when l == |empty:///|;
list[loc] infer(loc span, [*loc pre, loc l, loc after, *loc post])  = infer(span, [*pre, after[offset=after.offset][length = 0], after, *post]) when l == |empty:///|;
default list[loc] infer(loc _span, list[loc] done)                                   = done;

@synopsis{Print the type of a value, as Rascal code}
str \type(value n) = "<type(typeOf(n), ())>";

@synopsis{Escape string elements for use in a Rascal template string}
str escape(str s) = "<[s]>"[2..-2];

@synopsis{Waiting for `node.src` to be available in Rascal for good...}
loc \loc(node n) = l when loc l := n.src;
