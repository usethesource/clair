module lang::cpp::IDE

import util::IDE;
import util::ValueUI;
import util::Editors;

import lang::cpp::AST;
import lang::cpp::TypeSymbol;

import IO;
import Node;

import vis::Figure;
import vis::Render;
import vis::KeySym;
  
void main() {
  registerNonRascalContributions("org.eclipse.cdt.ui.editor.CEditor", 
    {  
      popup(action("Show Clair AST", showAST)),
      popup(action("Show Clean Clair AST", showCleanAST)),
      popup(action("Draw Clair AST", drawAST)),
      popup(action("Draw Annotated Clair AST", drawAnnotatedAST)),
      popup(action("Show location", printLocation))
    });
}

void printLocation(str _, loc select) =
  util::ValueUI::text(select);

void showAST(str _, loc select) 
  = text(findNearEnoughSubTree(select, parseCpp(select.top)));
  
void showCleanAST(str _, loc select) 
  = text(unsetRec(findNearEnoughSubTree(select, parseCpp(select.top))));

@doc{finds the smallest tree which contains the full selection}
node findNearEnoughSubTree(loc select, node tree) {
   for (a <- tree) {
      if (node arg := a, begin(select) >= begin(arg.src), end(select) <= end(arg.src)) { 
        return findNearEnoughSubTree(select, arg);
      }
      else if (list[value] args := a) {
        if (node arg <- args, begin(select) >= begin(arg.src), end(select) <= end(arg.src)) {
          return findNearEnoughSubTree(select, arg);
        }      
      } 
   }
   
   return tree;
}

default node findNearEnoughSubTree(loc _, value t) = t;

private int end(loc l)   = l.offset + l.length; 
private int begin(loc l) = l.offset;

void drawAST(str x, loc select) {
  drawASTOptions(x, select);
}

void drawAnnotatedAST(str x, loc select) {
  drawASTOptions(x, select, withTypes=true, withNames=true);
}

void drawASTOptions(str _, loc select, bool withTypes=false, bool withNames=false) {
    ast = findNearEnoughSubTree(select, parseCpp(select.top));
    int count = 0;
    int next() { count +=1 ; return count; }
    str yield = readFile(select);
    
    FProperty popup(str s, str color){
      return mouseOver(box(text(s, fontSize(24)), grow(1.1),resizable(false), fillColor(color)));
    }
    
    str theDecl(loc l) = l.authority when l.scheme == "cpp+problem";
    default str theDecl(loc l) = "<l>";
    
    FProperty theColor(node x) = fillColor("lightgreen") when x.decl?, loc d := x.decl, "cpp+problem" != d.scheme;
    FProperty theColor(node x) = fillColor("red") when x.decl?, loc d := x.decl, "cpp+problem" == d.scheme;
    default FProperty theColor(node x) = fillColor("yellow");
     
    default str txt(node x) = getName(x);
    default str mid(node x) { return "<txt(x)>:<x.src>"; }
    
    Figure treeNode(node x) 
      = box(vcat([text(txt(x), fontSize(20)), 
                *[box(text(theDecl(x.decl), fontSize(15)),grow(1.1)) | withNames, x.decl?],
                *[box(text("<x.typ>", fontSize(15)), grow(1.1)) | withTypes, x.typ?]],
                gap(5)), 
           popup(readFile(l), "white"),
           id(mid(x)), 
           grow(1.2), 
           theColor(x) 
        ) when loc l := x.src;
    
    Figure treeFig(node x) = tree(treeNode(x), [ treeFig(y) | y <- x], gap(10));
    Figure treeFig(list[value] l) = tree(box(size(10)), [ treeFig(e) | e <- l], gap(10));
    default Figure treeFig(value x) = ellipse(text("<x>", fontSize(20)));    
            
    render(treeFig(ast));
}
