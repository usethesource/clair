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
      popup(action("Draw Clair AST", drawAST))
    });
}

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

public FProperty popup(str s){
    return mouseOver(box(text(s, fontSize(24)), grow(1.1),resizable(false)));
}

void drawAST(str _, loc select) {
    ast = findNearEnoughSubTree(select, parseCpp(select.top));
    int count = 0;
    int next() { count +=1 ; return count; }

    default str txt(node x) = getName(x);
    default str mid(node x) { return "<txt(x)>:<x.src>"; }
    
    Figure treeNode(node x) 
      = box(text(txt(x), fontSize(20)), 
          popup(readFile(l)),
           id(mid(x)), 
           grow(1.2), 
           fillColor("yellow") 
        ) when loc l := x.src;
    
    Figure treeFig(node x) = tree(treeNode(x), [ treeFig(y) | arg <- x, (node y := arg || list[value] l := arg && node y <- l)], gap(10));
        
            
    render(treeFig(ast));
}
