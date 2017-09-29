module lang::cpp::IDE

import util::IDE;
import util::ValueUI;
import lang::cpp::AST;
import IO;
  
void main() {
  registerNonRascalContributions("org.eclipse.cdt.ui.editor.CEditor", 
    {  
      popup(action("Show Clair AST", showAST))
    });
}

void showAST(str _, loc select) 
  = text(findNearEnoughSubTree(select, parseCpp(select.top)));

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