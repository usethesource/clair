module lang::cpp::Examples

//import lang::cpp::AST;
//import IO;
//import vis::Figure;
//import vis::Render;
//import Relation;
//
//rel[str,str] showCalls(Declaration tu) 
//  = ();//{<from,to> | /functionDefinition(_,functionDeclarator(_,_,name(from),_,_),_,/functionCall(/name(to), _)) := tu};
//
//void vagueCallGraph(Declaration tu) {
//  calls = showCalls(tu);
//  render(graph([box(text(name), id(name), size(50), fillColor("lightgreen")) | name <- carrier(calls)], 
//               [edge(from,to, toArrow(box(size(20)))) | <from,to> <- calls], 
//               hint("layered"), gap(100)));
//}  
//
//void t(Declaration tu) {
//  for (/Declaration f:= tu)
//    println(f);
//}