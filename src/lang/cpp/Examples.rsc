@license{Copyright (c) 2016-2017, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
All rights reserved. 
 
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
 
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
  
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

}
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