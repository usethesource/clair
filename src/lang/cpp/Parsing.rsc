@license{Copyright (c) 2016-2017, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
All rights reserved. 
 
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
 
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
  
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

}
module lang::cpp::Parsing

//import lang::cpp::AST;
//import IO;
//import util::FileSystem;
//import Relation;
//import Set;
//import Map;
//
//
//set[loc] listCppFiles(loc root, set[str] exts = {"cpp"}) = {*find(root,ext)|ext<-exts};
//
//map[loc, Declaration] parseFiles(set[loc] files) = (file: parseCpp(file)|file <- files, bprintln("Parsing <file>"));
//
//set[Declaration] getAsts (map[loc,Declaration] locDeclMap) = range(locDeclMap);
//
//set[Expression] getFQNs (set[Declaration] asts) = {*{ qualifiedName(x,y) | /qualifiedName(x,y) := t} | t <- asts};
//
//set[Expression] canCalls (set[Expression] fqns) = { fqn | fqn <- fqns, name("Send",_) := fqn.lastName};
//
//rel[str,str] funToFQN (set[Declaration] asts) =
//	{<from,to> | ast<- asts, /functionDefinition(_,/name(from), _, /functionCall(/qualifiedName(_,name(to)),_)) := ast};
//
//rel[loc,loc] funToLocs (set[Declaration] asts) =
//	{<a.src,b.src> | ast<- asts, /functionDefinition(_,/a:name(_), _, /functionCall(/b:qualifiedName(_,_),_)) := ast};
