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
