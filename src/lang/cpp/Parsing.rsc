module lang::cpp::Parsing

import lang::cpp::AST;
import IO;
import util::FileSystem;
import Relation;
import Set;
import Map;

set[loc] COMcalls(loc file) = {};

set[loc] listCppFiles(loc root, set[str] exts = {"cpp"}) = {*find(root,ext)|ext<-exts};

map[loc, Declaration] foo(loc root) = (file:bprintln("Parsing <file>") && parseCpp(file)|file <- listCppFiles(root));
