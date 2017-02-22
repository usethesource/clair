module lang::cpp::ASTgen

import lang::cpp::AST;  
import Type; 
import List;
import String;
import IO;


public str apiGen(str apiName,list[type[value]] ts) {
  map[str,str] emp = ();
  return apiGen(apiName,ts,emp);
}

public void generate()  {
  code = generate("AST", [#Declarator, #DeclSpecifier, #Declaration, #Expression, #Type, #Statement, #Modifier]);
  
  writeFile(|project://clair/src/lang/cpp/internal/AST.java|, code);
}

public str generate(str apiName, list[type[value]] types) {
  allTypes = types;
 
  return   "// This code was generated by lang::cpp::ASTgen
           'package lang.cpp.internal;
           '
           'import org.rascalmpl.value.type.Type;
           'import org.rascalmpl.value.type.TypeFactory;
           'import org.rascalmpl.value.type.TypeStore;
           'import org.rascalmpl.value.*;
           'import java.util.Map;
           'import java.util.HashMap;
           '
           '@SuppressWarnings(\"deprecation\")
           'public class <apiName> {
           '  private static TypeStore typestore = new TypeStore();
           '  private static TypeFactory tf = TypeFactory.getInstance();
           '  private IValueFactory vf;
           '
           '  public <apiName> (IValueFactory vf) {
           '    this.vf = vf;
           '  }
           '
           '  <for(type[value] t <- allTypes, adt(name, _) := t.symbol) {>private static final Type _<name> = tf.abstractDataType(typestore, \"<name>\");
           '  <}> 
           '  <for(type[value] t <- allTypes) {>
           '  <declareType(t.symbol, t.definitions[t.symbol])>
           '  <}>
           '  <for(type[value] t <- allTypes, t.symbol in t.definitions, choice(_,cs) := t.definitions[t.symbol]) {> 
           '  <declareMakers(t.symbol,cs)> <}>
           '  
           '}";

}

str declareType(adt(name, list[Symbol] _), Production choice) 
 =   "<for(Production c <- choice.alternatives ) {>
     '<declareConstructor(c, name)><}>";
  
  
str declareConstructor(Production::cons(label(str cname, Symbol _), list[Symbol] args, list[Symbol] kwTypes, set[Attr] _), str typeName) 
  = "private static final Type _<typeName>_<cname>_<size(args)> 
    '  = tf.constructor(typestore,_<typeName>,\"<cname>\"<typeNameTuples2FactoryCallArgs(args)>);";
  
str type2FactoryCall(Symbol t){
    switch(t){
      case Symbol::\value() : return "tf.valueType()";
      case Symbol::\void() : return "tf.voidType()";
      case Symbol::\int() :  return "tf.integerType()"; 
      case Symbol::\rat() : return "tf.rationalType()";
      case Symbol::\num() : return "tf.numberType()";
      case Symbol::\real() : return "tf.realType()";
      case Symbol::\bool() : return "tf.boolType()";
      case Symbol::\str() :  return "tf.stringType()"; 
      case Symbol::\loc() : return "tf.sourceLocationType()";
      case Symbol::\datetime() : return "tf.dateTimeType()";
      case Symbol::\node() : return "tf.nodeType()";
      case Symbol::\cons(Symbol::\adt(name,_),_,_) : return  (name);
      case Symbol::\set(ti) :  return "tf.setType(<type2FactoryCall(ti)>)";  
      case Symbol::\list(ti) :  return "tf.listType(<type2FactoryCall(ti)>)";
      case Symbol::\map(label(l1,ti),label(l2, ti2)) : return "tf.mapType(<type2FactoryCall(ti)>,\"<l1>\", <type2FactoryCall(ti2)>, \"<l2>\")";
      case Symbol::\map(ti,ti2) : return "tf.mapType(<type2FactoryCall(ti)>,<type2FactoryCall(ti2)>)";
      case Symbol::\tuple(tis) : return "tf.tupleType(<typeList2FactoryVarArgs(tis)>)";
      case Symbol::\rel(tis) : return "tf.relType(<typeList2FactoryVarArgs(tis)>)";
      case Symbol::\adt(name, _) : return "_<name>";
      default: 
        throw "Do not now how to construct <t>";  
    }
}
  
  str typeList2FactoryVarArgs(list[Symbol] tss){
    if (tss == []) { return "";}
    else { return toExtraArgs([ type2FactoryCall(t) | t <- tss]); }
  }
  
  str typeList2FactoryVarArgsFirstPos(list[Symbol] tss){
    return intercalate(",",[ type2FactoryCall(t) | t <- tss]);
  }
  
  str toExtraArgs(list[str] strs) =
    ("" | "<it>,<s>" | s <- strs);
  
  
  str typeNameTuples2FactoryCallArgs(list[Symbol] args) {
    return toExtraArgs([type2FactoryCall(t),"\"" + n + "\"" | label(n,t) <- args]);
  } 
  
  str declareGetters(Symbol t, set[Production] cs){
    if(adt(name, ps) := t){
      return   "<for(c <- cs) {><declareConstructorGetters(c,name)><}>";
    } 
    // throw "Cannot declare getters for type <t>";
    return ""; 
  }
  
  str declareMakers(adt(str name, list[Symbol] _), set[Production] cs) 
     = "<for (c <- cs) {>
       '<declareMaker(c)>
       '<}>";
  
  bool hasDecl("Declarator", str _) = true;
  bool hasDecl("DeclSpecifier", "declSpecifier") = false;
  bool hasDecl("DeclSpecifier", str _) = true;
  bool hasDecl("Declaration", str cname)
    = cname in {"enumerator", "usingDirective", "sttClass", "sttTypename", "tttParameter", "baseSpecifier", "namespaceDefinition", "usingDeclaration", "namespaceAlias", "alias"};
  bool hasDecl("Expression", str cname)
    = cname in {"qualifiedName", "idExpression", "fieldReference", "templateId", "constructorChainInitializer", "capture", "captureByRef"};
  bool hasDecl("Statement", str cname) = cname in {"label", "goto"};
  bool hasDecl(str _, str _) = false;
  
  bool hasTyp("Expression", str cname) = cname in
    {"arraySubscriptExpression", "multiply", "divide", "modulo", "plus", "minus", "shiftLeft",
    "shiftRight", "lessThan", "greaterThan", "lessEqual", "greaterEqual", "binaryAnd",
    "binaryXor", "binaryOr", "logicalAnd", "logicalOr", "assign", "multiplyAssign",
    "divideAssign", "moduloAssign", "plusAssign", "minusAssign", "shiftLeftAssign",
    "shiftRightAssign", "binaryAndAssign", "binaryXorAssign", "binaryOrAssign", "equals",
    "notEquals", "pmDot", "pmArrow", "max", "min", "ellipses",
    "prefixIncr", "prefixDecr", "plus", "minus", "star", "amper", "tilde", "not", "sizeof", "postfixIncr",
    "postfixDecr", "bracketed", "throw", "typeid", "alignOf", "sizeofParameterPack", "noexcept", "labelReference",
    "functionCall", "fieldReference", "expressionList", "conditional",
    "cast", "dynamicCast", "staticCast", "reinterpretCast", "constCast", "idExpression"
    };
  default bool hasTyp(str _, str _) = false;
  
  str declareMaker(Production::cons(label(str cname, Symbol typ:adt(str typeName, list[Symbol] ps)), list[Symbol] args, list[Symbol] kwTypes,set[Attr] _)) 
     = "public <typeToJavaType(typ)> <typeName>_<cname>(<(declareConsArgs(args)+", ISourceLocation $loc"+(hasDecl(typeName, cname)?", ISourceLocation $decl":"")+(hasTyp(typeName, cname)?", IConstructor $typ":""))[2..]>) {
       '  <for (label(str l, Symbol t) <- args) { str argName = argToSimpleJavaArg(l, t); str argType = type2FactoryCall(t);>  
       '  if (!<argName>.getType().isSubtypeOf(<argType>)) {
       '    throw new IllegalArgumentException(\"Expected \" + <argType> + \" but got \" + <argName>.getType() + \" for <argName>:\" + <argName>);
       '  }
       '  <}>
       '  Map\<String, IValue\> kwParams = new HashMap\<String, IValue\>();
       '  kwParams.put(\"src\", $loc);
       '  <(hasDecl(typeName, cname)?"kwParams.put(\"decl\", $decl);":"")>
       '  <(hasTyp(typeName, cname)?"kwParams.put(\"typ\", $typ);":"")>
       '  return vf.constructor(_<typeName>_<cname>_<size(args)> <callConsArgs(args)>).asWithKeywordParameters().setParameters(kwParams);
       '}";
  
  str declareConsArgs(list[Symbol] args) = "<for (label(str l, Symbol t) <- args) {>, <typeToSimpleJavaType(t)> $<l><}>";
     
  str callConsArgs( list[Symbol] args) = "<for (label(str l, Symbol t) <- args) {>, <argToSimpleJavaArg(l, t)><}>";
  
  str argToSimpleJavaArg(str l, Symbol t) {
    switch(t){
      case \int() : return "vf.integer($<l>)";
      case \real() : return "vf.real($<l>)";
      case \bool() : return "vf.bool($<l>)";
      case \str() :  return "vf.string($<l>)";
      default : return "$<l>";
    }
  }
  
  str typeToSimpleJavaType(Symbol t){
    switch(t){
      case \int() : return "int";
      case \real() : return "double";
      case \num() : return "double";
      case \bool() : return "boolean";
      case \str() :  return "String";
      case \label(_, x) : return typeToSimpleJavaType(x);
      default : return typeToJavaType(t);
    }
  }
  
  str javaResult(Symbol t, str access){
    switch(t){
      case \int() : return "((IInteger)<access>).intValue()";
      case \real() : return "((IReal)<access>).doubleValue()";
      case \num() : return "<access> instanceof IInteger ? (double)((IInteger)<access>).intValue() : ((IReal)<access>).doubleValue()";
      case \bool() : return "((IBool)<access>).getValue()";
      case \str() :  return "((IString)<access>).getValue()";
      case \label(_,x) : return javaResult(x, access);
      default : return "(<typeToJavaType(t)>)<access>";
    }
  }
  
  str typeToJavaType(Symbol t){
    str result ;
    
    switch(t){
      case \adt(_,_) : result =  "IConstructor";
      case \cons(_,_,_) : result =  "IConstructor";
      case \int() : result =  "IInteger";
      case \real() : result =  "IReal";
      case \num() : result =  "INumber";
      case \bool() : result =  "IBool";
      case \list(_) : result =  "IList";
      case \map(_,_) : result =  "IMap";
      case \rel(_) : result =  "IRelation";
      case \set(_) : result =  "ISet";
      case \loc() : result =  "ISourceLocation";
      case \str() :  result =  "IString";
      case \datetime() : result =  "IDateTime";
      case \tuple(_) : result =   "ITuple";     
      case \func(returnType, args): result = "ICallableValue";
      case \alias(_,_,a) : result = typeToJavaType(a);
      default : result = "IValue";
    }
    
    return result;
  }
    
