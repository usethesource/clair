module lang::cpp::ST

import IO;
import List;
import Map;
import Node;
import Type;
import util::Math;

data MyList[&T] = lval(list[&T] elts, list[str] seps = [], str sep = "");

data SDeclarator(MyList[SAttribute] attributes = lval([]), loc src = |unknown:///|, loc decl = |unknown:///|, list[str] seps = [])
    = \declarator(MyList[SDeclaration] pointerOperators, SName name)
    | \declarator(MyList[SDeclaration] pointerOperators, SName name, SExpression initializer)
    | \fieldDeclarator(MyList[SDeclaration] pointerOperators, SName name, SExpression bitFieldSize)
    | \fieldDeclarator(MyList[SDeclaration] pointerOperators, SName name, SExpression bitFieldSize, SExpression initializer)
    | \functionDeclarator(MyList[SDeclaration] pointerOperators, SName name, MyList[SDeclaration] parameters)  //superfluous?
    | \functionDeclarator(MyList[SDeclaration] pointerOperators, MyList[SModifier] modifiers, SName name, MyList[SDeclaration] parameters, MyList[SDeclaration] virtSpecifiers)
    | \functionDeclarator(MyList[SDeclaration] pointerOperators, MyList[SModifier] modifiers, SName name, MyList[SDeclaration] parameters, MyList[SDeclaration] virtSpecifiers, SExpression trailingReturnType)
    | \functionDeclaratorNested(MyList[SDeclaration] pointerOperators, MyList[SModifier] modifiers, SDeclarator declarator, MyList[SDeclaration] parameters, MyList[SDeclaration] virtSpecifiers)
    | \functionDeclaratorNested(MyList[SDeclaration] pointerOperators, MyList[SModifier] modifiers, SDeclarator declarator, MyList[SDeclaration] parameters, MyList[SDeclaration] virtSpecifiers, SExpression initializer)
    | \functionDeclaratorNoexcept(MyList[SDeclaration] pointerOperators, MyList[SModifier] modifiers, SName name, MyList[SDeclaration] parameters, MyList[SDeclaration] virtSpecifiers, SExpression noexceptExpression)
    | \functionDeclaratorWithES(MyList[SDeclaration] pointerOperators, MyList[SModifier] modifiers, SName name, MyList[SDeclaration] parameters, MyList[SDeclaration] virtSpecifiers) //empty exception specification
    | \functionDeclaratorWithES(MyList[SDeclaration] pointerOperators, MyList[SModifier] modifiers, SName name, MyList[SDeclaration] parameters, MyList[SDeclaration] virtSpecifiers, MyList[SExpression] exceptionSpecification)
    | \arrayDeclarator(MyList[SDeclaration] pointerOperators, SName name, MyList[SExpression] arrayModifier)
    | \arrayDeclarator(MyList[SDeclaration] pointerOperators, SName name, MyList[SExpression] arrayModifier, SExpression initializer)
    | \arrayDeclaratorNested(MyList[SDeclaration] pointerOperators, SDeclarator declarator, MyList[SExpression] arrayModifier)
    | \arrayDeclaratorNested(MyList[SDeclaration] pointerOperators, SDeclarator declarator, MyList[SExpression] arrayModifier, SExpression initializer)
    
    //quick fix
    | \missingDeclarator() //no attributes
    ;
    
data SDeclSpecifier(MyList[SAttribute] attributes = lval([]), loc src = |unknown:///|, list[str] seps = [])
    = \declSpecifier(MyList[SModifier] modifiers, SType \type)
    | \declSpecifier(MyList[SModifier] modifiers, SType \type, SExpression expression) //decltype and type_of
    | \etsEnum(MyList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    | \etsStruct(MyList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //ElaboratedTypeSpecifier //no attributes
    | \etsUnion(MyList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    | \etsClass(MyList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    | \namedTypeSpecifier(MyList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    
    | \struct(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] members, loc decl = |unknown:///|)  //c //no attributes
    | \union(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] members, loc decl = |unknown:///|)   //c //no attributes
    | \class(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] members, loc decl = |unknown:///|)   //c //no attributes
    | \struct(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] baseSpecifiers, MyList[SDeclaration] members, loc decl = |unknown:///|)
    | \union(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] baseSpecifiers, MyList[SDeclaration] members, loc decl = |unknown:///|)
    | \class(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] baseSpecifiers, MyList[SDeclaration] members, loc decl = |unknown:///|)
    | \structFinal(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] baseSpecifiers, MyList[SDeclaration] members, loc decl = |unknown:///|)
    | \unionFinal(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] baseSpecifiers, MyList[SDeclaration] members, loc decl = |unknown:///|)
    | \classFinal(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] baseSpecifiers, MyList[SDeclaration] members, loc decl = |unknown:///|)
    
    | \enum(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] enumerators, loc decl = |unknown:///|)
    | \enum(MyList[SModifier] modifiers, SDeclSpecifier baseType, SName name, MyList[SDeclaration] enumerators, loc decl = |unknown:///|)
    | \enumOpaque(MyList[SModifier] modifiers, SDeclSpecifier baseType, SName name, loc decl = |unknown:///|)
    | \enumScoped(MyList[SModifier] modifiers, SName name, MyList[SDeclaration] enumerators, loc decl = |unknown:///|)
    | \enumScoped(MyList[SModifier] modifiers, SDeclSpecifier baseType, SName name, MyList[SDeclaration] enumerators, loc decl = |unknown:///|)
    | \enumScopedOpaque(MyList[SModifier] modifiers, SName name, loc decl = |unknown:///|)
    | \enumScopedOpaque(MyList[SModifier] modifiers, SDeclSpecifier baseType, SName name, loc decl = |unknown:///|)
    
     // Non-standard MSVC throw ellipsis
    | \msThrowEllipsis() //no attributes
    ;
    
data SDeclaration(MyList[SAttribute] attributes = lval([]), loc src=|unknown:///|, list[str] seps = [])
    = \translationUnit(MyList[SDeclaration] declarations) //no attributes
    | \simpleDeclaration(SDeclSpecifier declSpecifier, MyList[SDeclarator] declarators)
    | \functionDefinition(SExpression returnSpec, SDeclarator declarator, SStatement body)//? //no attributes
    | \defaultedFunctionDefinition(SDeclSpecifier declSpecifier, MyList[SExpression] memberInitializer, SDeclarator declarator)
    | \deletedFunctionDefinition(SDeclSpecifier declSpecifier, MyList[SExpression] memberInitializer, SDeclarator declarator)
    | \functionDefinition(SDeclSpecifier declSpecifier, SDeclarator declarator, MyList[SExpression] memberInitializer, SStatement body)
    | \functionWithTryBlockDefinition(SDeclSpecifier declSpecifier, SDeclarator declarator, MyList[SExpression] memberInitializers, SStatement sbody, MyList[SStatement] catchHandlers)
    
    | \asmDeclaration(str assembly) //no attributes
    
    | \enumerator(SName name, SExpression \value, loc decl = |unknown:///|) //no attributes
    | \enumerator(SName name, loc decl = |unknown:///|) //no attributes
    
    | \usingDirective(SName qualifiedName, loc decl = |unknown:///|)
    | \visibilityLabel(SModifier visibility) //no attributes
    
    //| \etsEnum(Name name)
    //| \etsStruct(Name name) //ElaboratedTypeSpecifier
    //| \etsUnion(Name name)
    //| \etsClass(Name name)
    
    | \pointer(MyList[SModifier] modifiers)    // *
    | \pointerToMember(MyList[SModifier] modifiers, SName nestedName)
    | \reference()  // &
    | \rvalueReference() // &&
    
    | \parameter(SDeclSpecifier declSpecifier) //no attributes
    | \parameter(SDeclSpecifier declSpecifier, SDeclarator declarator) //no attributes
    
    //| \declSpecifier(MyList[SModifier] modifiers, Type \type)
    //| \declSpecifier(MyList[SModifier] modifiers, Type \type, Expression expression) //decltype and type_of
    //| \initializerClause(Expression expression) Unneeded layer of abstraction?
    //| \initializerList(MyList[SExpression] clauses)
    
    //| \declarationEqualsInitializer(str name, Expression initializer) //weg //Que?
    
    | \template(MyList[SDeclaration] parameters, SDeclaration declaration, STypeSymbol \type) //no attributes
    | \sttClass(SName name, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes
    | \sttTypename(SName name, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes
    | \sttClass(SName name, SExpression defaultType, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes    
    | \sttTypename(SName name, SExpression defaultType, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes
    
    | \tttParameter(MyList[SDeclaration] nestedParameters, SName name, loc decl = |unknown:///|) //templatedTypeTemplateParameter //no attributes
    
    | \baseSpecifier(MyList[SModifier] modifiers, loc decl = |unknown:///|) //no attributes
    | \baseSpecifier(MyList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    
    | \virtSpecifier(SModifier modifier) //no attributes
    
    | \namespaceDefinition(SName name, MyList[SDeclaration] declarations, loc decl = |unknown:///|)
    | \namespaceDefinitionInline(SName name, MyList[SDeclaration] declarations, loc decl = |unknown:///|)
    | \usingDeclaration(MyList[SModifier] modifiers, SName name, loc decl = |unknown:///|)
    | \namespaceAlias(SName \alias, SName mapping, loc decl = |unknown:///|) //no attributes
    
    | \linkageSpecification(str literal, MyList[SDeclaration] declarations) //no attributes
    | \alias(SName \alias, SExpression mappingTypeId, loc decl = |unknown:///|)
    
    | \staticAssert(SExpression condition, SExpression message) //no attributes
    
    | \explicitTemplateInstantiation(SDeclaration declaration) //no attributes
    | \explicitTemplateInstantiation(SModifier modifier, SDeclaration declaration) //no attributes
    | \explicitTemplateSpecialization(SDeclaration declaration) //no attributes
    
    | \varArgs() //encoding for ellipsis in f(x, ...); //no attributes
    
    | \problemDeclaration() //no attributes
    ;


data SExpression(loc src = |unknown:///|, STypeSymbol typ = \unresolved(), list[str] seps = []) //no attributes
    = \multiply(SExpression lhs, SExpression rhs)
    | \divide(SExpression lhs, SExpression rhs)
    | \modulo(SExpression lhs, SExpression rhs)
    | \plus(SExpression lhs, SExpression rhs)
    | \minus(SExpression lhs, SExpression rhs)
    | \shiftLeft(SExpression lhs, SExpression rhs)
    | \shiftRight(SExpression lhs, SExpression rhs)
    | \lessThan(SExpression lhs, SExpression rhs)
    | \greaterThan(SExpression lhs, SExpression rhs)
    | \lessEqual(SExpression lhs, SExpression rhs)
    | \greaterEqual(SExpression lhs, SExpression rhs)
    | \binaryAnd(SExpression lhs, SExpression rhs)
    | \binaryXor(SExpression lhs, SExpression rhs)
    | \binaryOr(SExpression lhs, SExpression rhs)
    | \logicalAnd(SExpression lhs, SExpression rhs)
    | \logicalOr(SExpression lhs, SExpression rhs)
    | \assign(SExpression lhs, SExpression rhs)
    | \multiplyAssign(SExpression lhs, SExpression rhs)
    | \divideAssign(SExpression lhs, SExpression rhs)
    | \moduloAssign(SExpression lhs, SExpression rhs)
    | \plusAssign(SExpression lhs, SExpression rhs)
    | \minusAssign(SExpression lhs, SExpression rhs)
    | \shiftLeftAssign(SExpression lhs, SExpression rhs)
    | \shiftRightAssign(SExpression lhs, SExpression rhs)
    | \binaryAndAssign(SExpression lhs, SExpression rhs)
    | \binaryXorAssign(SExpression lhs, SExpression rhs)
    | \binaryOrAssign(SExpression lhs, SExpression rhs)
    | \equals(SExpression lhs, SExpression rhs)
    | \notEquals(SExpression lhs, SExpression rhs)
    | \pmDot(SExpression lhs, SExpression rhs) //c++ only //required decl?
    | \pmArrow(SExpression lhs, SExpression rhs) //c++ only //requires decl?
    | \max(SExpression lhs, SExpression rhs) //g++ only
    | \min(SExpression lhs, SExpression rhs) //g++ only
    | \ellipses(SExpression lhs, SExpression rhs) //g++ only
    
    | \prefixIncr(SExpression expression)    //++exp
    | \prefixDecr(SExpression expression)    //--exp
    | \plus(SExpression expression)          //+exp
    | \minus(SExpression expression)         //-exp
    | \star(SExpression expression)          //*exp
    | \amper(SExpression expression)         //&exp
    | \tilde(SExpression expression)         //~exp
    | \not(SExpression expression)           //!exp
    | \sizeof(SExpression expression)        //sizeof exp
    | \postfixIncr(SExpression expression)   //exp++
    | \postfixDecr(SExpression expression)   //exp--
    | \bracketed(SExpression expression)     //(exp)
    | \throw()
    | \throw(SExpression expression)         //throw exp, c++ only
    | \typeid(SExpression expression)        //typeid exp, c++ only
    //| \typeof(Expression expression)        //typeof exp, deprecated
    | \alignOf(SExpression expression)       //__alignOf(exp), gcc only
    | \sizeofParameterPack(SExpression expression) //sizeof...(parameterpack), c++ only?
    | \noexcept(SExpression expression)      //noexcept (exp), c++ only
    | \labelReference(SExpression expression)//&&label, gcc only?
    
    | \cast(SExpression typeId, SExpression expression)
    | \dynamicCast(SExpression typeId, SExpression expression)
    | \staticCast(SExpression typeId, SExpression expression)
    | \reinterpretCast(SExpression typeId, SExpression expression)
    | \constCast(SExpression typeId, SExpression expression)
    
    | \idExpression(SName name, loc decl = |unknown:///|)
    | \integerLiteral(int number)
    | \conditional(SExpression condition, SExpression positive, SExpression negative)
    
    | \integerConstant(str \value)
    | \floatConstant(str \value)
    | \charConstant(str \value)
    | \stringLiteral(str \value)
    | \this()
    | \true()
    | \false()
    | \nullptr()
    
    //| \namedTypeSpecifier(MyList[SModifier] modifiers, Name name)
    
    | \functionCall(SExpression functionName, MyList[SExpression] arguments)
    
    | \fieldReference(SExpression fieldOwner, SName name, loc decl = |unknown:///|)
    | \fieldReferencePointerDeref(SExpression fieldOwner, SName name, loc decl = |unknown:///|)
    | \new(SExpression typeId)
    | \new(SExpression typeId, SExpression initializer)
    | \newWithArgs(MyList[SExpression] arguments, SExpression typeId)
    | \newWithArgs(MyList[SExpression] arguments, SExpression typeId, SExpression initializer)
    | \globalNew(SExpression typeId)
    | \globalNew(SExpression typeId, SExpression initializer)
    | \globalNewWithArgs(MyList[SExpression] arguments, SExpression typeId)
    | \globalNewWithArgs(MyList[SExpression] arguments, SExpression typeId, SExpression initializer)
    
    | \delete(SExpression expression)
    | \vectoredDelete(SExpression expression)
    | \globalDelete(SExpression expression)
    | \globalVectoredDelete(SExpression expression)
    
    | \arraySubscriptExpression(SExpression array, SExpression argument)
    | \arrayModifier(MyList[SAttribute] attributes = lval([]))
    | \arrayModifier(SExpression constExpression, MyList[SAttribute] attributes = lval([]))
    
    | \simpleTypeConstructor(SDeclSpecifier declSpecifier, SExpression initializer)
    
    | \expressionList(MyList[SExpression] expressions)
    
    | \compoundStatementExpression(SStatement compoundStatement)
    
    | \empty()    
    | \nyi(str raw)
    
    | \lambda(SModifier captureDefault, MyList[SExpression] captures, SDeclarator declarator, SStatement body)
    
    | \packExpansion(SExpression pattern)
    
    | \typeIdInitializerExpression(SExpression typeId, SExpression initializer)
    
    // TypeId below
    | \typeId(SDeclSpecifier declSpecifier)
    | \typeId(SDeclSpecifier declSpecifier, SDeclarator abstractDeclarator)
    
    // Initializers below
    | \equalsInitializer(SExpression initializer)
    | \initializerList(MyList[SExpression] clauses) //initializerClause?
    | \constructorChainInitializer(SName name, SExpression initializer, loc decl = |unknown:///|)
    | \constructorInitializer(MyList[SExpression] arguments)
    
    // DesignatedInitializers below
    | \designatedInitializer(MyList[SExpression] designators, SExpression operand)
    
    // Designators below
    | \arrayDesignator(SExpression subscript)
    | \fieldDesignator(SExpression fieldName)
    | \arrayRangeDesignator(SExpression rangeFloor, SExpression rangeCeiling) //gcc-only
    
    // Captures
    | \capture(SName name, loc decl = |unknown:///|)
    | \captureByRef(SName name, loc decl = |unknown:///|)
    | \captureThisPtr()
    
    | \problemExpression()
    
    ;                       
 
data SName(loc src = |unknown:///|, list[str] seps = []) //no attributes
    = \name(str \value)
    | \qualifiedName(MyList[SName] qualifiers, SName lastName, loc decl = |unknown:///|)
    | \operatorName(str \value)
    | \conversionName(str \value, SExpression typeId)
    
    | \templateId(SName name, MyList[SExpression] argumentTypes, loc decl = |unknown:///|)

    | \abstractEmptyName()
    ;
 
data SStatement(MyList[SAttribute] attributes = lval([]), loc src = |unknown:///|, list[str] seps = [])
    = \compoundStatement(MyList[SStatement] statements)
    | \declarationStatement(SDeclaration declaration)
    | \expressionStatement(SExpression expression)
    | \if(SExpression condition, SStatement thenClause)
    | \if(SExpression condition, SStatement thenClause, SStatement elseClause)
    | \ifWithDecl(SDeclaration conditionDeclaration, SStatement thenClause)
    | \ifWithDecl(SDeclaration conditionDeclaration, SStatement thenClause, SStatement elseClause)
    | \for(SStatement sInitializer, SExpression condition, SExpression iteration, SStatement body)
    | \forWithDecl(SStatement sInitializer, SDeclaration conditionDeclaration, SExpression iteration, SStatement body)
    | \rangeBasedFor(SDeclaration declaration, SExpression initializer, SStatement body)
    | \switch(SExpression controller, SStatement body)
    | \switchWithDecl(SDeclaration controllerDeclaration, SStatement body)
    | \case(SExpression expression)
    | \defaultCase()
    | \break()
    | \while(SExpression condition, SStatement body)
    | \whileWithDecl(SDeclaration conditionDeclaration, SStatement body)
    | \continue()
    | \do(SStatement body, SExpression condition)
    
    | \return(SExpression expression) //note: also with initializerClause, which currently is an Expression
    | \return()
    | \nullStatement()
    | \label(SName name, SStatement nestedStatement, loc decl = |unknown:///|)
    | \goto(SName name, loc decl = |unknown:///|)
    
    | \tryBlock(SStatement tryBody, MyList[SStatement] catchHandlers)
    | \catch(SDeclaration declaration, SStatement body)
    | \catchAll(SStatement body)    
    
    | \problem(str raw) //no attributes
    ;           
  
data SType(loc src = |unknown:///|, list[str] seps = []) //no attributes
    = \unspecified()
    | \void()
    | \char()
    | \integer()
    | \float()
    | \double()
    | \bool()
    | \wchar_t()
    | \typeof()     //needs an argument?
    | \decltype()   //needs an argument?
    | \auto()       //is this a type?
    | \char16_t()
    | \char32_t()
    | \int128()
    | \float128()
    | \decimal32()
    | \decimal64()
    | \decimal128()
    
    
    | \arrayType(SType \type, int size)
    | \basicType(SType \type, MyList[SModifier] modifiers)
    | \nullptr()
    
    | \structType(SName name)
    | \unionType(SName name)
    | \classType(SName name)

    ;
  
data SModifier(loc src = |unknown:///|, list[str] seps = []) //no attributes
    = typedef()
    | \extern()
    | \static()
    | \modAuto()
    | \register()
    | \mutable()
    
    | \public()
    | \protected()
    | \private()
    | \unspecifiedInheritance()
    
    | \signed()
    | \unsigned()
    | \short()
    | \long()
    | \longlong()
    | \complex()
    | \imaginary()
    
    | \const()
    | \volatile()
    | \restrict()
    | \inline()
    
    | \final()
    | \override()
    
    | \friend()
    | \virtual()
    | \explicit()
    | \constexpr()
    | \threadLocal()
    | \pureVirtual()
    
    | \typename()
    
    | \captDefUnspecified()
    | \captDefByCopy()
    | \captDefByReference()
    ;

data SAttribute(list[str] seps = []) //no attributes
    = \attribute(str name)
    | \attribute(str name, str argumentClause)
    | \attributeSpecifier(MyList[SAttribute] attributes)
    | \alignmentSpecifier(SExpression typeIdOrExpression)
	;
	
data Bound = \unbounded();

public data STypeSymbol
  = \unspecified()
  | \void()
  | \char()
  | \wchar()
  | \int()
  | \float()
  | \double()
  | \boolean()
  | \char16()
  | \char32()
  | \nullPtr()
  | \int128()
  | \float128()
  | \decimal32()
  | \decimal64()
  | \decimal128()
  
  | \array(STypeSymbol baseType)
  | \array(STypeSymbol baseType, int size)
  | \basicType(MyList[STypeModifier] modifiers, STypeSymbol baseType)
  | \class(loc decl)
  | \union(loc decl)
  | \struct(MyList[STypeSymbol] fields)
  | \qualifierType(MyList[STypeModifier] modifiers, STypeSymbol \type)
  | \pointerType(MyList[STypeModifier] modifiers, STypeSymbol \type)
  | \functionType(STypeSymbol returnType, MyList[STypeSymbol] parameterTypes)
  | \functionTypeVarArgs(STypeSymbol returnType, MyList[STypeSymbol] parameterTypes)
  | \typeContainer(STypeSymbol \type)
  | \typedef(STypeSymbol \type)
  | \enumeration(loc decl)
  | \referenceType(STypeSymbol \type)
  | \parameterPackType(STypeSymbol \type)
  
  | \classSpecialization(loc decl, MyList[STypeSymbol] templateArguments)
  | \enumerationSpecialization(loc specializedBinding, MyList[STypeSymbol] templateArguments)
  
  | \templateTypeParameter(loc owner, loc decl)
  | \implicitTemplateTypeParameter(loc owner, int position) //no decl?
  | \deferredClassInstance(str name)
  | \unknownMemberClass(loc owner, str name)
  
  | \typeOfDependentExpression(loc src)
  | \problemBinding()
  | \problemType(str msg)
  | \noType()
  
  | \cStructTemplate(loc decl, MyList[loc] templateParameters)
  | \cUnionTemplate(loc decl, MyList[loc] templateParameters)
  | \cClassTemplate(loc decl, MyList[loc] templateParameters)
  | \eStructTemplate(loc decl, MyList[loc] templateParameters)
  | \eUnionTemplate(loc decl, MyList[loc] templateParameters)
  | \eClassTemplate(loc decl, MyList[loc] templateParameters)
  | \eEnumTemplate(loc decl, MyList[loc] templateParameters)
  | \templateTemplate(STypeSymbol child, MyList[loc] templateParameters)
  | \functionTemplate(loc decl, MyList[loc] templateParameters)
  | \variableTemplate(loc decl, MyList[loc] templateParameters)
  
  | \aliasTemplate(loc decl, MyList[loc] templateParameters)
  
  | \functionSetType(loc decl, MyList[STypeSymbol] templateArguments)
  | \functionSetTypePointer(loc decl, MyList[STypeSymbol] templateArguments)
  
  | \unresolved()
  | \any()
  
  ;
  
public data STypeModifier
  = \signed()
  | \unsigned()
  | \short()
  | \long()
  | \longlong()
  | \complex()
  | \imaginary()
  
  | \static()
  | \const()
  | \volatile()
  | \restrict()
  
  ;
  
node toST(node tree) = toST(tree, ());
node toST(node tree, map[loc,str] sourceCache) {
  source = readSrc(asLoc(tree.src), sourceCache);
  return toST(tree, source, sourceCache);
}

node toST(node tree, str src) = toST(tree, src, ());
node toST(node tree, str src, map[loc,str] srcCache) {
  return addSeps(wrapLists(tree), srcCache);
}

node wrapLists(node ast) {
  children = for (child <- getChildren(ast)) {
    if ([*elts] := child) {
      append lval([wrapLists(elt) | elt <- elts]);
    } else {
      append wrapLists(child);
    }
  }
  parameters = getKeywordParameters(ast);
  params = ();
  for (key <- parameters, val := parameters[key]) {
    if ([*elts] := val) {
      params += (key : lval([wrapLists(elt) | elt <- elts]));
    } else {
      params += (key : wrapLists(val));
    }
  }
  assert size(parameters) == size(params);
  
  switch(typeOf(ast)) {
    case adt("Declaration",[]) : return make(#SDeclaration, getName(ast), children, params);
    case adt("Declarator",[]) : return make(#SDeclarator, getName(ast), children, params);
    case adt("DeclSpecifier",[]) : return make(#SDeclSpecifier, getName(ast), children, params);
    case adt("Statement",[]) : return make(#SStatement, getName(ast), children, params);
    case adt("Expression",[]) : return make(#SExpression, getName(ast), children, params);
    case adt("Name",[]) : return make(#SName, getName(ast), children, params);
    case adt("Modifier",[]) : return make(#SModifier, getName(ast), children, params);
    case adt("Type",[]) : return make(#SType, getName(ast), children, params);
    case adt("TypeSymbol",[]) : return make(#STypeSymbol, getName(ast), children, params);
    case adt("TypeModifier",[]) : return make(#STypeModifier, getName(ast), children, params);
    default: {
      println("FOO");
      throw "Missed adt <typeOf(ast)>";
    }
  }
}
str wrapLists(str s) = s;
loc wrapLists(loc l) = l;
//maybe: default &T wrapLists(&T val) = val;

loc asLoc(value v) {
  if (loc l := v) {
    return l;
  }
  throw "Impossible";
}

loc getLoc(node n) {
  if (lval(elts) := n) {
    loc first = getLoc(elts[0]);
    loc last = getLoc(elts[-1]);
    return first[length=last.offset+last.length-first.offset];
  }
  if (loc l := n.src) {
    return l;
  }
  throw "Impossible";
}

list[&T] repeat(&T what, int times) = ([what] | it + what | i <- [1..times]);

default &T <: node addSeps(&T <: node tree, map[loc,str] srcCache) {
  srcLoc = asLoc(tree.src);
  code = readSrc(srcLoc, srcCache);
  offset = srcLoc.offset;
  children = getChildren(tree);
  newChildren = [];
  seps = [];
  if (children == []) {
    seps += code;
    newChildren = [];
  } else if ([str s] := children) {
    seps = ["",""];
    newChildren = children;
  } else {
    i = 0;
    while (i < size(children) && lval([]) := children[i]) {//find cutoff point for "prefix"
      i = i + 1;
    }
    if (i == size(children)) {//what to do if all children are lval([])?
      seps = [code] + repeat("", size(children)-1);
    } else {
      seps += code[..getLoc(children[i]).offset-offset];//"prefix"
      m = max(0,size(children)-1);
      for (j <- [0..m]) {//add separators between children
        if (lval([]) := children[j]) {
          seps += "";
          continue;
        }
        firstLoc = getLoc(children[j]);
        k = 1;
        while (j+k < size(children) && lval([]) := children[j+k]) {
          k = k + 1;
        }
        if (j+k < size(children)) {
          secondLoc = getLoc(children[j+k]);
          seps += code[firstLoc.offset-offset+firstLoc.length..secondLoc.offset-offset];
        }
      }
      if (lval([]) := children[-1]) {//TODO: Check
        seps += "";
      }
    
      assert size(children) == size(seps);//TODO: REMOVE
    
      i = size(children) - 1;//find cutoff point for "postfix"
      while (lval([]) := children[i] && i >= 0) {
        i = i - 1;
      }
      if (i == -1) {//what to do if all children are lval([])?
        throw "FIXME";
      }
      lastLoc = getLoc(children[i]);
      seps += code[lastLoc.offset+lastLoc.length-offset..];//"postfix"
    }
  }
  newChildren = [addSeps(child, srcCache) | child <- children];
  params = getKeywordParameters(tree) + ("seps":seps);
  switch(typeOf(tree)) {
    case adt("SDeclaration",[]) : return make(#SDeclaration, getName(tree), newChildren, params);
    case adt("SDeclarator",[]) : return make(#SDeclarator, getName(tree), newChildren, params);
    case adt("SDeclSpecifier",[]) : return make(#SDeclSpecifier, getName(tree), newChildren, params);
    case adt("SStatement",[]) : return make(#SStatement, getName(tree), newChildren, params);
    case adt("SExpression",[]) : return make(#SExpression, getName(tree), newChildren, params);
    case adt("SName",[]) : return make(#SName, getName(tree), newChildren, params);
    case adt("SModifier",[]) : return make(#SModifier, getName(tree), newChildren, params);
    case adt("SType",[]) : return make(#SType, getName(tree), newChildren, params);
    default: throw "Missed adt <typeOf(tree)>";
  }
}

str readSrc(loc l, map[loc,str] sourceCache) {
  if (l.scheme == "cache") {
    return sourceCache[l.top][l.offset..l.offset+l.length];
  } else {
    return readFile(l);
  }
}

str readBetween(node before, node after, map[loc,str] sourceCache) {
  if (loc beforeLoc := before.src && loc afterLoc := after.src) {
    return readSrc(beforeLoc[offset=beforeLoc.offset+beforeLoc.length][length=afterLoc.offset-beforeLoc.offset-beforeLoc.length], sourceCache);
  }
  throw "Impossible";
}

MyList[&T] addSeps(MyList[&T] lst, map[loc,str] sourceCache) {
  if (lst := lval([])) {
    return lst[seps=[]];
  }
  seps = for (i <- [0..size(lst.elts)-1]) {
    append readBetween(lst.elts[i], lst.elts[i+1], sourceCache);
  }
  elts = [addSeps(elt, sourceCache) | elt <- lst.elts];
  return lst[elts=elts][seps=seps];
}

loc addSeps(loc l, map[loc,str] _) = l;
str addSeps(str s, map[loc,str] _) = s;

list[str] getSeps(node n) {
  if (list[str] seps := n.seps) {
    return seps;
  }
  throw "Impossible";
}

str yield(str s, bool addIndentation = false) = s;
str yield(node n, bool addIndentation = false) {
  if (addIndentation) {
    println("Adding indentation NYI!");
  }
  if (lval(elts, seps = seps) := n) {
    if (elts == []) {
      return "";
    }
    if ([elt] := elts) {
      return yield(elt);
    }
    return (yield(elts[0]) | it + seps[i-1] + yield(elts[i]) | i <- [1..size(elts)]);
  }
  seps = getSeps(n);
  children = getChildren(n);
  return (seps[0] | it + yield(children[i]) + seps[i+1] | i <- [0..size(children)]);
}