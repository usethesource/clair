module lang::cpp::ST

import IO;
import List;
import Map;
import Node;
import String;
import Type;
import util::Math;

import SST;

data SDeclarator(SepList[SAttribute] attributes = lst([], []), loc src = |unknown:///|, loc decl = |unknown:///|, list[str] seps = [])
    = \declarator(SepList[SDeclaration] pointerOperators, SName name)
    | \declarator(SepList[SDeclaration] pointerOperators, SName name, SExpression initializer)
    | \fieldDeclarator(SepList[SDeclaration] pointerOperators, SName name, SExpression bitFieldSize)
    | \fieldDeclarator(SepList[SDeclaration] pointerOperators, SName name, SExpression bitFieldSize, SExpression initializer)
    | \functionDeclarator(SepList[SDeclaration] pointerOperators, SName name, SepList[SDeclaration] parameters)  //superfluous?
    | \functionDeclarator(SepList[SDeclaration] pointerOperators, SepList[SModifier] modifiers, SName name, SepList[SDeclaration] parameters, SepList[SDeclaration] virtSpecifiers)
    | \functionDeclarator(SepList[SDeclaration] pointerOperators, SepList[SModifier] modifiers, SName name, SepList[SDeclaration] parameters, SepList[SDeclaration] virtSpecifiers, SExpression trailingReturnType)
    | \functionDeclaratorNested(SepList[SDeclaration] pointerOperators, SepList[SModifier] modifiers, SDeclarator declarator, SepList[SDeclaration] parameters, SepList[SDeclaration] virtSpecifiers)
    | \functionDeclaratorNested(SepList[SDeclaration] pointerOperators, SepList[SModifier] modifiers, SDeclarator declarator, SepList[SDeclaration] parameters, SepList[SDeclaration] virtSpecifiers, SExpression initializer)
    | \functionDeclaratorNoexcept(SepList[SDeclaration] pointerOperators, SepList[SModifier] modifiers, SName name, SepList[SDeclaration] parameters, SepList[SDeclaration] virtSpecifiers, SExpression noexceptExpression)
    | \functionDeclaratorWithES(SepList[SDeclaration] pointerOperators, SepList[SModifier] modifiers, SName name, SepList[SDeclaration] parameters, SepList[SDeclaration] virtSpecifiers) //empty exception specification
    | \functionDeclaratorWithES(SepList[SDeclaration] pointerOperators, SepList[SModifier] modifiers, SName name, SepList[SDeclaration] parameters, SepList[SDeclaration] virtSpecifiers, SepList[SExpression] exceptionSpecification)
    | \arrayDeclarator(SepList[SDeclaration] pointerOperators, SName name, SepList[SExpression] arrayModifier)
    | \arrayDeclarator(SepList[SDeclaration] pointerOperators, SName name, SepList[SExpression] arrayModifier, SExpression initializer)
    | \arrayDeclaratorNested(SepList[SDeclaration] pointerOperators, SDeclarator declarator, SepList[SExpression] arrayModifier)
    | \arrayDeclaratorNested(SepList[SDeclaration] pointerOperators, SDeclarator declarator, SepList[SExpression] arrayModifier, SExpression initializer)
    
    //quick fix
    | \missingDeclarator() //no attributes
    ;
    
data SDeclSpecifier(SepList[SAttribute] attributes = lst([],[]), loc src = |unknown:///|, list[str] seps = [])
    = \declSpecifier(SepList[SModifier] modifiers, SType \type)
    | \declSpecifier(SepList[SModifier] modifiers, SType \type, SExpression expression) //decltype and type_of
    | \etsEnum(SepList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    | \etsStruct(SepList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //ElaboratedTypeSpecifier //no attributes
    | \etsUnion(SepList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    | \etsClass(SepList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    | \namedTypeSpecifier(SepList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    
    | \struct(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] members, loc decl = |unknown:///|)  //c //no attributes
    | \union(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] members, loc decl = |unknown:///|)   //c //no attributes
    | \class(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] members, loc decl = |unknown:///|)   //c //no attributes
    | \struct(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] baseSpecifiers, SepList[SDeclaration] members, loc decl = |unknown:///|)
    | \union(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] baseSpecifiers, SepList[SDeclaration] members, loc decl = |unknown:///|)
    | \class(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] baseSpecifiers, SepList[SDeclaration] members, loc decl = |unknown:///|)
    | \structFinal(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] baseSpecifiers, SepList[SDeclaration] members, loc decl = |unknown:///|)
    | \unionFinal(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] baseSpecifiers, SepList[SDeclaration] members, loc decl = |unknown:///|)
    | \classFinal(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] baseSpecifiers, SepList[SDeclaration] members, loc decl = |unknown:///|)
    
    | \enum(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] enumerators, loc decl = |unknown:///|)
    | \enum(SepList[SModifier] modifiers, SDeclSpecifier baseType, SName name, SepList[SDeclaration] enumerators, loc decl = |unknown:///|)
    | \enumOpaque(SepList[SModifier] modifiers, SDeclSpecifier baseType, SName name, loc decl = |unknown:///|)
    | \enumScoped(SepList[SModifier] modifiers, SName name, SepList[SDeclaration] enumerators, loc decl = |unknown:///|)
    | \enumScoped(SepList[SModifier] modifiers, SDeclSpecifier baseType, SName name, SepList[SDeclaration] enumerators, loc decl = |unknown:///|)
    | \enumScopedOpaque(SepList[SModifier] modifiers, SName name, loc decl = |unknown:///|)
    | \enumScopedOpaque(SepList[SModifier] modifiers, SDeclSpecifier baseType, SName name, loc decl = |unknown:///|)
    
     // Non-standard MSVC throw ellipsis
    | \msThrowEllipsis() //no attributes
    ;
    
data SDeclaration(SepList[SAttribute] attributes = lst([],[]), loc src=|unknown:///|, list[str] seps = [])
    = \translationUnit(SepList[SDeclaration] declarations) //no attributes
    | \simpleDeclaration(SDeclSpecifier declSpecifier, SepList[SDeclarator] declarators)
    | \functionDefinition(SExpression returnSpec, SDeclarator declarator, SStatement body)//? //no attributes
    | \defaultedFunctionDefinition(SDeclSpecifier declSpecifier, SepList[SExpression] memberInitializer, SDeclarator declarator)
    | \deletedFunctionDefinition(SDeclSpecifier declSpecifier, SepList[SExpression] memberInitializer, SDeclarator declarator)
    | \functionDefinition(SDeclSpecifier declSpecifier, SDeclarator declarator, SepList[SExpression] memberInitializer, SStatement body)
    | \functionWithTryBlockDefinition(SDeclSpecifier declSpecifier, SDeclarator declarator, SepList[SExpression] memberInitializers, SStatement sbody, SepList[SStatement] catchHandlers)
    
    | \asmDeclaration(str assembly) //no attributes
    
    | \enumerator(SName name, SExpression \value, loc decl = |unknown:///|) //no attributes
    | \enumerator(SName name, loc decl = |unknown:///|) //no attributes
    
    | \usingDirective(SName qualifiedName, loc decl = |unknown:///|)
    | \visibilityLabel(SModifier visibility) //no attributes
    
    //| \etsEnum(Name name)
    //| \etsStruct(Name name) //ElaboratedTypeSpecifier
    //| \etsUnion(Name name)
    //| \etsClass(Name name)
    
    | \pointer(SepList[SModifier] modifiers)    // *
    | \pointerToMember(SepList[SModifier] modifiers, SName nestedName)
    | \reference()  // &
    | \rvalueReference() // &&
    
    | \parameter(SDeclSpecifier declSpecifier) //no attributes
    | \parameter(SDeclSpecifier declSpecifier, SDeclarator declarator) //no attributes
    
    //| \declSpecifier(SepList[SModifier] modifiers, Type \type)
    //| \declSpecifier(SepList[SModifier] modifiers, Type \type, Expression expression) //decltype and type_of
    //| \initializerClause(Expression expression) Unneeded layer of abstraction?
    //| \initializerList(SepList[SExpression] clauses)
    
    //| \declarationEqualsInitializer(str name, Expression initializer) //weg //Que?
    
    | \template(SepList[SDeclaration] parameters, SDeclaration declaration, STypeSymbol \type) //no attributes
    | \sttClass(SName name, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes
    | \sttTypename(SName name, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes
    | \sttClass(SName name, SExpression defaultType, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes    
    | \sttTypename(SName name, SExpression defaultType, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes
    
    | \tttParameter(SepList[SDeclaration] nestedParameters, SName name, loc decl = |unknown:///|) //templatedTypeTemplateParameter //no attributes
    
    | \baseSpecifier(SepList[SModifier] modifiers, loc decl = |unknown:///|) //no attributes
    | \baseSpecifier(SepList[SModifier] modifiers, SName name, loc decl = |unknown:///|) //no attributes
    
    | \virtSpecifier(SModifier modifier) //no attributes
    
    | \namespaceDefinition(SName name, SepList[SDeclaration] declarations, loc decl = |unknown:///|)
    | \namespaceDefinitionInline(SName name, SepList[SDeclaration] declarations, loc decl = |unknown:///|)
    | \usingDeclaration(SepList[SModifier] modifiers, SName name, loc decl = |unknown:///|)
    | \namespaceAlias(SName \alias, SName mapping, loc decl = |unknown:///|) //no attributes
    
    | \linkageSpecification(str literal, SepList[SDeclaration] declarations) //no attributes
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
    
    //| \namedTypeSpecifier(SepList[SModifier] modifiers, Name name)
    
    | \functionCall(SExpression functionName, SepList[SExpression] arguments)
    
    | \fieldReference(SExpression fieldOwner, SName name, loc decl = |unknown:///|)
    | \fieldReferencePointerDeref(SExpression fieldOwner, SName name, loc decl = |unknown:///|)
    | \new(SExpression typeId)
    | \new(SExpression typeId, SExpression initializer)
    | \newWithArgs(SepList[SExpression] arguments, SExpression typeId)
    | \newWithArgs(SepList[SExpression] arguments, SExpression typeId, SExpression initializer)
    | \globalNew(SExpression typeId)
    | \globalNew(SExpression typeId, SExpression initializer)
    | \globalNewWithArgs(SepList[SExpression] arguments, SExpression typeId)
    | \globalNewWithArgs(SepList[SExpression] arguments, SExpression typeId, SExpression initializer)
    
    | \delete(SExpression expression)
    | \vectoredDelete(SExpression expression)
    | \globalDelete(SExpression expression)
    | \globalVectoredDelete(SExpression expression)
    
    | \arraySubscriptExpression(SExpression array, SExpression argument)
    | \arrayModifier(SepList[SAttribute] attributes = lst([],[]))
    | \arrayModifier(SExpression constExpression, SepList[SAttribute] attributes = lst([],[]))
    
    | \simpleTypeConstructor(SDeclSpecifier declSpecifier, SExpression initializer)
    
    | \expressionList(SepList[SExpression] expressions)
    
    | \compoundStatementExpression(SStatement compoundStatement)
    
    | \empty()    
    | \nyi(str raw)
    
    | \lambda(SModifier captureDefault, SepList[SExpression] captures, SDeclarator declarator, SStatement body)
    
    | \packExpansion(SExpression pattern)
    
    | \typeIdInitializerExpression(SExpression typeId, SExpression initializer)
    
    // TypeId below
    | \typeId(SDeclSpecifier declSpecifier)
    | \typeId(SDeclSpecifier declSpecifier, SDeclarator abstractDeclarator)
    
    // Initializers below
    | \equalsInitializer(SExpression initializer)
    | \initializerList(SepList[SExpression] clauses) //initializerClause?
    | \constructorChainInitializer(SName name, SExpression initializer, loc decl = |unknown:///|)
    | \constructorInitializer(SepList[SExpression] arguments)
    
    // DesignatedInitializers below
    | \designatedInitializer(SepList[SExpression] designators, SExpression operand)
    
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
    | \qualifiedName(SepList[SName] qualifiers, SName lastName, loc decl = |unknown:///|)
    | \operatorName(str \value)
    | \conversionName(str \value, SExpression typeId)
    
    | \templateId(SName name, SepList[SExpression] argumentTypes, loc decl = |unknown:///|)

    | \abstractEmptyName()
    ;
 
data SStatement(SepList[SAttribute] attributes = lst([],[]), loc src = |unknown:///|, list[str] seps = [])
    = \compoundStatement(SepList[SStatement] statements)
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
    
    | \tryBlock(SStatement tryBody, SepList[SStatement] catchHandlers)
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
    | \basicType(SType \type, SepList[SModifier] modifiers)
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
    | \attributeSpecifier(SepList[SAttribute] attributes)
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
  | \basicType(SepList[STypeModifier] modifiers, STypeSymbol baseType)
  | \class(loc decl)
  | \union(loc decl)
  | \struct(SepList[STypeSymbol] fields)
  | \qualifierType(SepList[STypeModifier] modifiers, STypeSymbol \type)
  | \pointerType(SepList[STypeModifier] modifiers, STypeSymbol \type)
  | \functionType(STypeSymbol returnType, SepList[STypeSymbol] parameterTypes)
  | \functionTypeVarArgs(STypeSymbol returnType, SepList[STypeSymbol] parameterTypes)
  | \typeContainer(STypeSymbol \type)
  | \typedef(STypeSymbol \type)
  | \enumeration(loc decl)
  | \referenceType(STypeSymbol \type)
  | \parameterPackType(STypeSymbol \type)
  
  | \classSpecialization(loc decl, SepList[STypeSymbol] templateArguments)
  | \enumerationSpecialization(loc specializedBinding, SepList[STypeSymbol] templateArguments)
  
  | \templateTypeParameter(loc owner, loc decl)
  | \implicitTemplateTypeParameter(loc owner, int position) //no decl?
  | \deferredClassInstance(str name)
  | \unknownMemberClass(loc owner, str name)
  
  | \typeOfDependentExpression(loc src)
  | \problemBinding()
  | \problemType(str msg)
  | \noType()
  
  | \cStructTemplate(loc decl, SepList[loc] templateParameters)
  | \cUnionTemplate(loc decl, SepList[loc] templateParameters)
  | \cClassTemplate(loc decl, SepList[loc] templateParameters)
  | \eStructTemplate(loc decl, SepList[loc] templateParameters)
  | \eUnionTemplate(loc decl, SepList[loc] templateParameters)
  | \eClassTemplate(loc decl, SepList[loc] templateParameters)
  | \eEnumTemplate(loc decl, SepList[loc] templateParameters)
  | \templateTemplate(STypeSymbol child, SepList[loc] templateParameters)
  | \functionTemplate(loc decl, SepList[loc] templateParameters)
  | \variableTemplate(loc decl, SepList[loc] templateParameters)
  
  | \aliasTemplate(loc decl, SepList[loc] templateParameters)
  
  | \functionSetType(loc decl, SepList[STypeSymbol] templateArguments)
  | \functionSetTypePointer(loc decl, SepList[STypeSymbol] templateArguments)
  
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
  
  
default &T <: node removeAligners(&T <: node n) {
  if (!(n has seps)) {
    println("?");
    return n;
  }
  oldChildren = getChildren(n);
  list[node] newChildren = [];
  newChildren = for (node child <- oldChildren) {
    append(removeAligners(child));
  }
  newSeps = for (list[str] ss := n.seps, sep <- ss) {
    append(removeAligners(sep));
  }
  switch(typeOf(n)) {
    case adt("SDeclaration",[]) : return make(#SDeclaration, getName(n), newChildren, getKeywordParameters(n) + ("seps" : newSeps));
    case adt("SDeclarator",[]) : return make(#SDeclarator, getName(n), newChildren, getKeywordParameters(n) + ("seps" : newSeps));
    case adt("SDeclSpecifier",[]) : return make(#SDeclSpecifier, getName(n), newChildren, getKeywordParameters(n) + ("seps" : newSeps));
    case adt("SStatement",[]) : return make(#SStatement, getName(n), newChildren, getKeywordParameters(n) + ("seps" : newSeps));
    case adt("SExpression",[]) : return make(#SExpression, getName(n), newChildren, getKeywordParameters(n) + ("seps" : newSeps));
    case adt("SName",[]) : return make(#SName, getName(n), getChildren(n), getKeywordParameters(n) + ("seps" : newSeps));
    case adt("SModifier",[]) : return make(#SModifier, getName(n), newChildren, getKeywordParameters(n) + ("seps" : newSeps));
    case adt("SType",[]) : return make(#SType, getName(n), newChildren, getKeywordParameters(n) + ("seps" : newSeps));
    default: throw "Missed adt <typeOf(n)>";
  }
}

SepList[&T] removeAligners(SepList[&T] sl) {
  newSeps = for (sep <- sl.seps) {
    append(removeAligners(sep));
  }
  if (sl.seps != newSeps) {
    println("LKJKLJLKJLK");
    println(sl[seps=newSeps]);
  }
  return sl[seps=newSeps];
}

str removeAligners(str src) {
  if (contains(src,"\'")) {
    newline = findFirst(src,"\n");
    quote = findFirst(src,"\'");
    if (newline == -1) {
      return substring(src,quote);
    }
    return src[..newline+1] + src[quote+1..];
  }
  return src;
}

node toST(node tree) = toST(tree, ());
node toST(node tree, map[loc,str] sourceCache) {
  source = readSrc(asLoc(tree.src), sourceCache);
  sst = toST(tree, source, sourceCache);
  return removeAligners(sst);
}

node toST(node tree, str src) = toST(tree, src, ());
node toST(node tree, str _, map[loc,str] srcCache) {
  return removeAligners(addSeps(wrapLists(tree), srcCache));
}

node wrapLists(node ast) {
  children = for (child <- getChildren(ast)) {
    if ([*elts] := child) {
      append lst([wrapLists(elt) | elt <- elts], []);
    } else {
      append wrapLists(child);
    }
  }
  parameters = getKeywordParameters(ast);
  params = ();
  for (key <- parameters, val := parameters[key]) {
    if ([*elts] := val) {
      params += (key : lst([wrapLists(elt) | elt <- elts], []));
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
default &T wrapLists(&T val) {
  throw "Impossible";
}
//maybe: default &T wrapLists(&T val) = val;

loc asLoc(value v) {
  if (loc l := v) {
    return l;
  }
  throw "Impossible";
}

loc getLoc(node n) {
  if (lst(elts, []) := n) {
    loc first = getLoc(elts[0]);
    loc last = getLoc(elts[-1]);
    return first[length=last.offset+last.length-first.offset];
  }
  if (loc l := n.src) {
    return l;
  }
  throw "Impossible";
}

list[&T] repeat(&T what, int times) = ([what] | it + what | _ <- [1..times]);

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
  } else if ([str _] := children) {
    seps = ["",""];
    newChildren = children;
  } else {
    i = 0;
    while (i < size(children) && lst([],[]) := children[i]) {//find cutoff point for "prefix"
      i = i + 1;
    }
    if (i == size(children)) {//what to do if all children are lst([],[])?
      seps = [code] + repeat("", size(children)-1);
    } else {
      seps += code[..getLoc(children[i]).offset-offset];//"prefix"
      m = max(0,size(children)-1);
      for (j <- [0..m]) {//add separators between children
        if (lst([],[]) := children[j]) {
          seps += "";
          continue;
        }
        firstLoc = getLoc(children[j]);
        k = 1;
        while (j+k < size(children) && lst([], []) := children[j+k]) {
          k = k + 1;
        }
        if (j+k < size(children)) {
          secondLoc = getLoc(children[j+k]);
          seps += code[firstLoc.offset-offset+firstLoc.length..secondLoc.offset-offset];
        }
      }
      if (lst([], []) := children[-1]) {//TODO: Check
        seps += "";
      }
    
      assert size(children) == size(seps);//TODO: REMOVE
    
      i = size(children) - 1;//find cutoff point for "postfix"
      while (lst([], []) := children[i] && i >= 0) {
        i = i - 1;
      }
      if (i == -1) {//what to do if all children are lst([],[])?
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
    str result = readSrc(beforeLoc[offset=beforeLoc.offset+beforeLoc.length][length=afterLoc.offset-beforeLoc.offset-beforeLoc.length], sourceCache);
    return removeAligners(result);
  }
  throw "Impossible";
}

SepList[value] addSeps(sl:lst([], []), _) = lst([],[]); 
SepList[&T] addSeps(SepList[&T] sl, map[loc,str] sourceCache) {
  seps = for (i <- [0..size(sl.elts)-1]) {
    append readBetween(sl.elts[i], sl.elts[i+1], sourceCache);
  }
  elts = [addSeps(elt, sourceCache) | elt <- sl.elts];
  return lst(elts, seps);
}

loc addSeps(loc l, map[loc,str] _) = l;
str addSeps(str s, map[loc,str] _) = s;

list[str] getSeps(node n) {
  if (list[str] seps := n.seps) {
    return seps;
  }
  throw "Impossible";
}

str yield(str s) = s;
str yield(node n, bool addIndentation = false) {
  if (addIndentation) {
    println("Adding indentation NYI!");
  }
  if (lst(elts, seps) := n) {
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