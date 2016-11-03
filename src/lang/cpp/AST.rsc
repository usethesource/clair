module lang::cpp::AST

import IO;

extend analysis::m3::AST;

data Declarator
    = \declarator(list[Declaration] pointerOperators, Expression nname)
    | \declarator(list[Declaration] pointerOperators, Expression nname, Expression initializer)
    | \functionDeclarator(list[Declaration] pointerOperators, Expression name, list[Declaration] parameters)  //superfluous?
    | \functionDeclarator(list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] parameters, list[Declaration] virtSpecifiers)
    | \functionDeclaratorNested(list[Declaration] pointerOperators, list[Modifier] modifiers, Declarator declarator, list[Declaration] arguments, list[Declaration] virtSpecifiers)
    | \functionDeclaratorNested(list[Declaration] pointerOperators, list[Modifier] modifiers, Declarator declarator, list[Declaration] arguments, list[Declaration] virtSpecifiers, Expression initializer)
    | \functionDeclaratorWithES(list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] arguments, list[Declaration] virtSpecifiers) //empty exception specification
    | \functionDeclaratorWithES(list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] arguments, list[Declaration] virtSpecifiers, list[Type] exceptionSpecification)
    | \arrayDeclarator(Expression name, list[Expression] arrayModifier)
    | \arrayDeclarator(Expression name, list[Expression] arrayModifier, Expression initializer)
    ;
    
data DeclSpecifier
    = \declSpecifier(list[Modifier] modifiers, Type \type)
    | \declSpecifier(list[Modifier] modifiers, Type \type, Expression expression) //decltype and type_of
    | \etsEnum(Expression name)
    | \etsStruct(Expression name) //ElaboratedTypeSpecifier
    | \etsUnion(Expression name)
    | \etsClass(Expression name)
    | \namedTypeSpecifier(list[Modifier] modifiers, Expression name)
    
    | \struct(Expression name, list[Declaration] members)  //c
    | \union(Expression name, list[Declaration] members)   //c
    | \class(Expression name, list[Declaration] members)   //c
    | \struct(Expression name,  list[Declaration] baseSpecifiers,list[Declaration] members)
    | \union(Expression name, list[Declaration] baseSpecifiers, list[Declaration] members)
    | \class(Expression name, list[Declaration] baseSpecifiers, list[Declaration] members)
    
    | \enum(Expression name, list[Declaration] enumerators)
    ;
    
data Declaration
    = \translationUnit(list[Declaration] declarations)
    | \simpleDeclaration(DeclSpecifier declSpecifier, list[Declarator] declarators)//?
    | \functionDefinition(Expression returnSpec, Declarator declarator, Statement body)//?
    | \defaultedFunctionDefinition(DeclSpecifier declSpecifier, list[Expression] memberInitializer, Declarator declarator)
    | \deletedFunctionDefinition(DeclSpecifier declSpecifier, list[Expression] memberInitializer, Declarator declarator)
    | \functionDefinition(DeclSpecifier declSpecifier, Declarator declarator, list[Expression] memberInitializer, Statement body)
    | \functionWithTryBlockDefinition(DeclSpecifier declSpecifier, Declarator declarator, list[Expression] memberInitializers, Statement sbody, list[Statement] catchHandlers)
    
    | \asmDeclaration(str assembly)
    
    | \enumerator(str name, Expression \value)
    | \enumerator(str name)
    
    | \usingDirective(Expression qualifiedName)
    | \visibilityLabel(Modifier visibility)
    
    //| \etsEnum(Expression nname)
    //| \etsStruct(Expression nname) //ElaboratedTypeSpecifier
    //| \etsUnion(Expression nname)
    //| \etsClass(Expression nname)
    
    | \pointer(list[Modifier] modifiers)    // *
    | \reference()  // &
    
    | \parameter(DeclSpecifier declSpecifier)
    | \parameter(DeclSpecifier declSpecifier, Declarator declarator)
    
    //| \declSpecifier(list[Modifier] modifiers, Type \type)
    //| \declSpecifier(list[Modifier] modifiers, Type \type, Expression expression) //decltype and type_of
    //| \initializerClause(Expression expression) Unneeded layer of abstraction?
    //| \initializerList(list[Expression] clauses)
    
    //| \declarationEqualsInitializer(str name, Expression initializer) //weg //Que?
    
    | \template(list[Declaration] parameters, Declaration declaration)
    | \sttClass(Expression nname) //simpleTypeTemplateParameter    
    | \sttTypename(Expression nname) //simpleTypeTemplateParameter
    
    | \baseSpecifier(Modifier modifier)
    | \baseSpecifier(Modifier modifier, Expression nname)
    
    | \virtSpecifier(Modifier modifier)
    
    | \namespaceDefinition(Expression nname, list[Declaration] declarations, bool isInline)
    | \usingDeclaration(Expression nname)
    | \namespaceAlias(Expression \alias, Expression mapping)
    
    | \linkageSpecification(str literal, list[Declaration] declarations)
    | \alias(Expression \alias, Expression mappingTypeId)
    
    ;


data Expression
    = \multiply(Expression lhs, Expression rhs)
    | \divide(Expression lhs, Expression rhs)
    | \modulo(Expression lhs, Expression rhs)
    | \plus(Expression lhs, Expression rhs)
    | \minus(Expression lhs, Expression rhs)
    | \shiftLeft(Expression lhs, Expression rhs)
    | \shiftRight(Expression lhs, Expression rhs)
    | \lessThan(Expression lhs, Expression rhs)
    | \greaterThan(Expression lhs, Expression rhs)
    | \lessEqual(Expression lhs, Expression rhs)
    | \greaterEqual(Expression lhs, Expression rhs)
    | \binaryAnd(Expression lhs, Expression rhs)
    | \binaryXor(Expression lhs, Expression rhs)
    | \binaryOr(Expression lhs, Expression rhs)
    | \logicalAnd(Expression lhs, Expression rhs)
    | \logicalOr(Expression lhs, Expression rhs)
    | \assign(Expression lhs, Expression rhs)
    | \multiplyAssign(Expression lhs, Expression rhs)
    | \divideAssign(Expression lhs, Expression rhs)
    | \moduloAssign(Expression lhs, Expression rhs)
    | \plusAssign(Expression lhs, Expression rhs)
    | \minusAssign(Expression lhs, Expression rhs)
    | \shiftLeftAssign(Expression lhs, Expression rhs)
    | \shiftRightAssign(Expression lhs, Expression rhs)
    | \binaryAndAssign(Expression lhs, Expression rhs)
    | \binaryXorAssign(Expression lhs, Expression rhs)
    | \binaryOrAssign(Expression lhs, Expression rhs)
    | \equals(Expression lhs, Expression rhs)
    | \notEquals(Expression lhs, Expression rhs)
    | \pmDot(Expression lhs, Expression rhs) //c++ only
    | \pmArrow(Expression lhs, Expression rhs) //c++ only
    | \max(Expression lhs, Expression rhs) //g++ only
    | \min(Expression lhs, Expression rhs) //g++ only
    | \ellipses(Expression lhs, Expression rhs) //g++ only
    
    | \prefixIncr(Expression expression)    //++exp
    | \prefixDecr(Expression expression)    //--exp
    | \plus(Expression expression)          //+exp
    | \minus(Expression expression)         //-exp
    | \star(Expression expression)          //*exp
    | \amper(Expression expression)         //&exp
    | \tilde(Expression expression)         //~exp
    | \not(Expression expression)           //!exp
    | \sizeof(Expression expression)        //sizeof exp
    | \postfixIncr(Expression expression)   //exp++
    | \postfixDecr(Expression expression)   //exp--
    | \bracketed(Expression expression)     //(exp)
    | \throw()
    | \throw(Expression expression)         //throw exp, c++ only
    | \typeid(Expression expression)        //typeid exp, c++ only
    //| \typeof(Expression expression)        //typeof exp, deprecated
    | \alignOf(Expression expression)       //__alignOf(exp), gcc only
    | \sizeofParameterPack(Expression expression) //sizeof...(parameterpack), c++ only?
    | \noexcept(Expression expression)      //noexcept (exp), c++ only
    | \labelReference(Expression expression)//&&label, gcc only?
    
    | \cast(Expression typeId, Expression expression)
    | \dynamicCast(Expression typeId, Expression expression)
    | \staticCast(Expression typeId, Expression expression)
    | \reinterpretCast(Expression typeId, Expression expression)
    | \constCast(Expression typeId, Expression expression)
    
    | \name(str name)
    | \qualifiedName(list[Expression] qualifiers, Expression lastName)
    | \operatorName(str vvalue)
    | \conversionName(str vvalue, Expression typeId)
    | \idExpression(Expression nname)
    | \integerLiteral(int number)
    | \conditional(Expression condition, Expression positive, Expression negative)
    
    | \integerConstant(str vvalue)
    | \floatConstant(str vvalue)
    | \charConstant(str vvalue)
    | \stringLiteral(str vvalue)
    | \this()
    | \true()
    | \false()
    | \nullptr()
    
    //| \namedTypeSpecifier(list[Modifier] modifiers, Expression nname)
    
    | \functionCall(Expression functionName, list[Expression] arguments)
    
    | \fieldReference(Expression fieldOwner, Expression nname, Type fieldType)
    //| \constructorInitializer(list[Expression] arguments)
    | \new(Expression typeId)
    | \new(Expression typeId, Expression initializer)
    | \newWithArgs(list[Expression] arguments, Expression typeId)
    | \newWithArgs(list[Expression] arguments, Expression typeId, Expression initializer)
    | \delete(bool isVectored, Expression expression)
    
    | \arraySubscriptExpression(Expression array, Expression argument)
    | \arrayModifier()
    | \arrayModifier(Expression constExpression)
    
    | \simpleTypeConstructor(DeclSpecifier declSpecifier, Expression initializer)
    
    | \expressionList(list[Expression] expressions)
    
    | \templateId(Expression nname, list[Expression] argumentTypes)

    | \empty()    
    | \nyi(str raw)
    
    | \lambda(Modifier captureDefault, Declarator declarator, Statement body)
    
    // TypeId below
    | \typeId(DeclSpecifier declSpecifier)
    | \typeId(DeclSpecifier declSpecifier, Declarator abstractDeclarator)
    
    // Initializers below
    | \equalsInitializer(Expression initializer)
    | \initializerList(list[Expression] clauses) //initializerClause?
    | \constructorChainInitializer(Expression nname, Expression initializer)
    | \constructorInitializer(list[Expression] arguments)
    ;                       
 

data Statement              
    = \compoundStatement(list[Statement] statements)
    | \declarationStatement(Declaration declaration)
    | \expressionStatement(Expression expression)
    | \if(Expression condition, Statement thenClause)
    | \if(Expression condition, Statement thenClause, Statement elseClause)
    | \for(Statement sInitializer, Expression condition, Expression iteration, Statement body)
    | \rangeBasedFor(Declaration declaration, Expression initializer, Statement body)
    | \switch(Expression controller, Statement body)
    | \case(Expression expression)
    | \defaultCase()
    | \break()
    | \while(Expression condition, Statement body)
    | \continue()
    | \do(Statement body, Expression condition)
    
    | \return(Expression expression)
    | \return()
    | \nullStatement()
    | \label(Expression nname, Statement nestedStatement)
    | \goto(Expression nname)
    
    | \tryBlock(Statement tryBody, list[Statement] catchHandlers)
    | \catch(Declaration declaration, Statement body)
    | \catchAll(Statement body)    
    
    ;           
  
data Type 
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
    
    
    | \arrayType(Type \type, int size)
    | \basicType(Type \type, list[Modifier] modifiers)
    | \nullptr()
    
    | \structType(Expression nname)
    | \unionType(Expression nname)
    | \classType(str name)

    ;
  
data Modifier
    = typedef()
    | \extern()
    | \static()
    | \auto()
    | \register()
    | \mutable()
    
    | \public()
    | \protected()
    | \private()
    
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
    //| \constexpr()
    | \threadLocal()
    | \pureVirtual()
    
    
    | \captUnspecified()
    | \captByCopy()
    | \captByReference()
    ;
    
@javaClass{lang.cpp.internal.Parser}  
@reflect{need access to streams}   
java Declaration parseCpp(loc file);

