module lang::cpp::AST

import IO;
import Node;

extend analysis::m3::AST;

data Declarator(loc src = |unknown:///|, loc decl = |unknown:///|)
    = \declarator(list[Declaration] pointerOperators, Expression name)
    | \declarator(list[Declaration] pointerOperators, Expression name, Expression initializer)
    | \functionDeclarator(list[Declaration] pointerOperators, Expression name, list[Declaration] parameters)  //superfluous?
    | \functionDeclarator(list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] parameters, list[Declaration] virtSpecifiers)
    | \functionDeclaratorNested(list[Declaration] pointerOperators, list[Modifier] modifiers, Declarator declarator, list[Declaration] arguments, list[Declaration] virtSpecifiers)
    | \functionDeclaratorNested(list[Declaration] pointerOperators, list[Modifier] modifiers, Declarator declarator, list[Declaration] arguments, list[Declaration] virtSpecifiers, Expression initializer)
    | \functionDeclaratorWithES(list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] arguments, list[Declaration] virtSpecifiers) //empty exception specification
    | \functionDeclaratorWithES(list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] arguments, list[Declaration] virtSpecifiers, list[Expression] exceptionSpecification)
    | \arrayDeclarator(Expression name, list[Expression] arrayModifier)
    | \arrayDeclarator(Expression name, list[Expression] arrayModifier, Expression initializer)
    
    //quick fix
    | \missingDeclarator()
    ;
    
data DeclSpecifier(loc src = |unknown:///|)
    = \declSpecifier(list[Modifier] modifiers, Type \type)
    | \declSpecifier(list[Modifier] modifiers, Type \type, Expression expression) //decltype and type_of
    | \etsEnum(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    | \etsStruct(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|) //ElaboratedTypeSpecifier
    | \etsUnion(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    | \etsClass(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    | \namedTypeSpecifier(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    
    | \struct(Expression name, list[Declaration] members, loc decl = |unknown:///|)  //c
    | \union(Expression name, list[Declaration] members, loc decl = |unknown:///|)   //c
    | \class(Expression name, list[Declaration] members, loc decl = |unknown:///|)   //c
    | \struct(Expression name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \union(Expression name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \class(Expression name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    
    | \enum(Expression name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enum(DeclSpecifier baseType, Expression name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enumScoped(Expression name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enumScoped(DeclSpecifier baseType, Expression name, list[Declaration] enumerators, loc decl = |unknown:///|)
    ;
    
data Declaration(loc src=|unknown:///|)
    = \translationUnit(list[Declaration] declarations)
    | \simpleDeclaration(DeclSpecifier declSpecifier, list[Declarator] declarators)//?
    | \functionDefinition(Expression returnSpec, Declarator declarator, Statement body)//?
    | \defaultedFunctionDefinition(DeclSpecifier declSpecifier, list[Expression] memberInitializer, Declarator declarator)
    | \deletedFunctionDefinition(DeclSpecifier declSpecifier, list[Expression] memberInitializer, Declarator declarator)
    | \functionDefinition(DeclSpecifier declSpecifier, Declarator declarator, list[Expression] memberInitializer, Statement body)
    | \functionWithTryBlockDefinition(DeclSpecifier declSpecifier, Declarator declarator, list[Expression] memberInitializers, Statement sbody, list[Statement] catchHandlers)
    
    | \asmDeclaration(str assembly)
    
    | \enumerator(Expression name, Expression \value, loc decl = |unknown:///|)
    | \enumerator(Expression name, loc decl = |unknown:///|)
    
    | \usingDirective(Expression qualifiedName, loc decl = |unknown:///|)
    | \visibilityLabel(Modifier visibility)
    
    //| \etsEnum(Expression name)
    //| \etsStruct(Expression name) //ElaboratedTypeSpecifier
    //| \etsUnion(Expression name)
    //| \etsClass(Expression name)
    
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
    | \sttClass(Expression name, loc decl = |unknown:///|) //simpleTypeTemplateParameter    
    | \sttTypename(Expression name, loc decl = |unknown:///|) //simpleTypeTemplateParameter
    
    | \tttParameter(list[Declaration] nestedParameters, Expression name, loc decl = |unknown:///|) //templatedTypeTemplateParameter
    
    | \baseSpecifier(Modifier modifier, loc decl = |unknown:///|)
    | \baseSpecifier(Modifier modifier, Expression name, loc decl = |unknown:///|)
    
    | \virtSpecifier(Modifier modifier)
    
    | \namespaceDefinition(Expression name, list[Declaration] declarations, bool isInline, loc decl = |unknown:///|)
    | \usingDeclaration(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    | \namespaceAlias(Expression \alias, Expression mapping, loc decl = |unknown:///|)
    
    | \linkageSpecification(str literal, list[Declaration] declarations)
    | \alias(Expression \alias, Expression mappingTypeId, loc decl = |unknown:///|)
    
    | \staticAssert(Expression condition)
    
    | \explicitTemplateInstantiation(Modifier modifier, Declaration declaration)
    
    | \problemDeclaration()
    ;


data Expression(loc src = |unknown:///|)
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
    | \pmDot(Expression lhs, Expression rhs) //c++ only //required decl?
    | \pmArrow(Expression lhs, Expression rhs) //c++ only //requires decl?
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
    
    | \name(str \value)
    | \qualifiedName(list[Expression] qualifiers, Expression lastName, loc decl = |unknown:///|)
    | \operatorName(str \value)
    | \conversionName(str \value, Expression typeId)
    | \idExpression(Expression name, loc decl = |unknown:///|)
    | \integerLiteral(int number)
    | \conditional(Expression condition, Expression positive, Expression negative)
    
    | \integerConstant(str \value)
    | \floatConstant(str \value)
    | \charConstant(str \value)
    | \stringLiteral(str \value)
    | \this()
    | \true()
    | \false()
    | \nullptr()
    
    //| \namedTypeSpecifier(list[Modifier] modifiers, Expression name)
    
    | \functionCall(Expression functionName, list[Expression] arguments)
    
    | \fieldReference(Expression fieldOwner, Expression name, Type fieldType, loc decl = |unknown:///|)
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
    
    | \templateId(Expression name, list[Expression] argumentTypes, loc decl = |unknown:///|)

    | \empty()    
    | \nyi(str raw)
    
    | \lambda(Modifier captureDefault, list[Expression] captures, Declarator declarator, Statement body)
    
    | \packExpansion(Expression pattern)
    
    // TypeId below
    | \typeId(DeclSpecifier declSpecifier)
    | \typeId(DeclSpecifier declSpecifier, Declarator abstractDeclarator)
    
    // Initializers below
    | \equalsInitializer(Expression initializer)
    | \initializerList(list[Expression] clauses) //initializerClause?
    | \constructorChainInitializer(Expression name, Expression initializer, loc decl = |unknown:///|)
    | \constructorInitializer(list[Expression] arguments)
    
    // DesignatedInitializers below
    | \designatedInitializer(list[Expression] designators, Expression operand)
    
    // Designators below
    | \arrayDesignator(Expression subscript)
    | \fieldDesignator(Expression fieldName)
    | \arrayRangeDesignator(Expression rangeFloor, Expression rangeCeiling) //gcc-only
    
    // Captures
    | \capture(Expression name, loc decl = |unknown:///|)
    | \captureByRef(Expression name, loc decl = |unknown:///|)
    | \captureThisPtr()
    ;                       
 

data Statement(loc src = |unknown:///|)
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
    | \label(Expression name, Statement nestedStatement, loc decl = |unknown:///|)
    | \goto(Expression name, loc decl = |unknown:///|)
    
    | \tryBlock(Statement tryBody, list[Statement] catchHandlers)
    | \catch(Declaration declaration, Statement body)
    | \catchAll(Statement body)    
    
    | \problem(str raw)
    ;           
  
data Type(loc src = |unknown:///|)
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
    
    | \structType(Expression name)
    | \unionType(Expression name)
    | \classType(Expression name)

    ;
  
data Modifier(loc src = |unknown:///|)
    = typedef()
    | \extern()
    | \static()
    | \auto()
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
    
loc c = |file://c:|;
    
@javaClass{lang.cpp.internal.Parser}  
@reflect{need access to streams}   
java Declaration parseCpp(loc file,// list[loc] includes = [|file:///Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/usr/include/c++/4.2.1|, |file:///Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/usr/include|]);
 list[loc] includePaths = [|file://c:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2014.0/VC/include/|]);
 //list[loc] includes = [c+"MinGW/lib/gcc/mingw32/5.3.0/include", c+"MinGW/lib/gcc/mingw32/5.3.0/include/c++"]);
 //list[loc] includes = [c+"mingw/lib/gcc/mingw32/5.3.0/",c+"mingw/lib/gcc/",c+"mingw/mingw32/lib/",c+"mingw/lib/"]);
//list[loc] includes = [c+"mingw\\bin\\..\\lib\\gcc\\mingw32\\5.3.0",c+"mingw\\bin\\..\\lib\\gcc",c+"mingw\\bin\\..\\lib\\gcc\\mingw32\\5.3.0..\\..\\..\\..\\mingw32\\lib",c+"mingw\\bin\\..\\lib\\gcc\\mingw32\\5.3.0\\..\\..\\.."]);

@javaClass{lang.cpp.internal.Parser}  
@reflect{need access to streams}   
java Expression parseExpression(str expression);

Expression parseExpr(str expression) = unsetRec(parseExpression(expression));