@license{Copyright (c) 2016-2018, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
All rights reserved. 
 
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
 
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
  
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

}
module lang::cpp::AST

import IO;
import Node;
import lang::cpp::TypeSymbol;

//extend analysis::m3::AST;

data Declarator(list[Attribute] attributes = [], loc src = |unknown:///|, loc decl = |unknown:///|)
    = \declarator(list[Declaration] pointerOperators, Name name)
    | \declarator(list[Declaration] pointerOperators, Name name, Expression initializer)
    | \fieldDeclarator(list[Declaration] pointerOperators, Name name, Expression bitFieldSize)
    | \fieldDeclarator(list[Declaration] pointerOperators, Name name, Expression bitFieldSize, Expression initializer)
    | \functionDeclarator(list[Declaration] pointerOperators, Name name, list[Declaration] parameters)  //superfluous?
    | \functionDeclarator(list[Declaration] pointerOperators, list[Modifier] modifiers, Name name, list[Declaration] parameters, list[Declaration] virtSpecifiers)
    | \functionDeclarator(list[Declaration] pointerOperators, list[Modifier] modifiers, Name name, list[Declaration] parameters, list[Declaration] virtSpecifiers, Expression trailingReturnType)
    | \functionDeclaratorNested(list[Declaration] pointerOperators, list[Modifier] modifiers, Declarator declarator, list[Declaration] parameters, list[Declaration] virtSpecifiers)
    | \functionDeclaratorNested(list[Declaration] pointerOperators, list[Modifier] modifiers, Declarator declarator, list[Declaration] parameters, list[Declaration] virtSpecifiers, Expression initializer)
    | \functionDeclaratorNoexcept(list[Declaration] pointerOperators, list[Modifier] modifiers, Name name, list[Declaration] parameters, list[Declaration] virtSpecifiers, Expression noexceptExpression)
    | \functionDeclaratorWithES(list[Declaration] pointerOperators, list[Modifier] modifiers, Name name, list[Declaration] parameters, list[Declaration] virtSpecifiers) //empty exception specification
    | \functionDeclaratorWithES(list[Declaration] pointerOperators, list[Modifier] modifiers, Name name, list[Declaration] parameters, list[Declaration] virtSpecifiers, list[Expression] exceptionSpecification)
    | \arrayDeclarator(list[Declaration] pointerOperators, Name name, list[Expression] arrayModifier)
    | \arrayDeclarator(list[Declaration] pointerOperators, Name name, list[Expression] arrayModifier, Expression initializer)
    | \arrayDeclaratorNested(list[Declaration] pointerOperators, Declarator declarator, list[Expression] arrayModifier)
    | \arrayDeclaratorNested(list[Declaration] pointerOperators, Declarator declarator, list[Expression] arrayModifier, Expression initializer)
    
    //quick fix
    | \missingDeclarator() //no attributes
    ;
    
data DeclSpecifier(list[Attribute] attributes = [], loc src = |unknown:///|)
    = \declSpecifier(list[Modifier] modifiers, Type \type)
    | \declSpecifier(list[Modifier] modifiers, Type \type, Expression expression) //decltype and type_of
    | \etsEnum(list[Modifier] modifiers, Name name, loc decl = |unknown:///|) //no attributes
    | \etsStruct(list[Modifier] modifiers, Name name, loc decl = |unknown:///|) //ElaboratedTypeSpecifier //no attributes
    | \etsUnion(list[Modifier] modifiers, Name name, loc decl = |unknown:///|) //no attributes
    | \etsClass(list[Modifier] modifiers, Name name, loc decl = |unknown:///|) //no attributes
    | \namedTypeSpecifier(list[Modifier] modifiers, Name name, loc decl = |unknown:///|) //no attributes
    
    | \struct(list[Modifier] modifiers, Name name, list[Declaration] members, loc decl = |unknown:///|)  //c //no attributes
    | \union(list[Modifier] modifiers, Name name, list[Declaration] members, loc decl = |unknown:///|)   //c //no attributes
    | \class(list[Modifier] modifiers, Name name, list[Declaration] members, loc decl = |unknown:///|)   //c //no attributes
    | \struct(list[Modifier] modifiers, Name name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \union(list[Modifier] modifiers, Name name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \class(list[Modifier] modifiers, Name name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \structFinal(list[Modifier] modifiers, Name name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \unionFinal(list[Modifier] modifiers, Name name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \classFinal(list[Modifier] modifiers, Name name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    
    | \enum(list[Modifier] modifiers, Name name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enum(list[Modifier] modifiers, DeclSpecifier baseType, Name name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enumOpaque(list[Modifier] modifiers, DeclSpecifier baseType, Name name, loc decl = |unknown:///|)
    | \enumScoped(list[Modifier] modifiers, Name name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enumScoped(list[Modifier] modifiers, DeclSpecifier baseType, Name name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enumScopedOpaque(list[Modifier] modifiers, Name name, loc decl = |unknown:///|)
    | \enumScopedOpaque(list[Modifier] modifiers, DeclSpecifier baseType, Name name, loc decl = |unknown:///|)
    
     // Non-standard MSVC throw ellipsis
    | \msThrowEllipsis() //no attributes
    ;
    
data Declaration(list[Attribute] attributes = [], loc src=|unknown:///|)
    = \translationUnit(list[Declaration] declarations) //no attributes
    | \simpleDeclaration(DeclSpecifier declSpecifier, list[Declarator] declarators)
    | \functionDefinition(Expression returnSpec, Declarator declarator, Statement body)//? //no attributes
    | \defaultedFunctionDefinition(DeclSpecifier declSpecifier, list[Expression] memberInitializer, Declarator declarator)
    | \deletedFunctionDefinition(DeclSpecifier declSpecifier, list[Expression] memberInitializer, Declarator declarator)
    | \functionDefinition(DeclSpecifier declSpecifier, Declarator declarator, list[Expression] memberInitializer, Statement body)
    | \functionWithTryBlockDefinition(DeclSpecifier declSpecifier, Declarator declarator, list[Expression] memberInitializers, Statement sbody, list[Statement] catchHandlers)
    
    | \asmDeclaration(str assembly) //no attributes
    
    | \enumerator(Name name, Expression \value, loc decl = |unknown:///|) //no attributes
    | \enumerator(Name name, loc decl = |unknown:///|) //no attributes
    
    | \usingDirective(Name qualifiedName, loc decl = |unknown:///|)
    | \visibilityLabel(Modifier visibility) //no attributes
    
    //| \etsEnum(Name name)
    //| \etsStruct(Name name) //ElaboratedTypeSpecifier
    //| \etsUnion(Name name)
    //| \etsClass(Name name)
    
    | \pointer(list[Modifier] modifiers)    // *
    | \pointerToMember(list[Modifier] modifiers, Name nestedName)
    | \reference()  // &
    | \rvalueReference() // &&
    
    | \parameter(DeclSpecifier declSpecifier) //no attributes
    | \parameter(DeclSpecifier declSpecifier, Declarator declarator) //no attributes
    
    //| \declSpecifier(list[Modifier] modifiers, Type \type)
    //| \declSpecifier(list[Modifier] modifiers, Type \type, Expression expression) //decltype and type_of
    //| \initializerClause(Expression expression) Unneeded layer of abstraction?
    //| \initializerList(list[Expression] clauses)
    
    //| \declarationEqualsInitializer(str name, Expression initializer) //weg //Que?
    
    | \template(list[Declaration] parameters, Declaration declaration, TypeSymbol \type) //no attributes
    | \sttClass(Name name, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes
    | \sttTypename(Name name, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes
    | \sttClass(Name name, Expression defaultType, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes    
    | \sttTypename(Name name, Expression defaultType, loc decl = |unknown:///|) //simpleTypeTemplateParameter //no attributes
    
    | \tttParameter(list[Declaration] nestedParameters, Name name, loc decl = |unknown:///|) //templatedTypeTemplateParameter //no attributes
    
    | \baseSpecifier(list[Modifier] modifiers, loc decl = |unknown:///|) //no attributes
    | \baseSpecifier(list[Modifier] modifiers, Name name, loc decl = |unknown:///|) //no attributes
    
    | \virtSpecifier(Modifier modifier) //no attributes
    
    | \namespaceDefinition(Name name, list[Declaration] declarations, loc decl = |unknown:///|)
    | \namespaceDefinitionInline(Name name, list[Declaration] declarations, loc decl = |unknown:///|)
    | \usingDeclaration(list[Modifier] modifiers, Name name, loc decl = |unknown:///|)
    | \namespaceAlias(Name \alias, Name mapping, loc decl = |unknown:///|) //no attributes
    
    | \linkageSpecification(str literal, list[Declaration] declarations) //no attributes
    | \alias(Name \alias, Expression mappingTypeId, loc decl = |unknown:///|)
    
    | \staticAssert(Expression condition, Expression message) //no attributes
    
    | \explicitTemplateInstantiation(Declaration declaration) //no attributes
    | \explicitTemplateInstantiation(Modifier modifier, Declaration declaration) //no attributes
    | \explicitTemplateSpecialization(Declaration declaration) //no attributes
    
    | \varArgs() //encoding for ellipsis in f(x, ...); //no attributes
    
    | \problemDeclaration() //no attributes
    ;


data Expression(loc src = |unknown:///|, TypeSymbol typ = \unresolved()) //no attributes
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
    
    | \idExpression(Name name, loc decl = |unknown:///|)
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
    
    //| \namedTypeSpecifier(list[Modifier] modifiers, Name name)
    
    | \functionCall(Expression functionName, list[Expression] arguments)
    
    | \fieldReference(Expression fieldOwner, Name name, loc decl = |unknown:///|)
    | \fieldReferencePointerDeref(Expression fieldOwner, Name name, loc decl = |unknown:///|)
    //| \constructorInitializer(list[Expression] arguments)
    | \new(Expression typeId)
    | \new(Expression typeId, Expression initializer)
    | \newWithArgs(list[Expression] arguments, Expression typeId)
    | \newWithArgs(list[Expression] arguments, Expression typeId, Expression initializer)
    | \globalNew(Expression typeId)
    | \globalNew(Expression typeId, Expression initializer)
    | \globalNewWithArgs(list[Expression] arguments, Expression typeId)
    | \globalNewWithArgs(list[Expression] arguments, Expression typeId, Expression initializer)
    
    | \delete(Expression expression)
    | \vectoredDelete(Expression expression)
    | \globalDelete(Expression expression)
    | \globalVectoredDelete(Expression expression)
    
    | \arraySubscriptExpression(Expression array, Expression argument)
    | \arrayModifier(list[Attribute] attributes = [])
    | \arrayModifier(Expression constExpression, list[Attribute] attributes = [])
    
    | \simpleTypeConstructor(DeclSpecifier declSpecifier, Expression initializer)
    
    | \expressionList(list[Expression] expressions)
    
    | \compoundStatementExpression(Statement compoundStatement)
    
    | \empty()    
    | \nyi(str raw)
    
    | \lambda(Modifier captureDefault, list[Expression] captures, Declarator declarator, Statement body)
    
    | \packExpansion(Expression pattern)
    
    | \typeIdInitializerExpression(Expression typeId, Expression initializer)
    
    // TypeId below
    | \typeId(DeclSpecifier declSpecifier)
    | \typeId(DeclSpecifier declSpecifier, Declarator abstractDeclarator)
    
    // Initializers below
    | \equalsInitializer(Expression initializer)
    | \initializerList(list[Expression] clauses) //initializerClause?
    | \constructorChainInitializer(Name name, Expression initializer, loc decl = |unknown:///|)
    | \constructorInitializer(list[Expression] arguments)
    
    // DesignatedInitializers below
    | \designatedInitializer(list[Expression] designators, Expression operand)
    
    // Designators below
    | \arrayDesignator(Expression subscript)
    | \fieldDesignator(Expression fieldName)
    | \arrayRangeDesignator(Expression rangeFloor, Expression rangeCeiling) //gcc-only
    
    // Captures
    | \capture(Name name, loc decl = |unknown:///|)
    | \captureByRef(Name name, loc decl = |unknown:///|)
    | \captureThisPtr()
    
    | \problemExpression()
    
    ;                       
 
data Name(loc src = |unknown:///|) //no attributes
    = \name(str \value)
    | \qualifiedName(list[Name] qualifiers, Name lastName, loc decl = |unknown:///|)
    | \operatorName(str \value)
    | \conversionName(str \value, Expression typeId)
    
    | \templateId(Name name, list[Expression] argumentTypes, loc decl = |unknown:///|)

    | \abstractEmptyName()
    ;
 
data Statement(list[Attribute] attributes = [], loc src = |unknown:///|)
    = \compoundStatement(list[Statement] statements)
    | \declarationStatement(Declaration declaration)
    | \expressionStatement(Expression expression)
    | \if(Expression condition, Statement thenClause)
    | \if(Expression condition, Statement thenClause, Statement elseClause)
    | \ifWithDecl(Declaration conditionDeclaration, Statement thenClause)
    | \ifWithDecl(Declaration conditionDeclaration, Statement thenClause, Statement elseClause)
    | \for(Statement sInitializer, Expression condition, Expression iteration, Statement body)
    | \forWithDecl(Statement sInitializer, Declaration conditionDeclaration, Expression iteration, Statement body)
    | \rangeBasedFor(Declaration declaration, Expression initializer, Statement body)
    | \switch(Expression controller, Statement body)
    | \switchWithDecl(Declaration controllerDeclaration, Statement body)
    | \case(Expression expression)
    | \defaultCase()
    | \break()
    | \while(Expression condition, Statement body)
    | \whileWithDecl(Declaration conditionDeclaration, Statement body)
    | \continue()
    | \do(Statement body, Expression condition)
    
    | \return(Expression expression) //note: also with initializerClause, which currently is an Expression
    | \return()
    | \nullStatement()
    | \label(Name name, Statement nestedStatement, loc decl = |unknown:///|)
    | \goto(Name name, loc decl = |unknown:///|)
    
    | \tryBlock(Statement tryBody, list[Statement] catchHandlers)
    | \catch(Declaration declaration, Statement body)
    | \catchAll(Statement body)    
    
    | \problem(str raw) //no attributes
    ;           
  
data Type(loc src = |unknown:///|) //no attributes
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
    
    | \structType(Name name)
    | \unionType(Name name)
    | \classType(Name name)

    ;
  
data Modifier(loc src = |unknown:///|) //no attributes
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

data Attribute //no attributes
    = \attribute(str name)
    | \attribute(str name, str argumentClause)
    | \attributeSpecifier(list[Attribute] attributes)
    | \alignmentSpecifier(Expression typeIdOrExpression)
	;
    
public map[str, list[loc]] classPaths =
  ("vs14": [|file:///C:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2014.0/VC/include/|],
  "vs12": [|file:///C:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2011.0/VC/include|,
    |file:///C:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2011.0/VC/atlmfc/include|,
    |file:///C:/Program%20Files%20(x86)/Windows%20Kits/8.0/Include/um|,
    |file:///C:/Program%20Files%20(x86)/Windows%20Kits/8.0/Include/shared|],
  "vs13": [|file:///C:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2012.0/VC/include|,
    |file:///C:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2012.0/VC/atlmfc/include|,
    |file:///C:/Program%20Files%20(x86)/Windows%20Kits/8.1/Include/um|,
    |file:///C:/Program%20Files%20(x86)/Windows%20Kits/8.1/Include/shared|],
  "mingw": [|file:///C:/MinGW/include|, |file:///C:/MinGW/include/sys|, |file:///C:/MinGW/lib/gcc/mingw32/5.3.0/include|,
    |file:///C:/MinGW/lib/gcc/mingw32/5.3.0/include/c++|, |file:///C:/MinGW/lib/gcc/mingw32/5.3.0/include/c++/mingw32|],
  "mac": [|file:///usr/include|,
    |file:///usr/include/c++/4.2.1|,
    |file:///usr/include/c++/4.2.1/tr1|],
  "mac-xcode": [|file:///Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/usr/include/c++/4.2.1|,
                |file:///Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/usr/include/c++/4.2.1/tr1|,
                |file:///Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/usr/include|
     ]
    );

@javaClass{lang.cpp.internal.Parser}  
@reflect{need access to streams}   
java Declaration parseCpp(loc file, list[loc] stdLib = classPaths["vs12"], list[loc] includeDirs = [], map[str,str] additionalMacros = (), bool includeStdLib = false);

@javaClass{lang.cpp.internal.Parser}  
@reflect{need access to streams}   
java Declaration parseString(str code);

@javaClass{lang.cpp.internal.Parser}  
@reflect{need access to streams}   
java Declaration parseString(str code, loc l);

@javaClass{lang.cpp.internal.Parser}
@reflect{need access to streams}
java list[loc] parseForComments(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ());

@javaClass{lang.cpp.internal.Parser}
@reflect{need access to streams}
java list[loc] parseForMacros(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ());

