@license{Copyright (c) 2016-2017, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
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

data Declarator(loc src = |unknown:///|, loc decl = |unknown:///|)
    = \declarator(list[Attribute] attributes, list[Declaration] pointerOperators, Expression name)
    | \declarator(list[Attribute] attributes, list[Declaration] pointerOperators, Expression name, Expression initializer)
    | \fieldDeclarator(list[Attribute] attributes, list[Declaration] pointerOperators, Expression name, Expression bitFieldSize)
    | \fieldDeclarator(list[Attribute] attributes, list[Declaration] pointerOperators, Expression name, Expression bitFieldSize, Expression initializer)
    | \functionDeclarator(list[Attribute] attributes, list[Declaration] pointerOperators, Expression name, list[Declaration] parameters)  //superfluous?
    | \functionDeclarator(list[Attribute] attributes, list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] parameters, list[Declaration] virtSpecifiers)
    | \functionDeclarator(list[Attribute] attributes, list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] parameters, list[Declaration] virtSpecifiers, Expression trailingReturnType)
    | \functionDeclaratorNested(list[Attribute] attributes, list[Declaration] pointerOperators, list[Modifier] modifiers, Declarator declarator, list[Declaration] parameters, list[Declaration] virtSpecifiers)
    | \functionDeclaratorNested(list[Attribute] attributes, list[Declaration] pointerOperators, list[Modifier] modifiers, Declarator declarator, list[Declaration] parameters, list[Declaration] virtSpecifiers, Expression initializer)
    | \functionDeclaratorNoexcept(list[Attribute] attributes, list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] parameters, list[Declaration] virtSpecifiers, Expression noexceptExpression)
    | \functionDeclaratorWithES(list[Attribute] attributes, list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] parameters, list[Declaration] virtSpecifiers) //empty exception specification
    | \functionDeclaratorWithES(list[Attribute] attributes, list[Declaration] pointerOperators, list[Modifier] modifiers, Expression name, list[Declaration] parameters, list[Declaration] virtSpecifiers, list[Expression] exceptionSpecification)
    | \arrayDeclarator(list[Attribute] attributes, list[Declaration] pointerOperators, Expression name, list[Expression] arrayModifier)
    | \arrayDeclarator(list[Attribute] attributes, list[Declaration] pointerOperators, Expression name, list[Expression] arrayModifier, Expression initializer)
    | \arrayDeclaratorNested(list[Attribute] attributes, list[Declaration] pointerOperators, Declarator declarator, list[Expression] arrayModifier)
    | \arrayDeclaratorNested(list[Attribute] attributes, list[Declaration] pointerOperators, Declarator declarator, list[Expression] arrayModifier, Expression initializer)
    
    //quick fix
    | \missingDeclarator()
    ;
    
data DeclSpecifier(loc src = |unknown:///|)
    = \declSpecifier(list[Attribute] attributes, list[Modifier] modifiers, Type \type)
    | \declSpecifier(list[Attribute] attributes, list[Modifier] modifiers, Type \type, Expression expression) //decltype and type_of
    | \etsEnum(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    | \etsStruct(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|) //ElaboratedTypeSpecifier
    | \etsUnion(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    | \etsClass(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    | \namedTypeSpecifier(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    
    | \struct(list[Modifier] modifiers, Expression name, list[Declaration] members, loc decl = |unknown:///|)  //c
    | \union(list[Modifier] modifiers, Expression name, list[Declaration] members, loc decl = |unknown:///|)   //c
    | \class(list[Modifier] modifiers, Expression name, list[Declaration] members, loc decl = |unknown:///|)   //c
    | \struct(list[Attribute] attributes, list[Modifier] modifiers, Expression name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \union(list[Attribute] attributes, list[Modifier] modifiers, Expression name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \class(list[Attribute] attributes, list[Modifier] modifiers, Expression name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \structFinal(list[Attribute] attributes, list[Modifier] modifiers, Expression name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \unionFinal(list[Attribute] attributes, list[Modifier] modifiers, Expression name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    | \classFinal(list[Attribute] attributes, list[Modifier] modifiers, Expression name, list[Declaration] baseSpecifiers, list[Declaration] members, loc decl = |unknown:///|)
    
    | \enum(list[Attribute] attributes, list[Modifier] modifiers, Expression name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enum(list[Attribute] attributes, list[Modifier] modifiers, DeclSpecifier baseType, Expression name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enumOpaque(list[Attribute] attributes, list[Modifier] modifiers, DeclSpecifier baseType, Expression name, loc decl = |unknown:///|)
    | \enumScoped(list[Attribute] attributes, list[Modifier] modifiers, Expression name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enumScoped(list[Attribute] attributes, list[Modifier] modifiers, DeclSpecifier baseType, Expression name, list[Declaration] enumerators, loc decl = |unknown:///|)
    | \enumScopedOpaque(list[Attribute] attributes, list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    | \enumScopedOpaque(list[Attribute] attributes, list[Modifier] modifiers, DeclSpecifier baseType, Expression name, loc decl = |unknown:///|)
    
     // Non-standard MSVC throw ellipsis
    | \msThrowEllipsis()
    ;
    
data Declaration(loc src=|unknown:///|)
    = \translationUnit(list[Declaration] declarations)
    | \simpleDeclaration(list[Attribute] attributes, DeclSpecifier declSpecifier, list[Declarator] declarators)//?
    | \functionDefinition(Expression returnSpec, Declarator declarator, Statement body)//?
    | \defaultedFunctionDefinition(list[Attribute] attributes, DeclSpecifier declSpecifier, list[Expression] memberInitializer, Declarator declarator)
    | \deletedFunctionDefinition(list[Attribute] attributes, DeclSpecifier declSpecifier, list[Expression] memberInitializer, Declarator declarator)
    | \functionDefinition(list[Attribute] attributes, DeclSpecifier declSpecifier, Declarator declarator, list[Expression] memberInitializer, Statement body)
    | \functionWithTryBlockDefinition(list[Attribute] attributes, DeclSpecifier declSpecifier, Declarator declarator, list[Expression] memberInitializers, Statement sbody, list[Statement] catchHandlers)
    
    | \asmDeclaration(str assembly)
    
    | \enumerator(Expression name, Expression \value, loc decl = |unknown:///|)
    | \enumerator(Expression name, loc decl = |unknown:///|)
    
    | \usingDirective(list[Attribute] attributes, Expression qualifiedName, loc decl = |unknown:///|)
    | \visibilityLabel(Modifier visibility)
    
    //| \etsEnum(Expression name)
    //| \etsStruct(Expression name) //ElaboratedTypeSpecifier
    //| \etsUnion(Expression name)
    //| \etsClass(Expression name)
    
    | \pointer(list[Attribute] attributes, list[Modifier] modifiers)    // *
    | \pointerToMember(list[Attribute] attributes, list[Modifier] modifiers, Expression nestedName)
    | \reference(list[Attribute] attributes)  // &
    | \rvalueReference(list[Attribute] attributes) // &&
    
    | \parameter(DeclSpecifier declSpecifier)
    | \parameter(DeclSpecifier declSpecifier, Declarator declarator)
    
    //| \declSpecifier(list[Modifier] modifiers, Type \type)
    //| \declSpecifier(list[Modifier] modifiers, Type \type, Expression expression) //decltype and type_of
    //| \initializerClause(Expression expression) Unneeded layer of abstraction?
    //| \initializerList(list[Expression] clauses)
    
    //| \declarationEqualsInitializer(str name, Expression initializer) //weg //Que?
    
    | \template(list[Declaration] parameters, Declaration declaration, TypeSymbol \type)
    | \sttClass(Expression name, loc decl = |unknown:///|) //simpleTypeTemplateParameter    
    | \sttTypename(Expression name, loc decl = |unknown:///|) //simpleTypeTemplateParameter
    | \sttClass(Expression name, Expression defaultType, loc decl = |unknown:///|) //simpleTypeTemplateParameter    
    | \sttTypename(Expression name, Expression defaultType, loc decl = |unknown:///|) //simpleTypeTemplateParameter
    
    | \tttParameter(list[Declaration] nestedParameters, Expression name, loc decl = |unknown:///|) //templatedTypeTemplateParameter
    
    | \baseSpecifier(list[Modifier] modifiers, loc decl = |unknown:///|)
    | \baseSpecifier(list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    
    | \virtSpecifier(Modifier modifier)
    
    | \namespaceDefinition(list[Attribute] attributes, Expression name, list[Declaration] declarations, loc decl = |unknown:///|)
    | \namespaceDefinitionInline(list[Attribute] attributes, Expression name, list[Declaration] declarations, loc decl = |unknown:///|)
    | \usingDeclaration(list[Attribute] attributes, list[Modifier] modifiers, Expression name, loc decl = |unknown:///|)
    | \namespaceAlias(Expression \alias, Expression mapping, loc decl = |unknown:///|)
    
    | \linkageSpecification(str literal, list[Declaration] declarations)
    | \alias(list[Attribute] attributes, Expression \alias, Expression mappingTypeId, loc decl = |unknown:///|)
    
    | \staticAssert(Expression condition, Expression message)
    
    | \explicitTemplateInstantiation(Declaration declaration)
    | \explicitTemplateInstantiation(Modifier modifier, Declaration declaration)
    | \explicitTemplateSpecialization(Declaration declaration)
    
    | \varArgs() //encoding for ellipsis in f(x, ...);
    
    | \problemDeclaration()
    ;


data Expression(loc src = |unknown:///|, TypeSymbol typ = \unresolved())
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
    | \abstractEmptyName()

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
    
    | \fieldReference(Expression fieldOwner, Expression name, loc decl = |unknown:///|)
    | \fieldReferencePointerDeref(Expression fieldOnwer, Expression name, loc decl = |unknown:///|)
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
    | \arrayModifier(list[Attribute] attributes)
    | \arrayModifier(list[Attribute] attributes, Expression constExpression)
    
    | \simpleTypeConstructor(DeclSpecifier declSpecifier, Expression initializer)
    
    | \expressionList(list[Expression] expressions)
    
    | \templateId(Expression name, list[Expression] argumentTypes, loc decl = |unknown:///|)

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
    
    | \problemExpression()
    
    ;                       
 

data Statement(loc src = |unknown:///|)
    = \compoundStatement(list[Attribute] attributes, list[Statement] statements)
    | \declarationStatement(list[Attribute] attributes, Declaration declaration)
    | \expressionStatement(list[Attribute] attributes, Expression expression)
    | \if(list[Attribute] attributes, Expression condition, Statement thenClause)
    | \if(list[Attribute] attributes, Expression condition, Statement thenClause, Statement elseClause)
    | \ifWithDecl(list[Attribute] attributes, Declaration conditionDeclaration, Statement thenClause)
    | \ifWithDecl(list[Attribute] attributes, Declaration conditionDeclaration, Statement thenClause, Statement elseClause)
    | \for(list[Attribute] attributes, Statement sInitializer, Expression condition, Expression iteration, Statement body)
    | \forWithDecl(list[Attribute] attributes, Statement sInitializer, Declaration conditionDeclaration, Expression iteration, Statement body)
    | \rangeBasedFor(list[Attribute] attributes, Declaration declaration, Expression initializer, Statement body)
    | \switch(list[Attribute] attributes, Expression controller, Statement body)
    | \switchWithDecl(list[Attribute] attributes, Declaration controllerDeclaration, Statement body)
    | \case(list[Attribute] attributes, Expression expression)
    | \defaultCase(list[Attribute] attributes)
    | \break(list[Attribute] attributes)
    | \while(list[Attribute] attributes, Expression condition, Statement body)
    | \whileWithDecl(list[Attribute] attributes, Declaration conditionDeclaration, Statement body)
    | \continue(list[Attribute] attributes)
    | \do(list[Attribute] attributes, Statement body, Expression condition)
    
    | \return(list[Attribute] attributes, Expression expression) //note: also with initializerClause, which currently is an Expression
    | \return(list[Attribute] attributes)
    | \nullStatement(list[Attribute] attributes)
    | \label(list[Attribute] attributes, Expression name, Statement nestedStatement, loc decl = |unknown:///|)
    | \goto(list[Attribute] attributes, Expression name, loc decl = |unknown:///|)
    
    | \tryBlock(list[Attribute] attributes, Statement tryBody, list[Statement] catchHandlers)
    | \catch(list[Attribute] attributes, Declaration declaration, Statement body)
    | \catchAll(list[Attribute] attributes, Statement body)    
    
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

data Attribute 
    = \attribute(str name)
    | \attribute(str name, str argumentClause)
    | \attributeSpecifier(list[Attribute] attributes)
    | \alignmentSpecifier(Expression typeIdOrExpression)
	;
    
public map[str, list[loc]] classPaths =
  ("vs14": [|file://c:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2014.0/VC/include/|],
  "vs12": [|file://c:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2011.0/VC/include|,
    |file://c:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2011.0/VC/atlmfc/include|,
    |file://c:/Program%20Files%20(x86)/Windows%20Kits/8.0/Include/um|,
    |file://c:/Program%20Files%20(x86)/Windows%20Kits/8.0/Include/shared|],
  "vs13": [|file:///c:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2012.0/VC/include|,
    |file:///c:/Program%20Files%20(x86)/Microsoft%20Visual%20Studio%2012.0/VC/atlmfc/include|,
    |file:///c:/Program%20Files%20(x86)/Windows%20Kits/8.0/Include/um|,
    |file:///c:/Program%20Files%20(x86)/Windows%20Kits/8.0/Include/shared|],
  "mingw": [|file://c:/MinGW/include|, |file://c:/MinGW/include/sys|, |file://c:/MinGW/lib/gcc/mingw32/5.3.0/include|,
    |file://c:/MinGW/lib/gcc/mingw32/5.3.0/include/c++|, |file://c:/MinGW/lib/gcc/mingw32/5.3.0/include/c++/mingw32|],
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
java Declaration parseCpp(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ());

@javaClass{lang.cpp.internal.Parser}
@reflect{need access to streams}
java list[loc] parseForComments(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ());

@javaClass{lang.cpp.internal.Parser}
@reflect{need access to streams}
java list[loc] parseForMacros(loc file, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ());

@javaClass{lang.cpp.internal.Parser}  
@reflect{need access to streams}   
java Expression parseExpression(str expression);

Expression parseExpr(str expression) = unsetRec(parseExpression(expression));

map[loc,Declaration] parseDir(loc dir, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ())
  = parseFiles([dir + file|file <- listEntries(dir)], includePaths = includePaths, additionalMacros = additionalMacros);

@javaClass{lang.cpp.internal.Parser}
@reflect{need access to streams}
java map[loc,Declaration] parseFiles(list[loc] files, list[loc] includePaths = classPaths["vs12"], map[str,str] additionalMacros = ());
