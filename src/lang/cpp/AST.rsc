module lang::cpp::AST

extend analysis::m3::AST;
   
data Declaration
    = \translationUnit(list[Declaration] declarations)
    | \simpleDeclaration(Type ddeclSpecifier, list[Declaration] declarators)//?
    | \functionDefinition(Type ddeclSpecifier, Declaration ddeclarators, Statement sbody)//?
    //| \declaration(str name, str declarator, list[Statement])
    //| \amb(set[Declaration] alternatives)
    
    | \asmDeclaration(str assembly)
    
    | \enumerator(str name, Expression evalue)
    | \enumerator(str name)//?
    | \enum(str name, list[Expression] enumerators)
    
    | \usingDirective(str qualifiedName)
    | \visibilityLabel(Modifier visibility)
    | \struct(str name, list[Declaration] members)
    | \union(str name, list[Declaration] members)
    | \class(str name, list[Declaration] members)
    
    | \pointerNYI()//TODO: fix
    
    | \declarator(Expression nname)
    | \declarator(Expression nname, Declaration init)
    | \equalsInitializer(Expression initializer)
    | \parameter(Declaration declSpecifier)
    | \parameter(Declaration declSpecifier, Declaration declarator)
    
    | \declSpecifier(list[Modifier] modifiers, Type \type)
    | \declSpecifier(list[Modifier] modifiers, Type \type, Expression expression) //decltype and type_of
    | \initializerClause(Expression expression)
    
    | \declarationEqualsInitializer(str name, Expression initializer) //weg
    
    //| \compilationUnit(Declaration package, list[Declaration] imports, list[Declaration] types)
    //| \enum(str name, list[Type] implements, list[Declaration] constants, list[Declaration] body)
    //| \enumConstant(str name, list[Expression] arguments, Declaration class)
    //| \enumConstant(str name, list[Expression] arguments)
    //| \class(str name, list[Type] extends, list[Type] implements, list[Declaration] body)
    //| \class(list[Declaration] body)
    //| \interface(str name, list[Type] extends, list[Type] implements, list[Declaration] body)
    //| \field(Type \type, list[Expression] fragments)
    //| \initializer(Statement initializerBody)
    //| \method(Type \return, str name, list[Declaration] parameters, list[Expression] exceptions, Statement impl)
    //| \method(Type \return, str name, list[Declaration] parameters, list[Expression] exceptions)
    //| \constructor(str name, list[Declaration] parameters, list[Expression] exceptions, Statement impl)
    //| \import(str name)
    //| \package(str name)
    //| \package(Declaration parentPackage, str name)
    //| \variables(Type \type, list[Expression] \fragments)
    //| \typeParameter(str name, list[Type] extendsList)
    //| \annotationType(str name, list[Declaration] body)
    //| \annotationTypeMember(Type \type, str name)
    //| \annotationTypeMember(Type \type, str name, Expression defaultBlock)
    // initializers missing in parameter, is it needed in vararg?
    //| \parameter(Type \type, str name, int extraDimensions)
    //| \vararg(Type \type, str name)
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
    | \throw(Expression expression)         //throw exp, c++ only
    | \typeid(Expression expression)        //typeid exp, c++ only
    //| \typeof(Expression expression)        //typeof exp, deprecated
    | \alignOf(Expression expression)       //__alignOf(exp), gcc only
    | \sizeofParameterPack(Expression expression) //sizeof...(parameterpack), c++ only?
    | \noexcept(Expression expression)      //noexcept (exp), c++ only
    | \labelReference(Expression expression)//&&label, gcc only?
    
    | \cast(Type \type, Expression expression)
    
    | \name(str name)
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
    
    | \functionDeclarator(list[Expression] arguments)
    | \namedTypeSpecifier(str name)
    
    | \functionCall(Expression functionName, list[Expression] arguments)
    
    | \fieldReference(Expression fieldOwner, Expression nname, Type fieldType)
    
    | \nyi(str raw)
    
    //| \functionCall(Expression functionName, list[Expression] arguments)
    //| \arrayAccess(Expression array, Expression index)
    //| \newArray(Type \type, list[Expression] dimensions, Expression init)
    //| \newArray(Type \type, list[Expression] dimensions)
    //| \arrayInitializer(list[Expression] elements)
    //| \assignment(Expression lhs, str operator, Expression rhs)
    //| \cast(Type \type, Expression expression)
    //| \characterLiteral(str charValue)
    //| \newObject(Expression expr, Type \type, list[Expression] args, Declaration class)
    //| \newObject(Type \type, list[Expression] args)
    //| \qualifiedName(Expression qualifier, Expression expression)
    //| \conditional(Expression expression, Expression thenBranch, Expression elseBranch)
    //| \fieldAccess(bool isSuper, Expression expression, str name)
    //| \fieldAccess(bool isSuper, str name)
    //| \instanceof(Expression leftSide, Type rightSide)
    //| \methodCall(bool isSuper, str name, list[Expression] arguments)
    //| \methodCall(bool isSuper, Expression receiver, str name, list[Expression] arguments)
    //| \null()
    //| \number(str numberValue)
    //| \booleanLiteral(bool boolValue)
    //| \stringLiteral(str stringValue)
    //| \type(Type \type)
    //| \variable(str name, int extraDimensions)
    //| \variable(str name, int extraDimensions, Expression \initializer)
    //| \bracket(Expression expression)
    //| \this()
    //| \this(Expression thisExpression)
    //| \super()
    //| \declarationExpression(Declaration declaration)
    //| \infix(Expression lhs, str operator, Expression rhs)
    //| \postfix(Expression operand, str operator)
    //| \prefix(str operator, Expression operand)
    //| \simpleName(str name)
    //| \markerAnnotation(str typeName)
    //| \normalAnnotation(str typeName, list[Expression] memberValuePairs)
    //| \memberValuePair(str name, Expression \value)             
    //| \singleMemberAnnotation(str typeName, Expression \value)
    ;                       
  
data Statement              
    = \compoundStatement(list[Statement] statements)
    | \declarationStatement(Declaration declaration)
    | \expressionStatement(Expression expression)
    | \if(Expression condition, Statement thenClause)
    | \if(Expression condition, Statement thenClause, Statement elseClause)
    | \for(Statement initializer, Expression condition, Expression iteration, Statement body)
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
    | \label(str name, Statement nestedStatement)
    | \goto(str name)
    
    
    //| \assert(Expression expression)
    //| \assert(Expression expression, Expression message)
    //| \block(list[Statement] statements)
    //| \break(str label)
    //| \continue()
    //| \continue(str label)
    //| \do(Statement body, Expression condition)
    //| \empty()
    //| \foreach(Declaration parameter, Expression collection, Statement body)
    //| \for(list[Expression] initializers, list[Expression] updaters, Statement body)
    //| \label(str name, Statement body)
    //| \return(Expression expression)
    //| \return()
    //| \switch(Expression expression, list[Statement] statements)
    //| \case(Expression expression)
    //| \defaultCase()
    //| \synchronizedStatement(Expression lock, Statement body)
    //| \throw(Expression expression)
    //| \try(Statement body, list[Statement] catchClauses)
    //| \try(Statement body, list[Statement] catchClauses, Statement \finally)                                        
    //| \catch(Declaration exception, Statement body)
    //| \declarationStatement(Declaration declaration)
    //| \while(Expression condition, Statement body)
    //| \expressionStatement(Expression stmt)
    //| \constructorCall(bool isSuper, Expression expr, list[Expression] arguments)
    //| \constructorCall(bool isSuper, list[Expression] arguments)
    ;           
  
data Type 
    = \unspecified()
    | \void()
    | \char()
    | \int()
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
    
    //| arrayType(Type \type)
    //| parameterizedType(Type \type)
    //| qualifiedType(Type qualifier, Expression simpleName)
    //| simpleType(Expression typeName)
    //| unionType(list[Type] types)
    //| wildcard()
    //| upperbound(Type \type)
    //| lowerbound(Type \type)
    //| \int()
    //| short()
    //| long()
    //| float()
    //| double()
    //| char()
    //| string()
    //| byte()
    //| \void()
    //| \boolean()
    ;
 
data Modifier
    = typedef()
    | \extern()
    | \static()
    //| \auto()
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
    
    
    //| \private()
    //| \public()
    //| \protected()
    //| \friendly()
    //| \static()
    //| \final()
    //| \synchronized()
    //| \transient()
    //| \abstract()
    //| \native()
    //| \volatile()
    //| \strictfp()
    //| \annotation(Expression \anno)
    //| \onDemand()
    //| \default()
    ;
    
@javaClass{lang.cpp.internal.Parser}  
@reflect{need access to streams}   
java Declaration parseCpp(loc file);

