module lang::cpp::TypeSymbol

import lang::cpp::AST;

//extend analysis::m3::TypeSymbol;

data Bound = \unbounded();

public data TypeSymbol
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
  
  | \array(TypeSymbol baseType)
  | \array(TypeSymbol baseType, int size)
  | \basicType(list[TypeModifier] modifiers, TypeSymbol baseType)
  | \class(loc decl)
  | \union(loc decl)
  | \struct(list[TypeSymbol] fields)
  | \qualifierType(list[TypeModifier] modifiers, TypeSymbol \type)
  | \pointerType(list[TypeModifier] modifiers, TypeSymbol \type)
  | \functionType(TypeSymbol returnType, list[TypeSymbol] parameterTypes)
  | \functionTypeVarArgs(TypeSymbol returnType, list[TypeSymbol] parameterTypes)
  | \typeContainer(TypeSymbol \type)
  | \enumeration(loc decl)
  | \referenceType(TypeSymbol \type)
  | \parameterPackType(TypeSymbol \type)
  
  | \classSpecialization(loc decl, list[TypeSymbol] templateArguments)
  | \enumerationSpecialization(loc specializedBinding, list[TypeSymbol] templateArguments)
  
  | \templateTypeParameter(loc owner, loc decl)
  | \deferredClassInstance(str name)
  | \unknownMemberClass(loc owner, str name)
  
  | \typeOfDependentExpression(loc src)
  | \problemBinding()
  | \problemType()
  | \noType()
  
  | \cStructTemplate(loc decl, list[loc] templateParameters)
  | \cUnionTemplate(loc decl, list[loc] templateParameters)
  | \cClassTemplate(loc decl, list[loc] templateParameters)
  | \eStructTemplate(loc decl, list[loc] templateParameters)
  | \eUnionTemplate(loc decl, list[loc] templateParameters)
  | \eClassTemplate(loc decl, list[loc] templateParameters)
  | \eEnumTemplate(loc decl, list[loc] templateParameters)
  | \templateTemplate(TypeSymbol child, list[loc] templateParameters)
  | \functionTemplate(loc decl, list[loc] templateParameters)
  | \variableTemplate(loc decl, list[loc] templateParameters)
  
  | \unresolved()
  | \any()
  
  ;
  
public data TypeModifier
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