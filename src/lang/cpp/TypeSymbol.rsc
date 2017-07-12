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
  
  | \templateTypeParameter(str ownerName, str name)
  | \deferredClassInstance(str name)
  | \unknownMemberClass(TypeSymbol owner, str name)
  
  | \typeOfDependentExpression(loc src)
  | \problemBinding()
  | \problemType()
  | \noType()
  
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