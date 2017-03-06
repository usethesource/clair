module lang::cpp::TypeSymbol

import lang::cpp::AST;

extend analysis::m3::TypeSymbol;

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
  | \basicType(list[Modifier] modifiers, TypeSymbol baseType)
  | \class(loc decl)
  | \struct(list[TypeSymbol] fields)
  | \qualifierType(list[Modifier] modifiers, TypeSymbol \type)
  | \pointerType(list[Modifier] modifiers, TypeSymbol \type)
  | \functionType(TypeSymbol returnType, list[TypeSymbol] parameterTypes, bool takesVarArgs)
  | \typeContainer(TypeSymbol \type)
  | \enumeration(loc decl)
  | \referenceType(TypeSymbol \type)
  | \parameterPackType(TypeSymbol \type)
  
  | \classSpecialization(loc decl, map[int,TypeSymbol] templateParameters)
  
  | \templateTypeParameter(str ownerName, str name)
  | \deferredClassInstance(str name)
  | \unknownMemberClass(TypeSymbol owner, str name)
  
  
  | \typeOfDependentExpression(loc src)
  | \problemBinding()
  | \problemType()
  | \noType()
  
  ;