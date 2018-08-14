@license{Copyright (c) 2016-2018, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
All rights reserved. 
 
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
 
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
  
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

}
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
  | \typedef(TypeSymbol \type)
  | \enumeration(loc decl)
  | \referenceType(TypeSymbol \type)
  | \parameterPackType(TypeSymbol \type)
  
  | \classSpecialization(loc decl, list[TypeSymbol] templateArguments)
  | \enumerationSpecialization(loc specializedBinding, list[TypeSymbol] templateArguments)
  
  | \templateTypeParameter(loc owner, loc decl)
  | \implicitTemplateTypeParameter(loc owner, int position) //no decl?
  | \deferredClassInstance(str name)
  | \unknownMemberClass(loc owner, str name)
  
  | \typeOfDependentExpression(loc src)
  | \problemBinding()
  | \problemType(str msg)
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
  
  | \aliasTemplate(loc decl, list[loc] templateParameters)
  
  | \functionSetType(loc decl, list[TypeSymbol] templateArguments)
  | \functionSetTypePointer(loc decl, list[TypeSymbol] templateArguments)
  
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