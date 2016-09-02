package lang.cpp.internal;

import org.rascalmpl.value.IValue;

public interface AST {
  IValue add(IValue lhs, IValue rhs);
}
