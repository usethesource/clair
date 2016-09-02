package lang.cpp.internal;

import java.io.IOException;
import java.io.InputStream;

import org.rascalmpl.interpreter.TypeReifier;
import org.rascalmpl.interpreter.utils.RuntimeExceptionFactory;
import org.rascalmpl.uri.URIResolverRegistry;
import org.rascalmpl.value.IBool;
import org.rascalmpl.value.IConstructor;
import org.rascalmpl.value.ISourceLocation;
import org.rascalmpl.value.IValue;
import org.rascalmpl.value.IValueFactory;
import org.rascalmpl.value.type.TypeStore;

public class Parser {
  private IValueFactory vf;

  public Parser(IValueFactory vf) {
    this.vf = vf;
  }
  
  public IValue parseCpp(ISourceLocation file, IValue definitions, IBool optie1) {
    TypeStore store = new TypeStore();
    new TypeReifier(vf).valueToType((IConstructor) definitions, store);
    // the store moet naar de AST builder class voor de definities.
    
    // put example code here
    try (InputStream in = URIResolverRegistry.getInstance().getInputStream(file)) {
      // TODO: lots of work here
    } catch (IOException e) {
      throw RuntimeExceptionFactory.io(vf.string(e.getMessage()), null, null);
    }
    
    return vf.string("todo");
  }
}
