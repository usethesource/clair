package lang.cpp.internal;

import java.io.IOException;
import java.io.InputStream;

import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.utils.RuntimeExceptionFactory;
import org.rascalmpl.uri.URIResolverRegistry;
import org.rascalmpl.value.IBool;
import org.rascalmpl.value.ISourceLocation;
import org.rascalmpl.value.IValue;
import org.rascalmpl.value.IValueFactory;

public class Parser {
  private IValueFactory vf;
  private AST builder;

  public Parser(IValueFactory vf) {
    this.vf = vf;
    this.builder = new AST(vf);
  }
  
  public IValue parseCpp(ISourceLocation file, IBool optie1, IEvaluatorContext ctx) {
    
    // the store moet naar de AST builder class voor de definities.
    
    // put example code here
    try (InputStream in = URIResolverRegistry.getInstance().getInputStream(file)) {
      // TODO: lots of work here
      ctx.getStdErr().println(builder.Modifier_abstract());
      
    } catch (IOException e) {
      throw RuntimeExceptionFactory.io(vf.string(e.getMessage()), null, null);
    }
    
    // just an example declaration
    return builder.Declaration_class(vf.list());
  }
}
