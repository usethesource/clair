import java.util.Collections;
import java.util.Map;

import org.rascalmpl.interpreter.IEvaluatorContext;

import io.usethesource.vallang.IBool;
import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IValue;
import io.usethesource.vallang.IValueFactory;

public class TreeRewriterHelper {
	private final IValueFactory vf;

	public TreeRewriterHelper(IValueFactory vf) {
		this.vf = vf;
	}

	private Map<ISourceLocation, IValue> getExternalConcretePatterns(ISourceLocation loc, IEvaluatorContext ctx) {
		String module = ctx.getHeap().getModuleForURI(loc.getURI());
		if (module == null) {
			return Collections.emptyMap();
		}
		return Collections.emptyMap();
	}

	public IValue getConcreteSyntaxImage(ISourceLocation loc, IEvaluatorContext ctx) {
		return getExternalConcretePatterns(loc, ctx).get(loc);
	}

	public IBool isConcreteSyntaxPattern(ISourceLocation loc, IEvaluatorContext ctx) {
		return vf.bool(getExternalConcretePatterns(loc, ctx).containsKey(loc));
	}
}
