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
//		return ctx.getHeap().getModule(module).getExternalConcretePatterns();
	}

	public IValue getConcreteSyntaxImage(ISourceLocation loc, IEvaluatorContext ctx) {
		Map<ISourceLocation, IValue> patterns = getExternalConcretePatterns(loc, ctx);
		for (ISourceLocation l : patterns.keySet()) {
			if (loc.top().isEqual(l.top()) && loc.getOffset() == l.getOffset() && loc.getLength() == l.getLength()) {
				return patterns.get(l);
			}
		}
		throw new IllegalArgumentException("No concrete image for " + loc);
	}

	public IBool isConcreteSyntaxPattern(ISourceLocation loc, IEvaluatorContext ctx) {
		Map<ISourceLocation, IValue> patterns = getExternalConcretePatterns(loc, ctx);
		for (ISourceLocation l : patterns.keySet()) {
			if (loc.top().isEqual(l.top()) && loc.getOffset() == l.getOffset() && loc.getLength() == l.getLength()) {
				return vf.bool(true);
			}
		}
		return vf.bool(false);
	}
}
