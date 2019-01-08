import java.util.Collections;
import java.util.Map;

import org.rascalmpl.interpreter.IEvaluatorContext;

import io.usethesource.vallang.IBool;
import io.usethesource.vallang.INode;
import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IValueFactory;

public class TreeRewriterHelper {
	private final IValueFactory vf;

	public TreeRewriterHelper(IValueFactory vf) {
		this.vf = vf;
	}

	private Map<ISourceLocation, INode> getExternalConcretePatterns(ISourceLocation loc, IEvaluatorContext ctx) {
		String module = ctx.getHeap().getModuleForURI(loc.getURI());
		if (module == null) {
			return Collections.emptyMap();
		}
		return ctx.getHeap().getModule(module).getExternalConcretePatterns();
	}

	public INode getConcreteSyntaxImage(ISourceLocation loc, IEvaluatorContext ctx) {
		return getExternalConcretePatterns(loc, ctx).get(loc);
	}

	public IBool isConcreteSyntaxPattern(ISourceLocation loc, IEvaluatorContext ctx) {
		return vf.bool(getExternalConcretePatterns(loc, ctx).containsKey(loc));
	}
}
