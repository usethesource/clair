import java.util.Collections;
import java.util.Map;

import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.env.ModuleEnvironment;

import io.usethesource.vallang.IBool;
import io.usethesource.vallang.IMapWriter;
import io.usethesource.vallang.ISet;
import io.usethesource.vallang.ISetWriter;
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
		return ctx.getHeap().getModule(module).getExternalConcretePatterns();
	}

	public IValue getConcreteSyntaxImage(ISourceLocation loc, IEvaluatorContext ctx) {
		return getExternalConcretePatterns(loc, ctx).get(loc);
	}

	public IBool isConcreteSyntaxPattern(ISourceLocation loc, IEvaluatorContext ctx) {
		return vf.bool(getExternalConcretePatterns(loc, ctx).containsKey(loc));
	}

	public ISet getMatchBindings(ISourceLocation loc, IEvaluatorContext ctx) {
		ModuleEnvironment me = ctx.getHeap().getModule(ctx.getHeap().getModuleForURI(loc.top().getURI()));
		ISetWriter bindings = vf.setWriter();
		me.getMatchBindings().forEach(it -> {
			IMapWriter binding = vf.mapWriter();
			it.forEach((key, value) -> binding.put(vf.string(key), value));
			bindings.insert(binding.done());
		});
		return bindings.done();
	}

}
