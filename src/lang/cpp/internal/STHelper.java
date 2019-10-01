package lang.cpp.internal;

import java.util.Map;

import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.env.Environment;
import org.rascalmpl.interpreter.result.Result;

import io.usethesource.vallang.IConstructor;
import io.usethesource.vallang.IList;
import io.usethesource.vallang.IString;
import io.usethesource.vallang.IValue;
import io.usethesource.vallang.IValueFactory;

public class STHelper {
	private final IValueFactory vf;

	public STHelper(IValueFactory vf) {
		this.vf = vf;
	}

	private IValue searchForVariable(IString name, Environment env) {
		Map<String, Result<IValue>> vars = env.getVariables();
		if (vars != null && vars.containsKey(name.getValue())) {
			return vars.get(name.getValue()).getValue();
		}
		if (env.getCallerScope() != null) {
			IValue var = searchForVariable(name, env.getCallerScope());
			if (var != null) {
				return var;
			}
		}
		if (env.getParent() != null) {
			IValue var = searchForVariable(name, env.getCallerScope());
			if (var != null) {
				return var;
			}
		}
		return null;
	}

	public IConstructor findSimpleVariable(IString name, IEvaluatorContext ctx) {
		return (IConstructor) searchForVariable(name, ctx.getCurrentEnvt());
	}

	public IList findListVariable(IString name, IEvaluatorContext ctx) {
		return (IList) searchForVariable(name, ctx.getCurrentEnvt());
	}

}
