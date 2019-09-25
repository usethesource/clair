package lang.cpp.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.env.Environment;
import org.rascalmpl.interpreter.result.AbstractFunction;
import org.rascalmpl.interpreter.result.Result;

import io.usethesource.vallang.IConstructor;
import io.usethesource.vallang.IString;
import io.usethesource.vallang.IValue;
import io.usethesource.vallang.IValueFactory;
import io.usethesource.vallang.type.Type;
import io.usethesource.vallang.type.TypeFactory;

public class STHelper {
	private final IValueFactory vf;

	public STHelper(IValueFactory vf) {
		this.vf = vf;
	}

	private IConstructor searchForVariable(IString name, Environment env) {
		Map<String, Result<IValue>> vars = env.getVariables();
		if (vars != null && vars.containsKey(name.getValue())) {
			return (IConstructor) vars.get(name.getValue()).getValue();
		}
		if (env.getCallerScope() != null) {
			IConstructor var = searchForVariable(name, env.getCallerScope());
			if (var != null) {
				return var;
			}
		}
		if (env.getParent() != null) {
			IConstructor var = searchForVariable(name, env.getCallerScope());
			if (var != null) {
				return var;
			}
		}
		return null;
	}

	public IConstructor findVariable(IString name, IEvaluatorContext ctx) {
		return searchForVariable(name, ctx.getCurrentEnvt());
//		Map<String, Result<IValue>> vars = ctx.getCurrentEnvt().getParent().getParent().getParent().getParent()
//				.getCallerScope().getParent().getVariables();
//		IConstructor variable = (IConstructor) vars.get(name).getValue();
//		return variable;
	}

	public IString substitute(IString source, IEvaluatorContext ctx) {
		List<Part> substrings = new ArrayList<>();
		String src = source.getValue();
		boolean inVariable = false;
		while (!src.equals("") && !ctx.isInterrupted() && !ctx.isCanceled()) {
			ctx.getStdOut().println("Src: \"" + src + "\"");
			if (!inVariable) {
				inVariable = true;
				int index = src.indexOf('[');
				if (index == -1) {
					substrings.add(new StringPart(src));
					src = "";
					break;
				}
				substrings.add(new StringPart(src.substring(0, index)));
				src = src.substring(index);
			} else {
				inVariable = false;
				int index = src.indexOf(']');
				substrings.add(new VariablePart(src.substring(1, index), ctx));
				src = src.substring(index + 1);
			}
		}
//		ctx.getStdOut().println(substrings.size());
//		substrings.stream().forEach(it -> ctx.getStdOut().println(it.yield()));
		return vf.string(substrings.stream().map(Part::yield).collect(Collectors.joining()));
	}

	interface Part {
		String yield();
	}

	class StringPart implements Part {
		private final String str;

		public StringPart(String str) {
			this.str = str;
		}

		@Override
		public String yield() {
			return str;
		}

		@Override
		public String toString() {
			return "STR(" + str + ")";
		}
	}

	class VariablePart implements Part {
		private final IEvaluatorContext ctx;
		private String var;

		public VariablePart(String var, IEvaluatorContext ctx) {
			this.var = var;
			this.ctx = ctx;
		}

		@Override
		public String yield() {
			Map<String, Result<IValue>> vars = ctx.getCurrentEnvt().getParent().getParent().getParent().getParent()
					.getCallerScope().getParent().getVariables();
			IValue variable = vars.get(var).getValue();
			ctx.getStdOut().println(variable);
			List<AbstractFunction> functions = new ArrayList<>();
			ctx.getCurrentEnvt().getAllFunctions(TypeFactory.getInstance().nodeType(), "yield", functions);
			Result<IValue> result = functions.get(0).call(new Type[] { TypeFactory.getInstance().nodeType() },
					new IValue[] { variable }, null);
			return ((IString) result.getValue()).getValue();
		}

		@Override
		public String toString() {
			return "VAR(" + var + ")";
		}
	}

}
