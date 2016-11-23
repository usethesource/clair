package lang.cpp.internal;

import java.net.URISyntaxException;

import org.eclipse.cdt.core.dom.ast.IBinding;
import org.eclipse.cdt.core.dom.ast.ICompositeType;
import org.eclipse.cdt.core.dom.ast.IEnumeration;
import org.eclipse.cdt.core.dom.ast.IEnumerator;
import org.eclipse.cdt.core.dom.ast.IFunction;
import org.eclipse.cdt.core.dom.ast.ILabel;
import org.eclipse.cdt.core.dom.ast.IMacroBinding;
import org.eclipse.cdt.core.dom.ast.IProblemBinding;
import org.eclipse.cdt.core.dom.ast.ITypedef;
import org.eclipse.cdt.core.dom.ast.IVariable;
import org.eclipse.cdt.core.dom.ast.c.ICExternalBinding;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPBinding;
import org.eclipse.cdt.core.index.IIndexBinding;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPTwoPhaseBinding;
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPVisitor;
import org.rascalmpl.value.ISourceLocation;
import org.rascalmpl.value.IValueFactory;
import org.rascalmpl.values.ValueFactoryFactory;

public class BindingsResolver {
	private final IValueFactory vf = ValueFactoryFactory.getValueFactory();

	public ISourceLocation resolveBinding(IBinding binding) {
		if (binding instanceof ICExternalBinding)
			return resolveICExternalBinding((ICExternalBinding) binding);
		if (binding instanceof ICompositeType)
			return resolveICompositeType((ICompositeType) binding);
		if (binding instanceof ICPPBinding)
			return resolveICPPBinding((ICPPBinding) binding);
		if (binding instanceof ICPPTwoPhaseBinding)
			return resolveICPPTwoPhaseBinding((ICPPTwoPhaseBinding) binding);
		if (binding instanceof IEnumeration)
			return resolveIEnumeration((IEnumeration) binding);
		if (binding instanceof IEnumerator)
			return resolveIEnumerator((IEnumerator) binding);
		if (binding instanceof IFunction)
			return resolveIFunction((IFunction) binding);
		if (binding instanceof IIndexBinding)
			return resolveIIndexBinding((IIndexBinding) binding);
		if (binding instanceof ILabel)
			return resolveILabel((ILabel) binding);
		if (binding instanceof IMacroBinding)
			return resolveIMacroBinding((IMacroBinding) binding);
		if (binding instanceof IProblemBinding)
			return resolveIProblemBinding((IProblemBinding) binding);
		if (binding instanceof ITypedef)
			return resolveITypedef((ITypedef) binding);
		if (binding instanceof IVariable)
			return resolveIVariable((IVariable) binding);
		return makeBinding("unknown", null, null);
	}

	public ISourceLocation resolveICExternalBinding(ICExternalBinding binding) {
		return null;
	}

	public ISourceLocation resolveICompositeType(ICompositeType binding) {
		return null;
	}

	public ISourceLocation resolveICPPBinding(ICPPBinding binding) {
		CPPVisitor visitor = new CPPVisitor();
		bindig
		return null;
	}

	public ISourceLocation resolveICPPTwoPhaseBinding(ICPPTwoPhaseBinding binding) {
		return null;
	}

	public ISourceLocation resolveIEnumeration(IEnumeration binding) {
		return null;
	}

	public ISourceLocation resolveIEnumerator(IEnumerator binding) {
		return null;
	}

	public ISourceLocation resolveIFunction(IFunction binding) {
		ISourceLocation parent = resolveBinding(binding.getOwner());
		// |cpp+function://<parent>/<name>(<params>)|

		return null;
	}

	public ISourceLocation resolveIIndexBinding(IIndexBinding binding) {
		return null;
	}

	public ISourceLocation resolveILabel(ILabel binding) {
		return null;
	}

	public ISourceLocation resolveIMacroBinding(IMacroBinding binding) {
		return null;
	}

	public ISourceLocation resolveIProblemBinding(IProblemBinding binding) {
		return null;
	}

	public ISourceLocation resolveITypedef(ITypedef binding) {
		return null;
	}

	public ISourceLocation resolveIVariable(IVariable binding) {
		return null;
	}

	protected ISourceLocation makeBinding(String scheme, String authority, String path) {
		try {
			return vf.sourceLocation(scheme, authority, path);
		} catch (URISyntaxException e) {
			throw new RuntimeException("Should not happen", e);
		}
	}
}
