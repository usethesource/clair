package lang.cpp.internal;

import java.net.URISyntaxException;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier.IASTEnumerator;
import org.eclipse.cdt.core.dom.ast.IASTFieldReference;
import org.eclipse.cdt.core.dom.ast.IASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.IASTIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTLabelStatement;
import org.eclipse.cdt.core.dom.ast.IASTNameOwner;
import org.eclipse.cdt.core.dom.ast.IASTNamedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorMacroDefinition;
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
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTAliasDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCapture;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCompositeTypeSpecifier.ICPPASTBaseSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTConstructorChainInitializer;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamespaceAlias;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamespaceDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTPointerToMember;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTQualifiedName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTSimpleTypeTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateId;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplatedTypeTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDirective;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPAliasTemplateInstance;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPBinding;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPEnumeration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPField;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFieldTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunction;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPMember;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPNamespace;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateNonTypeParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPUsingDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPVariable;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPVariableInstance;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPVariableTemplate;
import org.eclipse.cdt.core.index.IIndexBinding;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.value.ISourceLocation;
import org.rascalmpl.value.IValueFactory;
import org.rascalmpl.values.ValueFactoryFactory;

public class BindingsResolver {
	private final IValueFactory vf = ValueFactoryFactory.getValueFactory();
	public final ISourceLocation UNKNOWN = makeBinding("unknown", null, null);
	public final ISourceLocation NYI = makeBinding("nyi", null, null);
	private IEvaluatorContext ctx;

	static int prefix = 0;
	ISourceLocation sourceLoc = UNKNOWN;

	public void setSourceLocation(ISourceLocation loc) {
		sourceLoc = loc;
	}

	static String spaces() {
		return StringUtils.repeat(" ", prefix);
	}

	private void out(String msg) {
		ctx.getStdOut().println(spaces() + msg.replace("\n", "\n" + spaces()));
	}

	private void err(String msg) {
		ctx.getStdErr().println(spaces() + msg.replace("\n", "\n" + spaces()));
	}

	private ISourceLocation resolveOwner(IBinding binding) throws URISyntaxException {
		if (binding == null)
			return sourceLoc;
		IBinding owner = binding.getOwner();
		if (owner == null)
			return sourceLoc;
		else
			return resolveBinding(owner);
	}

	public ISourceLocation resolveBinding(IBinding binding) throws URISyntaxException {
		if (binding instanceof ICExternalBinding)
			return resolveICExternalBinding((ICExternalBinding) binding);
		if (binding instanceof ICompositeType)
			return resolveICompositeType((ICompositeType) binding);
		// CPPBinding moved to bottom
		// Discouraged access
		// if (binding instanceof ICPPTwoPhaseBinding)
		// return resolveICPPTwoPhaseBinding((ICPPTwoPhaseBinding) binding);
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

		if (binding instanceof ICPPBinding)
			return resolveICPPBinding((ICPPBinding) binding);
		return UNKNOWN;
	}

	private ISourceLocation resolveIVariable(IVariable binding) throws URISyntaxException {
		if (binding instanceof ICPPVariable)
			return resolveICPPVariable((ICPPVariable) binding);
		throw new RuntimeException("NYI: IVariable");
	}

	private ISourceLocation resolveITypedef(ITypedef binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveIProblemBinding(IProblemBinding binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		return NYI;
	}

	private ISourceLocation resolveIMacroBinding(IMacroBinding binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveILabel(ILabel binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveIIndexBinding(IIndexBinding binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveIFunction(IFunction binding) throws URISyntaxException {
		if (binding instanceof ICPPFunction) {
			ISourceLocation owner = resolveOwner(binding);
			return vf.sourceLocation("cpp+function", owner.getAuthority(), owner.getPath() + "/" + binding.getName());
		}
		throw new RuntimeException("NYI: C IFunction");
	}

	private ISourceLocation resolveIEnumerator(IEnumerator binding) throws URISyntaxException {
		IBinding owner = binding.getOwner();
		ISourceLocation ownerDecl = resolveBinding(owner);
		return vf.sourceLocation("cpp+enumerator", null, ownerDecl.getPath() + "/" + binding.getName().toString());
	}

	private ISourceLocation resolveIEnumeration(IEnumeration binding) {
		if (binding instanceof ICPPEnumeration)
			return resolveICPPEnumeration((ICPPEnumeration) binding);
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPBinding(ICPPBinding binding) throws URISyntaxException {
		if (binding instanceof ICPPAliasTemplateInstance)
			return resolveICPPAliasTemplateInstance((ICPPAliasTemplateInstance) binding);
		if (binding instanceof ICPPClassType)
			return resolveICPPClassType((ICPPClassType) binding);
		if (binding instanceof ICPPEnumeration)
			return resolveICPPEnumeration((ICPPEnumeration) binding);
		if (binding instanceof ICPPFunction)
			return resolveICPPFunction((ICPPFunction) binding);
		// Discouraged access
		// if (binding instanceof ICPPInternalBinding)
		// return resolveICPPInternalBinding((ICPPInternalBinding) binding);
		if (binding instanceof ICPPMember)
			return resolveICPPMember((ICPPMember) binding);
		if (binding instanceof ICPPNamespace)
			return resolveICPPNamespace((ICPPNamespace) binding);
		if (binding instanceof ICPPSpecialization)
			return resolveICPPSpecialization((ICPPSpecialization) binding);
		if (binding instanceof ICPPTemplateDefinition)
			return resolveICPPTemplateDefinition((ICPPTemplateDefinition) binding);
		if (binding instanceof ICPPTemplateParameter)
			return resolveICPPTemplateParameter((ICPPTemplateParameter) binding);
		// Discouraged access
		// if (binding instanceof ICPPUnknownBinding)
		// return resolveICPPUnknownBinding((ICPPUnknownBinding) binding);
		if (binding instanceof ICPPUsingDeclaration)
			return resolveICPPUsingDeclaration((ICPPUsingDeclaration) binding);
		if (binding instanceof ICPPVariable)
			return resolveICPPVariable((ICPPVariable) binding);
		return UNKNOWN;
	}

	private ISourceLocation resolveICPPVariable(ICPPVariable binding) throws URISyntaxException {
		if (binding instanceof ICPPField)
			return resolveICPPField((ICPPField) binding);
		// Discouraged access
		// if (binding instanceof ICPPInternalVariable)
		// return resolveICPPInternalVariable((ICPPInternalVariable) binding);
		if (binding instanceof ICPPParameter)
			return resolveICPPParameter((ICPPParameter) binding);
		if (binding instanceof ICPPTemplateNonTypeParameter)
			return resolveICPPTemplateNonTypeParameter((ICPPTemplateNonTypeParameter) binding);
		if (binding instanceof ICPPVariableInstance)
			return resolveICPPVariableInstance((ICPPVariableInstance) binding);
		if (binding instanceof ICPPVariableTemplate)
			return resolveICPPVariableTemplate((ICPPVariableTemplate) binding);
		ISourceLocation owner = resolveOwner(binding.getOwner());
		return vf.sourceLocation("cpp+variable", owner.getAuthority(), owner.getPath() + "/" + binding.getName());
	}

	private ISourceLocation resolveICPPField(ICPPField binding) throws URISyntaxException {
		if (binding instanceof ICPPFieldTemplate)
			throw new RuntimeException("NYI");
		ISourceLocation owner = resolveOwner(binding);
		return vf.sourceLocation("cpp+field", owner.getAuthority(), owner.getPath() + "/" + binding.getName());
	}

	private ISourceLocation resolveICPPParameter(ICPPParameter binding) {
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPTemplateNonTypeParameter(ICPPTemplateNonTypeParameter binding) {
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPVariableInstance(ICPPVariableInstance binding) {
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPVariableTemplate(ICPPVariableTemplate binding) {
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPUsingDeclaration(ICPPUsingDeclaration binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPTemplateParameter(ICPPTemplateParameter binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPTemplateDefinition(ICPPTemplateDefinition binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPSpecialization(ICPPSpecialization binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPNamespace(ICPPNamespace binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPMember(ICPPMember binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPFunction(ICPPFunction binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private String flatten(String[] fqn) {
		StringBuilder ret = new StringBuilder();
		Stream.of(fqn).forEach(it -> ret.append(it));
		ret.deleteCharAt(ret.length() - 1);
		return ret.toString();
	}

	private ISourceLocation resolveICPPEnumeration(ICPPEnumeration binding) {
		try {
			ISourceLocation owner = resolveOwner(binding);
			ISourceLocation loc = vf.sourceLocation("cpp+enum", owner.getAuthority(),
					owner.getPath() + "/" + binding.getName());
			return loc;
		} catch (URISyntaxException e) {
			return UNKNOWN;
		}
	}

	private ISourceLocation resolveICPPClassType(ICPPClassType binding) throws URISyntaxException {
		if (binding instanceof ICPPClassSpecialization)
			return resolveICPPClassSpecialization((ICPPClassSpecialization) binding);
		if (binding instanceof ICPPClassTemplate)
			return resolveICPPClassTemplate((ICPPClassTemplate) binding);
		// Discouraged access
		// if (binding instanceof ICPPDeferredClassInstance)
		// return resolveICPPDeferredClassInstance((ICPPDeferredClassInstance)
		// binding);
		// Not visible
		// if (binding instanceof ICPPInternalClassTypeMixinHost)
		// return
		// resolveICPPInternalClassTypeMixinHost((ICPPInternalClassTypeMixinHost)
		// binding);
		// Discouraged access
		// if (binding instanceof ICPPUnknownMemberClass)
		// return resolveICPPUnknownMemberClass((ICPPUnknownMemberClass)
		// binding);
		// Discouraged access
		// if (binding instanceof IPDOMCPPClassType)
		// return resolveIPDOMCPPClassType((IPDOMCPPClassType) binding);
		ISourceLocation owner = resolveOwner(binding);
		return vf.sourceLocation("cpp+class", owner.getAuthority(), owner.getPath() + "/" + binding.getName());
	}

	private ISourceLocation resolveICPPClassSpecialization(ICPPClassSpecialization binding) {
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPClassTemplate(ICPPClassTemplate binding) {
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPAliasTemplateInstance(ICPPAliasTemplateInstance binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICompositeType(ICompositeType binding) throws URISyntaxException {
		if (binding instanceof ICPPClassType)
			return resolveICPPClassType((ICPPClassType) binding);
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICExternalBinding(ICExternalBinding binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	public ISourceLocation resolveBinding(IASTNameOwner node) {
		try {
			if (node instanceof IASTCompositeTypeSpecifier)
				return resolveCompositeTypeSpecifier((IASTCompositeTypeSpecifier) node);
			if (node instanceof IASTDeclarator)
				return resolveDeclarator((IASTDeclarator) node);
			if (node instanceof IASTElaboratedTypeSpecifier)
				return resolveElaboratedTypeSpecifier((IASTElaboratedTypeSpecifier) node);
			if (node instanceof IASTEnumerationSpecifier)
				return resolveEnumerationSpecifier((IASTEnumerationSpecifier) node);
			if (node instanceof IASTEnumerator)
				return resolveEnumerator((IASTEnumerator) node);
			if (node instanceof IASTFieldReference)
				return resolveFieldReference((IASTFieldReference) node);
			if (node instanceof IASTGotoStatement)
				return resolveGotoStatement((IASTGotoStatement) node);
			if (node instanceof IASTIdExpression)
				return resolveIdExpression((IASTIdExpression) node);
			// Discouraged access
			// if (node instanceof IASTInternalNameOwner)
			// return resolveInternalNameOwner((IASTInternalNameOwner) node);
			if (node instanceof IASTLabelStatement)
				return resolveLabelStatement((IASTLabelStatement) node);
			if (node instanceof IASTNamedTypeSpecifier)
				return resolveNamedTypeSpecifier((IASTNamedTypeSpecifier) node);
			if (node instanceof IASTPreprocessorMacroDefinition)// TODO
				return resolvePreprocessorMacroDefinition((IASTPreprocessorMacroDefinition) node);
			if (node instanceof ICPPASTAliasDeclaration)
				return resolveAliasDeclaration((ICPPASTAliasDeclaration) node);
			if (node instanceof ICPPASTBaseSpecifier)
				return resolveBaseSpecifier((ICPPASTBaseSpecifier) node);
			if (node instanceof ICPPASTCapture)
				return resolveCapture((ICPPASTCapture) node);
			if (node instanceof ICPPASTConstructorChainInitializer)
				return resolveConstructorChainInitializer((ICPPASTConstructorChainInitializer) node);
			if (node instanceof ICPPASTNamespaceAlias)
				return resolveNamespaceAlias((ICPPASTNamespaceAlias) node);
			if (node instanceof ICPPASTNamespaceDefinition)
				return resolveNamespaceDefinition((ICPPASTNamespaceDefinition) node);
			if (node instanceof ICPPASTPointerToMember)// TODO
				return resolvePointerToMember((ICPPASTPointerToMember) node);
			if (node instanceof ICPPASTQualifiedName)
				return resolveQualifiedName((ICPPASTQualifiedName) node);
			if (node instanceof ICPPASTSimpleTypeTemplateParameter)
				return resolveSimpleTypeTemplateParameter((ICPPASTSimpleTypeTemplateParameter) node);
			if (node instanceof ICPPASTTemplatedTypeTemplateParameter)// TODO,
																		// NYI
				return resolveTemplatedTypeTemplateParameter((ICPPASTTemplatedTypeTemplateParameter) node);
			if (node instanceof ICPPASTTemplateId)
				return resolveTemplateId((ICPPASTTemplateId) node);
			// Deprecated // TODO?
			// if (node instanceof ICPPASTTypenameExpression)
			// return resolveTypenameExpression((ICPPASTTypenameExpression)
			// node);
			if (node instanceof ICPPASTUsingDeclaration)
				return resolveUsingDeclaration((ICPPASTUsingDeclaration) node);
			if (node instanceof ICPPASTUsingDirective)
				return resolveUsingDirective((ICPPASTUsingDirective) node);
			// if (node instanceof IGNUASTGotoStatement)
			// return resolveGnuGotoStatement((IGNUASTGotoStatement) node);
		} catch (URISyntaxException e) {
			err("Caught URISyntaxException, return UNKNOWN");
			err(e.getMessage());
		}
		return UNKNOWN;
	}

	private ISourceLocation resolveUsingDeclaration(ICPPASTUsingDeclaration node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveUsingDirective(ICPPASTUsingDirective node) throws URISyntaxException {
		return resolveBinding(node.getQualifiedName().resolveBinding());
	}

	private ISourceLocation resolveTemplateId(ICPPASTTemplateId node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveTemplatedTypeTemplateParameter(ICPPASTTemplatedTypeTemplateParameter node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveSimpleTypeTemplateParameter(ICPPASTSimpleTypeTemplateParameter node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveQualifiedName(ICPPASTQualifiedName node) throws URISyntaxException {
		return resolveBinding(node.resolveBinding());
	}

	private ISourceLocation resolvePointerToMember(ICPPASTPointerToMember node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveNamespaceDefinition(ICPPASTNamespaceDefinition node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveNamespaceAlias(ICPPASTNamespaceAlias node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveConstructorChainInitializer(ICPPASTConstructorChainInitializer node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveCapture(ICPPASTCapture node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveBaseSpecifier(ICPPASTBaseSpecifier node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveAliasDeclaration(ICPPASTAliasDeclaration node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolvePreprocessorMacroDefinition(IASTPreprocessorMacroDefinition node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveNamedTypeSpecifier(IASTNamedTypeSpecifier node) throws URISyntaxException {
		IBinding binding = node.getName().resolveBinding();
		return resolveBinding(binding);
	}

	private ISourceLocation resolveLabelStatement(IASTLabelStatement node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveIdExpression(IASTIdExpression node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveGotoStatement(IASTGotoStatement node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveFieldReference(IASTFieldReference node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveEnumerator(IASTEnumerator node) throws URISyntaxException {
		IBinding binding = node.getName().resolveBinding();
		return resolveBinding(binding);
	}

	private ISourceLocation resolveEnumerationSpecifier(IASTEnumerationSpecifier node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveElaboratedTypeSpecifier(IASTElaboratedTypeSpecifier node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveDeclarator(IASTDeclarator node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveCompositeTypeSpecifier(IASTCompositeTypeSpecifier node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	public ISourceLocation makeBinding(String scheme, String authority, String path) {
		try {
			return vf.sourceLocation(scheme, authority, path);
		} catch (URISyntaxException e) {
			throw new RuntimeException("Should not happen", e);
		}
	}

	public void setIEvaluatorContext(IEvaluatorContext ctx) {
		this.ctx = ctx;
	}
}
