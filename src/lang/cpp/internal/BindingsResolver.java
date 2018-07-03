/** 
 * Copyright (c) 2016-2018, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
 * All rights reserved. 
 *  
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
 *  
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
 *  
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 *  
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 */
package lang.cpp.internal;

import java.net.URISyntaxException;

import org.apache.commons.lang.StringUtils;
import org.eclipse.cdt.core.dom.ast.ASTTypeUtil;
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier.IASTEnumerator;
import org.eclipse.cdt.core.dom.ast.IASTFieldReference;
import org.eclipse.cdt.core.dom.ast.IASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.IASTIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTLabelStatement;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNameOwner;
import org.eclipse.cdt.core.dom.ast.IASTNamedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorMacroDefinition;
import org.eclipse.cdt.core.dom.ast.IBinding;
import org.eclipse.cdt.core.dom.ast.ICompositeType;
import org.eclipse.cdt.core.dom.ast.IEnumeration;
import org.eclipse.cdt.core.dom.ast.IEnumerator;
import org.eclipse.cdt.core.dom.ast.IFunction;
import org.eclipse.cdt.core.dom.ast.ILabel;
import org.eclipse.cdt.core.dom.ast.IMacroBinding;
import org.eclipse.cdt.core.dom.ast.IProblemBinding;
import org.eclipse.cdt.core.dom.ast.IType;
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
import org.eclipse.cdt.core.dom.ast.cpp.ICPPAliasTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPAliasTemplateInstance;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPBinding;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassTemplatePartialSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassTemplatePartialSpecializationSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPConstructor;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPConstructorSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPDeferredFunction;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPEnumeration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPEnumerationSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPField;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFieldTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunction;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunctionInstance;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunctionSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunctionTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPMember;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPMethod;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPMethodSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPNamespace;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPNamespaceAlias;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPPartialSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPPartiallySpecializable;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateNonTypeParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateTypeParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPUsingDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPVariable;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPVariableInstance;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPVariableTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPVariableTemplatePartialSpecialization;
import org.eclipse.cdt.core.index.IIndexBinding;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPDeferredClassInstance;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPInternalBinding;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPTwoPhaseBinding;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownBinding;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownMemberClass;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownMemberClassInstance;
import org.eclipse.cdt.internal.core.pdom.dom.cpp.IPDOMCPPClassType;
import org.eclipse.cdt.internal.core.pdom.dom.cpp.IPDOMCPPEnumType;
import org.eclipse.cdt.internal.core.pdom.dom.cpp.IPDOMCPPTemplateParameter;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.uri.URIUtil;
import org.rascalmpl.values.ValueFactoryFactory;

import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IValueFactory;

@SuppressWarnings("restriction")
public class BindingsResolver {
	private final IValueFactory vf = ValueFactoryFactory.getValueFactory();
	public final ISourceLocation UNKNOWN = makeBinding("UNKNOWN", null, null);
	public final ISourceLocation NYI = makeBinding("NYI", null, null);
	public final ISourceLocation FIXME = makeBinding("FIXME", null, null);
	private IEvaluatorContext ctx;

	private int prefix = 0;

	private String spaces() {
		return StringUtils.repeat(" ", prefix);
	}

	private void out(String msg) {
		// ctx.getStdOut().println(spaces() + msg.replace("\n", "\n" +
		// spaces()));
	}

	private void err(String msg) {
		// ctx.getStdErr().println(spaces() + msg.replace("\n", "\n" +
		// spaces()));
	}

	ISourceLocation resolveOwner(IBinding binding) throws URISyntaxException {
		if (binding == null)
			return URIUtil.rootLocation("cpp");
		IBinding owner = binding.getOwner();
		if (binding.equals(owner)) {
			// err("Binding " + binding + " has itself as owner??");
			return FIXME;
		}
		if (owner == null)
			return URIUtil.rootLocation("cpp");
		else
			return resolveBinding(owner);
	}

	public ISourceLocation resolveBinding(IBinding binding) throws URISyntaxException {
		ISourceLocation src = resolveBinding2(binding);
		if (true)
			return src;
		if (binding == null)
			return src;
		return vf.sourceLocation(URIUtil.changeAuthority(src.getURI(), System.identityHashCode(binding) + ""));
	}

	public ISourceLocation resolveBinding2(IBinding binding) throws URISyntaxException {
		if (binding instanceof ICExternalBinding)
			return resolveICExternalBinding((ICExternalBinding) binding);
		if (binding instanceof ICompositeType)
			return resolveICompositeType((ICompositeType) binding);
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
		if (binding instanceof ICPPTwoPhaseBinding)
			return resolveICPPTwoPhaseBinding((ICPPTwoPhaseBinding) binding);
		// TODO: throw Exception here
		return makeBinding("UNKNOWN1", null, null);
	}

	private ISourceLocation resolveICPPTwoPhaseBinding(ICPPTwoPhaseBinding binding) {
		throw new RuntimeException("Trying to resolve ICPPTwoPhaseBinding " + binding.getClass().getSimpleName());
	}

	private ISourceLocation resolveIVariable(IVariable binding) throws URISyntaxException {
		if (binding instanceof ICPPVariable)
			return resolveICPPVariable((ICPPVariable) binding);
		throw new RuntimeException("NYI: IVariable");
	}

	private ISourceLocation resolveITypedef(ITypedef binding) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPAliasTemplateInstance)
			scheme = "cpp+aliasTemplateInstance";
		else
			scheme = "cpp+typedef";
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), scheme);
	}

	private ISourceLocation resolveIProblemBinding(IProblemBinding binding) {
		err("IProblemBinding: " + binding.toString());
		return makeBinding("cpp+problem", binding.getMessage(), null);
	}

	private ISourceLocation resolveIMacroBinding(IMacroBinding binding) throws URISyntaxException {
		if (binding.isDynamic()) {
			String className = binding.getClass().getSimpleName();
			if ("CounterMacro".equals(className))
				return makeBinding("cpp+dynamicMacro", null, "counter");
			if ("DateMacro".equals(className))
				return makeBinding("cpp+dynamicMacro", null, "date");
			if ("FileMacro".equals(className))
				return makeBinding("cpp+dynamicMacro", null, "file");
			if ("LineMacro".equals(className))
				return makeBinding("cpp+dynamicMacro", null, "line");
			if ("TimeMacro".equals(className))
				return makeBinding("cpp+dynamicMacro", null, "time");
			err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
			throw new RuntimeException("Encountered unknown dynamic MacroBinding " + className);
		}
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), "cpp+macro");
	}

	private ISourceLocation resolveILabel(ILabel binding) throws URISyntaxException {
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), "cpp+label");
	}

	private ISourceLocation resolveIIndexBinding(IIndexBinding binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveIFunction(IFunction binding) throws URISyntaxException {
		if (binding instanceof ICPPFunction)
			return resolveICPPFunction((ICPPFunction) binding);
		throw new RuntimeException("NYI: C IFunction");
	}

	private ISourceLocation resolveIEnumerator(IEnumerator binding) throws URISyntaxException {
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()),
				"cpp+enumerator");
	}

	private ISourceLocation resolveIEnumeration(IEnumeration binding) throws URISyntaxException {
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
		if (binding instanceof ICPPUsingDeclaration)
			return resolveICPPUsingDeclaration((ICPPUsingDeclaration) binding);
		if (binding instanceof ICPPVariable)
			return resolveICPPVariable((ICPPVariable) binding);
		if (binding instanceof ICPPInternalBinding)
			return resolveICPPInternalBinding((ICPPInternalBinding) binding);
		if (binding instanceof ICPPUnknownBinding)
			return resolveICPPUnknownBinding((ICPPUnknownBinding) binding);
		return makeBinding("UNKNOWN3", null, null);
	}

	private ISourceLocation resolveICPPInternalBinding(ICPPInternalBinding binding) throws URISyntaxException {
		String scheme = "cpp+internal";
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), scheme);
	}

	private ISourceLocation resolveICPPUnknownBinding(ICPPUnknownBinding binding) {
		throw new RuntimeException("Trying to resolve ICPPUnknownBinding");
	}

	private ISourceLocation resolveICPPVariable(ICPPVariable binding) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPField) {
			if (binding instanceof ICPPFieldTemplate)
				scheme = "cpp+fieldTemplate";
			else
				scheme = "cpp+field";
		} else if (binding instanceof ICPPParameter)
			scheme = "cpp+parameter";
		else if (binding instanceof ICPPTemplateNonTypeParameter)
			scheme = "cpp+templateNonTypeParameter";
		else if (binding instanceof ICPPVariableInstance)
			scheme = "cpp+variableInstance";
		else if (binding instanceof ICPPVariableTemplate) {
			if (binding instanceof ICPPFieldTemplate)
				scheme = "cpp+fieldTemplate";
			else if (binding instanceof ICPPVariableTemplatePartialSpecialization)
				scheme = "cpp+variableTemplatePartialSpec";
			else
				scheme = "cpp+variableTemplate";
		} else
			scheme = "cpp+variable";
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), scheme);
	}

	private ISourceLocation resolveICPPUsingDeclaration(ICPPUsingDeclaration binding) throws URISyntaxException {
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()),
				"cpp+usingDeclaration");
	}

	private ISourceLocation resolveICPPTemplateParameter(ICPPTemplateParameter binding) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPTemplateNonTypeParameter)
			return resolveICPPVariable((ICPPTemplateNonTypeParameter) binding);
		else if (binding instanceof ICPPTemplateTemplateParameter)
			return resolveICPPClassType((ICPPTemplateTemplateParameter) binding);
		else if (binding instanceof ICPPTemplateTypeParameter)
			scheme = "cpp+templateTypeParameter";
		else if (binding instanceof IPDOMCPPTemplateParameter)
			throw new RuntimeException("resolveICPPTemplateParameter encountered IPDOMCPPTemplateParameter");
		else
			scheme = "cpp+templateParameter";
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), scheme);
	}

	private ISourceLocation resolveICPPTemplateDefinition(ICPPTemplateDefinition binding) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPAliasTemplate)
			scheme = "cpp+aliasTemplate";
		else if (binding instanceof ICPPFunctionTemplate)
			scheme = "cpp+functionTemplate";
		else if (binding instanceof ICPPPartiallySpecializable) {
			if (binding instanceof ICPPClassTemplate)
				return resolveICPPClassType((ICPPClassTemplate) binding);
			else if (binding instanceof ICPPVariableTemplate)
				return resolveICPPVariable((ICPPVariableTemplate) binding);
			else
				throw new RuntimeException("resolveICPPTemplateDefinition encountered unknown type");
		} else if (binding instanceof ICPPPartialSpecialization) {
			if (binding instanceof ICPPClassTemplatePartialSpecialization)
				return resolveICPPClassType((ICPPClassTemplatePartialSpecialization) binding);
			else if (binding instanceof ICPPVariableTemplatePartialSpecialization)
				return resolveICPPVariable((ICPPVariableTemplatePartialSpecialization) binding);
			else
				throw new RuntimeException("resolveICPPTemplateDefinition encountered unknown type");
		} else
			scheme = "cpp+templateDefinition";
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), scheme);
	}

	private ISourceLocation resolveICPPSpecialization(ICPPSpecialization binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICPPNamespace(ICPPNamespace binding) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPNamespaceAlias)
			scheme = "cpp+namespaceAlias";
		else
			scheme = "cpp+namespace";
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), scheme);
	}

	private ISourceLocation resolveICPPMember(ICPPMember binding) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private String printType(IType type) {
		// ICPPBasicType, CPPPointerType, ICPPReferenceType, CPPQualifierType
		if (type instanceof ICPPBinding) // ITypedef
			return ASTTypeUtil.getQualifiedName((ICPPBinding) type);
		return type.toString().replace(" ", ".");
	}

	private ISourceLocation resolveICPPFunction(ICPPFunction binding) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPDeferredFunction)
			scheme = "cpp+deferredFunction";
		else if (binding instanceof ICPPFunctionInstance)
			scheme = "cpp+functionInstance";
		else if (binding instanceof ICPPFunctionSpecialization)
			scheme = "cpp+functionSpecialization";
		else if (binding instanceof ICPPFunctionTemplate)
			scheme = "cpp+functionTemplate";
		else if (binding instanceof ICPPMethod) {
			if (binding instanceof ICPPConstructor) {
				if (binding instanceof ICPPConstructorSpecialization)
					scheme = "cpp+constructorSpecialization";
				else
					scheme = "cpp+constructor";
			} else if (binding instanceof ICPPMethodSpecialization)
				scheme = "cpp+methodSpecialization";
			else
				scheme = "cpp+method";
		} else
			scheme = "cpp+function";

		StringBuilder parameters = new StringBuilder("(");
		for (ICPPParameter parameter : binding.getParameters()) {
			if (parameters.length() > 1)
				parameters.append(',');
			parameters.append(printType(parameter.getType()));
		}
		parameters.append(')');

		ISourceLocation decl = URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()),
				scheme);
		decl = URIUtil.changePath(decl, decl.getPath() + parameters.toString());
		return decl;
	}

	private ISourceLocation resolveICPPEnumeration(ICPPEnumeration binding) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPEnumerationSpecialization)
			scheme = "cpp+enumSpecialization";
		else if (binding instanceof IPDOMCPPEnumType)
			throw new RuntimeException("resolveICPPEnumeration encountered IPDOMCPPEnumType");
		else
			scheme = "cpp+enum";
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), scheme);
	}

	private ISourceLocation resolveICPPClassType(ICPPClassType binding) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPClassSpecialization) {
			if (binding instanceof ICPPClassTemplatePartialSpecializationSpecialization)
				scheme = "cpp+classTemplatePartialSpecSpec";
			else
				scheme = "cpp+classSpecialization";
		} else if (binding instanceof ICPPClassTemplate) {
			if (binding instanceof ICPPClassTemplatePartialSpecialization)
				scheme = "cpp+classTemplatePartialSpec";
			else if (binding instanceof ICPPTemplateTemplateParameter)
				scheme = "cpp+templateTemplateParameter";
			else
				scheme = "cpp+classTemplate";
		} else if (binding instanceof ICPPDeferredClassInstance)
			scheme = "cpp+deferredClassInstance";
		else if (binding instanceof ICPPUnknownMemberClass) {
			if (binding instanceof ICPPUnknownMemberClassInstance)
				scheme = "cpp+unknownMemberClassInstance";
			else
				scheme = "cpp+unknownMemberClass";
		} else if (binding instanceof IPDOMCPPClassType)
			throw new RuntimeException("resolveICPPClassType encountered IPDOMCPPClassType");
		else
			scheme = "cpp+class";
		return URIUtil.changeScheme(URIUtil.getChildLocation(resolveOwner(binding), binding.getName()), scheme);
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
		ISourceLocation decl = resolveBinding2(node);
		if (decl.getScheme().equals("cpp+problem")) {
			out(((IASTNode) node).getRawSignature());
			// throw new RuntimeException();
		}
		// if (decl.getScheme().equals("cpp+problem")) {
		// IASTNode n = (IASTNode) node;
		// IASTNodeLocation[] nodeLocations = n.getNodeLocations();
		// if (nodeLocations.length == 1 && nodeLocations[0] instanceof
		// IASTMacroExpansionLocation) {
		// try {
		// return resolveBinding(((IASTMacroExpansionLocation)
		// nodeLocations[0]).getExpansion()
		// .getMacroReference().resolveBinding());
		// } catch (URISyntaxException e) {
		// err("Failed to get Macro binding for " + n);
		// }
		// }
		// }
		return decl;
	}

	public ISourceLocation resolveBinding2(IASTNameOwner node) {
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
		return makeBinding("UNKNOWN4", null, null);
	}

	private ISourceLocation resolveUsingDeclaration(ICPPASTUsingDeclaration node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveUsingDirective(ICPPASTUsingDirective node) throws URISyntaxException {
		return resolveBinding(node.getQualifiedName().resolveBinding());
	}

	private ISourceLocation resolveTemplateId(ICPPASTTemplateId node) throws URISyntaxException {
		return resolveBinding(node.resolveBinding());
	}

	private ISourceLocation resolveTemplatedTypeTemplateParameter(ICPPASTTemplatedTypeTemplateParameter node)
			throws URISyntaxException {
		return resolveBinding(node.getName().getBinding());
	}

	private ISourceLocation resolveSimpleTypeTemplateParameter(ICPPASTSimpleTypeTemplateParameter node)
			throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveQualifiedName(ICPPASTQualifiedName node) throws URISyntaxException {
		return resolveBinding(node.resolveBinding());
	}

	private ISourceLocation resolvePointerToMember(ICPPASTPointerToMember node) {
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveNamespaceDefinition(ICPPASTNamespaceDefinition node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveNamespaceAlias(ICPPASTNamespaceAlias node) throws URISyntaxException {
		return resolveBinding(node.getAlias().resolveBinding());
	}

	private ISourceLocation resolveConstructorChainInitializer(ICPPASTConstructorChainInitializer node)
			throws URISyntaxException {
		return resolveBinding(node.getMemberInitializerId().resolveBinding());
	}

	private ISourceLocation resolveCapture(ICPPASTCapture node) throws URISyntaxException {
		IASTName name = node.getIdentifier();
		if (name == null) {
			out("Resolving this capture; returning dummy value");
			return FIXME;
		}
		return resolveBinding(name.resolveBinding());
	}

	private ISourceLocation resolveBaseSpecifier(ICPPASTBaseSpecifier node) throws URISyntaxException {
		return resolveBinding(node.getNameSpecifier().resolveBinding());
	}

	private ISourceLocation resolveAliasDeclaration(ICPPASTAliasDeclaration node) throws URISyntaxException {
		return resolveBinding(node.getAlias().resolveBinding());
	}

	private ISourceLocation resolvePreprocessorMacroDefinition(IASTPreprocessorMacroDefinition node) {
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveNamedTypeSpecifier(IASTNamedTypeSpecifier node) throws URISyntaxException {
		IBinding binding = node.getName().resolveBinding();
		return resolveBinding(binding);
	}

	private ISourceLocation resolveLabelStatement(IASTLabelStatement node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveIdExpression(IASTIdExpression node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveGotoStatement(IASTGotoStatement node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveFieldReference(IASTFieldReference node) throws URISyntaxException {
		return resolveBinding(node.getFieldName().resolveBinding());
	}

	private ISourceLocation resolveEnumerator(IASTEnumerator node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveEnumerationSpecifier(IASTEnumerationSpecifier node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveElaboratedTypeSpecifier(IASTElaboratedTypeSpecifier node) throws URISyntaxException {
		return resolveBinding(node.getName().resolveBinding());
	}

	private ISourceLocation resolveDeclarator(IASTDeclarator node) throws URISyntaxException {
		if (node.getName() == null) {
			out("resolveDeclarator has null name. " + node.getClass().getSimpleName() + ": " + node.getRawSignature());
			return FIXME;
		}

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
