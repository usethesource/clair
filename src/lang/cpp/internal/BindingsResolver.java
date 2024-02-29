/** 
 * Copyright (c) 2016-2020, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
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

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.net.URISyntaxException;
import java.util.UUID;

import org.eclipse.cdt.core.dom.ast.ASTTypeUtil;
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier.IASTEnumerator;
import org.eclipse.cdt.core.dom.ast.IASTFieldReference;
import org.eclipse.cdt.core.dom.ast.IASTFileLocation;
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
import org.eclipse.cdt.core.dom.ast.IField;
import org.eclipse.cdt.core.dom.ast.IFunction;
import org.eclipse.cdt.core.dom.ast.ILabel;
import org.eclipse.cdt.core.dom.ast.IMacroBinding;
import org.eclipse.cdt.core.dom.ast.IParameter;
import org.eclipse.cdt.core.dom.ast.IProblemBinding;
import org.eclipse.cdt.core.dom.ast.IProblemType;
import org.eclipse.cdt.core.dom.ast.ISemanticProblem;
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
import org.eclipse.cdt.internal.core.dom.parser.c.CEnumeration;
import org.eclipse.cdt.internal.core.dom.parser.c.CFunction;
import org.eclipse.cdt.internal.core.dom.parser.c.CVariable;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPDeferredClassInstance;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPInternalBinding;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPTwoPhaseBinding;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownBinding;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownMemberClass;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownMemberClassInstance;
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPFunctionSet;
import org.eclipse.cdt.internal.core.pdom.dom.cpp.IPDOMCPPClassType;
import org.eclipse.cdt.internal.core.pdom.dom.cpp.IPDOMCPPEnumType;
import org.eclipse.cdt.internal.core.pdom.dom.cpp.IPDOMCPPTemplateParameter;
import org.rascalmpl.uri.URIUtil;

import io.usethesource.vallang.ISet;
import io.usethesource.vallang.ISetWriter;
import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IValueFactory;
import io.usethesource.vallang.exceptions.FactParseError;
import io.usethesource.vallang.exceptions.FactTypeUseException;
import io.usethesource.vallang.io.StandardTextReader;

public class BindingsResolver {
	private final IValueFactory vf;
	private final PrintWriter stdErr;

	public final ISourceLocation NYI;
	public final ISourceLocation FIXME;

	public ISetWriter containment;

	private ISourceLocation translationUnit;
	private ISourceLocation translationUnitRoot = URIUtil.rootLocation("cpp+translationUnit");

	private void out(String msg) {
//		stdOut.println(spaces() + msg.replace("\n", "\n" + spaces()));
	}

	private void err(String msg) {
//		stdErr.println(spaces() + msg.replace("\n", "\n" + spaces()));
	}

	/**
	 * This method can only be called once.
	 * @return containment relation build up as a side effect of binding resolution
	 */
	public ISet getContainmentRelation() {
		ISet result = containment.done();
		containment = vf.setWriter();
		return result;
	}


	/**
	 * Always call at the start of processing a translation unit.
	 * Will also clear the previous containment relation, if any.
	 * @param tu
	 */
	public void setTranslationUnit(ISourceLocation tu) {
		translationUnit = tu;
		containment = vf.setWriter();
	}

	public BindingsResolver(IValueFactory vf, PrintWriter stdOut, PrintWriter stdErr) {
		this.vf = vf;
		this.stdErr = stdErr;
		this.containment = vf.setWriter();
		this.translationUnit = URIUtil.rootLocation("cpp+translationUnit");
		this.NYI = makeBinding("NYI", null, null);
		this.FIXME = makeBinding("FIXME", null, null);
	}
	
	private ISourceLocation ownedBinding(IBinding binding, String scheme, ISourceLocation origin) throws URISyntaxException {
		return ownedBinding(binding, scheme, "", origin, false);
	}

	private ISourceLocation ownedBinding(IBinding binding, String scheme, String postfix, ISourceLocation origin, boolean isStatic) throws URISyntaxException {
		String name = renameOperators(binding.getName()) + postfix;
		ISourceLocation ownerLocation = resolveOwner(binding, origin);
		ISourceLocation location = null;
		boolean isAtRoot = "cpp+translationUnit".equals(ownerLocation.getScheme());

		// * When we are at the root, we want a different parent from |cpp+translationUnit:///| as containment parent; name it should contain the file name from `translationUnit`
		// * Also when we are the root, `static` variables and functions need to be prefixed with the file name because they are local to the current translationUnit

		if (isStatic) {
			if (isAtRoot) {
				// add prefix
				location = URIUtil.changeScheme(URIUtil.getChildLocation(translationUnit, name), scheme);
				// set long containment parent
				ownerLocation = translationUnit;
			}
			else {
				// the owner is the prefix; because we are inside some nested declaration
				location = URIUtil.changeScheme(URIUtil.getChildLocation(ownerLocation, name), scheme);	
			}
		}
		else {
			if (isAtRoot) {
				// do not add the prefix, because of global C[++] namespace while linking
				location = URIUtil.correctLocation(scheme, "", name);		
				// set long containment parent
				ownerLocation = translationUnit;
			}
			else {
				// the owner is the prefix; because we are inside some nested declaration
				location = URIUtil.changeScheme(URIUtil.getChildLocation(ownerLocation, name), scheme);	
			}
		}
		
		containment.append(vf.tuple(ownerLocation, location));
		return location;
	}
	
	private String renameOperators(String name) {
		if (isOperatorName(name)) {
			// wrap the operator in braces to disambiguate from possible methods with names like "operatordelete"
			name = "operator(" + name.substring("operator ".length()) + ")";
		}

		return name;
	}

	private ISourceLocation resolveOwner(IBinding binding, ISourceLocation origin) throws URISyntaxException {
		if (binding == null) {
			return translationUnitRoot;
		}

		IBinding owner = binding.getOwner();
		if (binding.equals(owner)) {
			return URIUtil.correctLocation("circular", "", UUID.randomUUID().toString());
		}
		if (owner == null) {
			return translationUnitRoot;
		}
		else {
			return resolveBinding(null, owner, origin);
		}
	}

	public static ISourceLocation failedBinding(String scheme) {
		return URIUtil.correctLocation(scheme, "", UUID.randomUUID().toString());
	}

	public ISourceLocation resolveBinding(IASTNameOwner owner, IBinding binding, ISourceLocation origin) {
		try {
			if (binding == null) {
				return failedBinding("unresolved");
			}
			if (binding instanceof ICExternalBinding) { 
				return resolveICExternalBinding((ICExternalBinding) binding, origin);
			}
			if (binding instanceof ICompositeType) {
				return resolveICompositeType((ICompositeType) binding, origin);
			}
			if (binding instanceof IEnumeration) {
				return resolveIEnumeration((IEnumeration) binding, origin);
			}
			if (binding instanceof IEnumerator) {
				return resolveIEnumerator((IEnumerator) binding, origin);
			}
			if (binding instanceof IFunction) {
				return resolveIFunction((IFunction) binding, origin);
			}
			if (binding instanceof IIndexBinding) {
				return resolveIIndexBinding((IIndexBinding) binding, origin);
			}
			if (binding instanceof ILabel) {
				return resolveILabel((ILabel) binding, origin);
			}
			if (binding instanceof IMacroBinding) {
				return resolveIMacroBinding((IMacroBinding) binding, origin);
			}
			if (binding instanceof IProblemBinding) {
				return resolveIProblemBinding((IProblemBinding) binding, origin);
			}
			if (binding instanceof ITypedef) {
				return resolveITypedef((ITypedef) binding, origin);
			}
			if (binding instanceof IVariable) {
				return resolveIVariable((IVariable) binding, origin);
			}
			if (binding instanceof ICPPBinding) {
				return resolveICPPBinding((ICPPBinding) binding, origin);
			}
			if (binding instanceof ICPPTwoPhaseBinding) {
				return resolveICPPTwoPhaseBinding(owner, (ICPPTwoPhaseBinding) binding, origin);
			}
		}
		catch (URISyntaxException e) {
			throw new RuntimeException("Unexpected error in URI syntax", e);
		}
		
		throw new RuntimeException("Encountered unknown Binding: " + binding.getName());
	}

	private ISourceLocation resolveICPPTwoPhaseBinding(IASTNameOwner owner, ICPPTwoPhaseBinding binding, ISourceLocation origin) throws URISyntaxException {
		if (binding instanceof CPPFunctionSet) {
			return resolveCPPFunctionSet(owner, (CPPFunctionSet) binding, origin);
		}
		
		throw new RuntimeException("Trying to resolve ICPPTwoPhaseBinding " + binding.getClass().getSimpleName());
	}

	private ISourceLocation resolveCPPFunctionSet(IASTNameOwner owner, CPPFunctionSet binding, ISourceLocation origin) throws URISyntaxException {
		// now we don't know which of the alternatives are going to be used.
		return ownedBinding(binding, "cpp+functionSet", origin); 
	}

	private ISourceLocation resolveIVariable(IVariable binding, ISourceLocation origin) throws URISyntaxException {
		if (binding instanceof ICPPVariable) {
			return resolveICPPVariable((ICPPVariable) binding, origin);
		}
		else if (binding instanceof IField) {
			return resolveIField((IField) binding, origin);
		}
		else if (binding instanceof IParameter) {
			return resolveIParameter((IParameter) binding, origin);
		}
		else if (binding instanceof CVariable) {
			return resolveCVariable((CVariable) binding, origin);
		}
		else {
			throw new RuntimeException("NYI: IVariable");
		}
	}

	private ISourceLocation resolveITypedef(ITypedef binding, ISourceLocation origin) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPAliasTemplateInstance) {
			scheme = "cpp+aliasTemplateInstance";
		}
		else {
			scheme = "cpp+typedef";
		}
		
		return ownedBinding(binding, scheme, origin);
	}

	private ISourceLocation resolveIProblemBinding(IProblemBinding binding, ISourceLocation origin) {
		err("IProblemBinding: " + binding.toString() + " @ " + origin);
	
		try {
			return URIUtil.changeQuery(failedBinding("problem"), "message=" + binding.getMessage());
		} 
		catch (URISyntaxException e) {
			throw new RuntimeException("could not create problem binding URI", e);
		}
	}

	private ISourceLocation resolveIMacroBinding(IMacroBinding binding, ISourceLocation origin) throws URISyntaxException {
		if (binding.isDynamic()) {
			String className = binding.getClass().getSimpleName();
			if ("CounterMacro".equals(className)) {
				return makeBinding("cpp+dynamicMacro", null, "counter");
			}
			if ("DateMacro".equals(className)) {
				return makeBinding("cpp+dynamicMacro", null, "date");
			}
			if ("FileMacro".equals(className)) {
				return makeBinding("cpp+dynamicMacro", null, "file");
			}
			if ("LineMacro".equals(className)) {
				return makeBinding("cpp+dynamicMacro", null, "line");
			}
			if ("TimeMacro".equals(className)) {
				return makeBinding("cpp+dynamicMacro", null, "time");
			}

			err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
			throw new RuntimeException("Encountered unknown dynamic MacroBinding " + className + " @ " + origin);
		}

		return ownedBinding(binding, "cpp+macro", origin);
	}

	private ISourceLocation resolveILabel(ILabel binding, ISourceLocation origin) throws URISyntaxException {
		return ownedBinding(binding, "cpp+label", origin);
	}

	private ISourceLocation resolveIIndexBinding(IIndexBinding binding, ISourceLocation origin) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI " + binding.getClass().getSimpleName() + " @ " + origin);
	}

	private ISourceLocation resolveIFunction(IFunction binding, ISourceLocation origin) throws URISyntaxException {
		if (binding instanceof ICPPFunction) {
			return resolveICPPFunction((ICPPFunction) binding, origin);
		}
		
		if (binding instanceof CFunction) {
			return resolveCFunction((CFunction) binding, origin);
		}
		throw new RuntimeException("NYI: unknown IFunction");
	}

	private ISourceLocation resolveIEnumerator(IEnumerator binding, ISourceLocation origin) throws URISyntaxException {
		return ownedBinding(binding, "cpp+enumerator", origin);
	}

	private ISourceLocation resolveIEnumeration(IEnumeration binding, ISourceLocation origin) throws URISyntaxException {
		if (binding instanceof ICPPEnumeration) {
			return resolveICPPEnumeration((ICPPEnumeration) binding, origin);
		}
		if (binding instanceof CEnumeration) {
			return resolveCEnumeration((CEnumeration) binding, origin);
		}
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding + " @ " + origin);
		throw new RuntimeException("NYI" + binding.getClass().getSimpleName() + ": " + binding + " @ " + origin);
	}

	public ISourceLocation resolveICPPBinding(ICPPBinding binding, ISourceLocation origin) throws URISyntaxException {
		if (binding instanceof ICPPAliasTemplateInstance) {
			return resolveICPPAliasTemplateInstance((ICPPAliasTemplateInstance) binding, origin);
		}
		if (binding instanceof ICPPClassType) {
			return resolveICPPClassType((ICPPClassType) binding, origin);
		}
		if (binding instanceof ICPPEnumeration) {
			return resolveICPPEnumeration((ICPPEnumeration) binding, origin);
		}
		if (binding instanceof ICPPFunction) {
			return resolveICPPFunction((ICPPFunction) binding, origin);
		}
		if (binding instanceof ICPPMember) {
			return resolveICPPMember((ICPPMember) binding, origin);
		}
		if (binding instanceof ICPPNamespace) {
			return resolveICPPNamespace((ICPPNamespace) binding, origin);
		}
		if (binding instanceof ICPPSpecialization) {
			return resolveICPPSpecialization((ICPPSpecialization) binding, origin);
		}
		if (binding instanceof ICPPTemplateDefinition) {
			return resolveICPPTemplateDefinition((ICPPTemplateDefinition) binding, origin);
		}
		if (binding instanceof ICPPTemplateParameter) {
			return resolveICPPTemplateParameter((ICPPTemplateParameter) binding, origin);
		}
		if (binding instanceof ICPPUsingDeclaration) {
			return resolveICPPUsingDeclaration((ICPPUsingDeclaration) binding, origin);
		}
		if (binding instanceof ICPPVariable) {
			return resolveICPPVariable((ICPPVariable) binding, origin);
		}
		if (binding instanceof ICPPInternalBinding) {
			return resolveICPPInternalBinding((ICPPInternalBinding) binding, origin);
		}
		if (binding instanceof ICPPUnknownBinding) {
			return resolveICPPUnknownBinding((ICPPUnknownBinding) binding, origin);
		}

		return unknown(); 
	}

	private ISourceLocation unknown() {
		return URIUtil.correctLocation("unknown", "", UUID.randomUUID().toString());
	}

	private ISourceLocation resolveICPPInternalBinding(ICPPInternalBinding binding, ISourceLocation origin) throws URISyntaxException {
		return ownedBinding(binding, "cpp+internal", origin);
	}

	private ISourceLocation resolveICPPUnknownBinding(ICPPUnknownBinding binding, ISourceLocation origin) {
		throw new RuntimeException("Trying to resolve ICPPUnknownBinding " + origin);
	}

	private ISourceLocation resolveIField(IField binding, ISourceLocation origin) throws URISyntaxException {
		return ownedBinding(binding, "cpp+field", origin);
	}

	private ISourceLocation resolveIParameter(IParameter binding, ISourceLocation origin) throws URISyntaxException {
		return ownedBinding(binding, "cpp+parameter", origin);
	}

	private ISourceLocation resolveCVariable(CVariable binding, ISourceLocation origin) throws URISyntaxException {
		return ownedBinding(binding, "c+variable", "", origin, binding.isStatic());
	}

	private ISourceLocation resolveICPPVariable(ICPPVariable binding, ISourceLocation origin) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPField) {
			if (binding instanceof ICPPFieldTemplate) {
				scheme = "cpp+fieldTemplate";
			}
			else {
				scheme = "cpp+field";
			}
		} else if (binding instanceof ICPPParameter) {
			scheme = "cpp+parameter";
		}
		else if (binding instanceof ICPPTemplateNonTypeParameter) {
			scheme = "cpp+templateNonTypeParameter";
		}
		else if (binding instanceof ICPPVariableInstance) {
			scheme = "cpp+variableInstance";
		}
		else if (binding instanceof ICPPVariableTemplate) {
			if (binding instanceof ICPPFieldTemplate) {
				scheme = "cpp+fieldTemplate";
			}
			else if (binding instanceof ICPPVariableTemplatePartialSpecialization) {
				scheme = "cpp+variableTemplatePartialSpec";
			}
			else {
				scheme = "cpp+variableTemplate";
			}
		} else {
			scheme = "cpp+variable";
		}

		return ownedBinding(binding, scheme, "", origin, binding.isStatic());
	}

	private ISourceLocation resolveICPPUsingDeclaration(ICPPUsingDeclaration binding, ISourceLocation origin) throws URISyntaxException {
		return ownedBinding(binding, "cpp+usingDeclaration", origin);
	}

	private ISourceLocation resolveICPPTemplateParameter(ICPPTemplateParameter binding, ISourceLocation origin) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPTemplateNonTypeParameter) {
			return resolveICPPVariable((ICPPTemplateNonTypeParameter) binding, origin);
		}
		else if (binding instanceof ICPPTemplateTemplateParameter) {
			return resolveICPPClassType((ICPPTemplateTemplateParameter) binding, origin);
		}
		else if (binding instanceof ICPPTemplateTypeParameter) {
			scheme = "cpp+templateTypeParameter";
		}
		else if (binding instanceof IPDOMCPPTemplateParameter) {
			throw new RuntimeException("resolveICPPTemplateParameter encountered IPDOMCPPTemplateParameter");
		}
		else {
			scheme = "cpp+templateParameter";
		}

		return ownedBinding(binding, scheme, origin);
	}

	private ISourceLocation resolveICPPTemplateDefinition(ICPPTemplateDefinition binding, ISourceLocation origin) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPAliasTemplate) {
			scheme = "cpp+aliasTemplate";
		}
		else if (binding instanceof ICPPFunctionTemplate) {
			scheme = "cpp+functionTemplate";
		}
		else if (binding instanceof ICPPPartiallySpecializable) {
			if (binding instanceof ICPPClassTemplate) {
				return resolveICPPClassType((ICPPClassTemplate) binding, origin);
			}
			else if (binding instanceof ICPPVariableTemplate) {
				return resolveICPPVariable((ICPPVariableTemplate) binding, origin);
			}
			else {
				throw new RuntimeException("resolveICPPTemplateDefinition encountered unknown type");
			}
		} else if (binding instanceof ICPPPartialSpecialization) {
			if (binding instanceof ICPPClassTemplatePartialSpecialization) {
				return resolveICPPClassType((ICPPClassTemplatePartialSpecialization) binding, origin);
			}
			else if (binding instanceof ICPPVariableTemplatePartialSpecialization) {
				return resolveICPPVariable((ICPPVariableTemplatePartialSpecialization) binding, origin);
			}
			else {
				throw new RuntimeException("resolveICPPTemplateDefinition encountered unknown type");
			}
		} else {
			scheme = "cpp+templateDefinition";
		}

		return ownedBinding(binding, scheme, origin);
	}

	private ISourceLocation resolveICPPSpecialization(ICPPSpecialization binding, ISourceLocation origin) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding + " @ " + origin);
		throw new RuntimeException("NYI"+ binding.getClass().getSimpleName() + ": " + binding + " @ " + origin);
	}

	private ISourceLocation resolveICPPNamespace(ICPPNamespace binding, ISourceLocation origin) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPNamespaceAlias) {
			scheme = "cpp+namespaceAlias";
		}
		else {
			scheme = "cpp+namespace";
		}

		return ownedBinding(binding, scheme, origin);
	}

	private ISourceLocation resolveICPPMember(ICPPMember binding, ISourceLocation origin) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding + " @ " + origin);
		throw new RuntimeException("NYI" + binding.getClass().getSimpleName() + ": " + binding + " @ " + origin);
	}

	private String printType(IType type) {
		// ICPPBasicType, CPPPointerType, ICPPReferenceType, CPPQualifierType
		// TODO: fix typedefs
		if (type instanceof ICPPBinding) { // ITypedef
			// nevermind that the %2F will be escaped _again_ when we build a location, we 
			// must get rid of nested slashes to avoid breaking the URI path here. The CDT will produce fully qualified references
			// to typedefs from others files here, which will include slashes in many cases.
			return ASTTypeUtil.getQualifiedName((ICPPBinding) type).replaceAll("/", "%2F");
		}
		
		if (type instanceof IProblemType
				&& ((IProblemType) type).getID() == ISemanticProblem.BINDING_KNR_PARAMETER_DECLARATION_NOT_FOUND) {
			return "$undeclaredKnRParameter";
		}
		
		return type.toString().replace(" ", ".");
	}

	public ISourceLocation getSourceLocation(IASTNode node) {
		IASTFileLocation astFileLocation = node.getFileLocation();

		if (astFileLocation != null) {
			String fileName = astFileLocation.getFileName();
			fileName = fileName.replace('\\', '/');
			try {
				return vf.sourceLocation(
						(ISourceLocation) new StandardTextReader().read(vf, new StringReader(fileName)),
						astFileLocation.getNodeOffset(), astFileLocation.getNodeLength());
			} catch (FactParseError | FactTypeUseException | IOException e) {
				// TODO: why ignore this
			}
			if (!fileName.startsWith("/")) {
				fileName = "/" + fileName;
			}
			try {
				return vf.sourceLocation(
						(ISourceLocation) new StandardTextReader().read(vf, new StringReader(fileName)),
						astFileLocation.getNodeOffset(), astFileLocation.getNodeLength());
			} catch (FactParseError | FactTypeUseException | IOException e) {
				// TODO: why ignore this
			}
			return vf.sourceLocation(vf.sourceLocation(fileName), astFileLocation.getNodeOffset(),
					astFileLocation.getNodeLength());
		}
		return vf.sourceLocation(URIUtil.rootLocation("unknown"), 0, 0);
	}

	private ISourceLocation resolveCFunction(CFunction binding, ISourceLocation origin) throws URISyntaxException {
		String scheme = "c+function";
		StringBuilder parameters = new StringBuilder("(");
		try {
			for (IParameter parameter : binding.getParameters()) {// getParameters can throw ClassCastException
				if (parameters.length() > 1) {
					parameters.append(',');
				}
				parameters.append(printType(parameter.getType()));
			}
			parameters.append(')');
		} catch (ClassCastException e) {
			stdErr.println("Encountered ClassCastException in CDT for binding " + binding.getName() + " in "
					+ getSourceLocation(binding.getDeclarations()[0]));
			parameters = new StringBuilder("($$internalError)");
		}

		return ownedBinding(binding, scheme, parameters.toString(), origin, binding.isStatic());
	}

	private ISourceLocation resolveICPPFunction(ICPPFunction binding, ISourceLocation origin) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPDeferredFunction) {
			scheme = "cpp+deferredFunction";
		}
		else if (binding instanceof ICPPFunctionInstance) {
			scheme = "cpp+functionInstance";
		}
		else if (binding instanceof ICPPFunctionSpecialization) {
			scheme = "cpp+functionSpecialization";
		}
		else if (binding instanceof ICPPFunctionTemplate) {
			scheme = "cpp+functionTemplate";
		}
		else if (binding instanceof ICPPMethod) {
			if (binding instanceof ICPPConstructor) {
				if (binding instanceof ICPPConstructorSpecialization) {
					scheme = "cpp+constructorSpecialization";
				}
				else
					scheme = "cpp+constructor";
			} else if (binding instanceof ICPPMethodSpecialization) {
				scheme = "cpp+methodSpecialization";
			}
			else if (binding.getName().startsWith("~")) {
				scheme = "cpp+destructor";
			}
			else {
				scheme = "cpp+method";
			}
		} else {
			scheme = "cpp+function"; 
		}

		StringBuilder parameters = new StringBuilder("(");
		for (ICPPParameter parameter : binding.getParameters()) {
			if (parameters.length() > 1) {
				parameters.append(',');
			}
			parameters.append(printType(parameter.getType()));
		}
		parameters.append(')');

		return ownedBinding(binding, scheme, parameters.toString(), origin, binding.isStatic());
	}

	private boolean isOperatorName(String name) {
		return name.startsWith("operator ");
	}

	private ISourceLocation resolveCEnumeration(CEnumeration binding, ISourceLocation origin) throws URISyntaxException {
		return ownedBinding(binding, "c+enum", origin);
	}

	private ISourceLocation resolveICPPEnumeration(ICPPEnumeration binding, ISourceLocation origin) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPEnumerationSpecialization) {
			scheme = "cpp+enumSpecialization";
		}
		else if (binding instanceof IPDOMCPPEnumType) {
			throw new RuntimeException("resolveICPPEnumeration encountered IPDOMCPPEnumType");
		}
		else {
			scheme = "cpp+enum";
		}

		return ownedBinding(binding, scheme, origin);
	}

	private ISourceLocation resolveICPPClassType(ICPPClassType binding, ISourceLocation origin) throws URISyntaxException {
		String scheme;
		if (binding instanceof ICPPClassSpecialization) {
			if (binding instanceof ICPPClassTemplatePartialSpecializationSpecialization) {
				scheme = "cpp+classTemplatePartialSpecSpec";
			}
			else {
				scheme = "cpp+classSpecialization";
			}
		} else if (binding instanceof ICPPClassTemplate) {
			if (binding instanceof ICPPClassTemplatePartialSpecialization) {
				scheme = "cpp+classTemplatePartialSpec";
			}
			else if (binding instanceof ICPPTemplateTemplateParameter) {
				scheme = "cpp+templateTemplateParameter";
			}
			else {
				scheme = "cpp+classTemplate";
			}
		} else if (binding instanceof ICPPDeferredClassInstance) {
			scheme = "cpp+deferredClassInstance";
		}

		else if (binding instanceof ICPPUnknownMemberClass) {
			if (binding instanceof ICPPUnknownMemberClassInstance) {
				scheme = "cpp+unknownMemberClassInstance";
			}
			else {
				scheme = "cpp+unknownMemberClass"; 
			}
		} else if (binding instanceof IPDOMCPPClassType) {
			throw new RuntimeException("resolveICPPClassType encountered IPDOMCPPClassType");
		}
		else {
			scheme = "cpp+class";
		}

		return ownedBinding(binding, scheme, origin);
	}

	private ISourceLocation resolveICPPAliasTemplateInstance(ICPPAliasTemplateInstance binding, ISourceLocation origin) {
		err("Trying to resolve " + binding.getClass().getSimpleName() + ": " + binding);
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveICompositeType(ICompositeType binding, ISourceLocation origin) throws URISyntaxException {
		if (binding instanceof ICPPClassType) {
			return resolveICPPClassType((ICPPClassType) binding, origin);
		}

		return ownedBinding(binding, "c+struct", origin);
	}

	private ISourceLocation resolveICExternalBinding(ICExternalBinding binding, ISourceLocation origin) throws URISyntaxException {
		return URIUtil.changePath(URIUtil.rootLocation("c+externalBinding"), binding.getName());
	}

	public ISourceLocation resolveBinding(IASTNameOwner node, ISourceLocation origin) {
		try {
			if (node instanceof IASTCompositeTypeSpecifier) {
				return resolveCompositeTypeSpecifier((IASTCompositeTypeSpecifier) node);
			}
			if (node instanceof IASTDeclarator) {
				return resolveDeclarator((IASTDeclarator) node);
			}
			if (node instanceof IASTElaboratedTypeSpecifier) {
				return resolveElaboratedTypeSpecifier((IASTElaboratedTypeSpecifier) node);
			}
			if (node instanceof IASTEnumerationSpecifier) {
				return resolveEnumerationSpecifier((IASTEnumerationSpecifier) node);
			}
			if (node instanceof IASTEnumerator) {
				return resolveEnumerator((IASTEnumerator) node);
			}
			if (node instanceof IASTFieldReference) {
				return resolveFieldReference((IASTFieldReference) node);
			}
			if (node instanceof IASTGotoStatement) {
				return resolveGotoStatement((IASTGotoStatement) node);
			}
			if (node instanceof IASTIdExpression) {
				return resolveIdExpression((IASTIdExpression) node);
			}
			// IASTInternalNameOwner skipped, checked as CPPASTElaboratedTypeSpecifier
			if (node instanceof IASTLabelStatement) {
				return resolveLabelStatement((IASTLabelStatement) node);
			}
			if (node instanceof IASTNamedTypeSpecifier) {
				return resolveNamedTypeSpecifier((IASTNamedTypeSpecifier) node);
			}
			if (node instanceof IASTPreprocessorMacroDefinition) { 
				return resolvePreprocessorMacroDefinition((IASTPreprocessorMacroDefinition) node);
			}
			if (node instanceof ICPPASTAliasDeclaration) {
				return resolveAliasDeclaration((ICPPASTAliasDeclaration) node);
			}
			if (node instanceof ICPPASTBaseSpecifier) {
				return resolveBaseSpecifier((ICPPASTBaseSpecifier) node);
			}
			if (node instanceof ICPPASTCapture) {
				return resolveCapture((ICPPASTCapture) node);
			}
			if (node instanceof ICPPASTConstructorChainInitializer) {
				return resolveConstructorChainInitializer((ICPPASTConstructorChainInitializer) node);
			}
			if (node instanceof ICPPASTNamespaceAlias) {
				return resolveNamespaceAlias((ICPPASTNamespaceAlias) node);
			}
			if (node instanceof ICPPASTNamespaceDefinition) {
				return resolveNamespaceDefinition((ICPPASTNamespaceDefinition) node);
			}
			if (node instanceof ICPPASTPointerToMember) {
				return resolvePointerToMember((ICPPASTPointerToMember) node);
			}
			if (node instanceof ICPPASTQualifiedName) {
				return resolveQualifiedName((ICPPASTQualifiedName) node);
			}
			if (node instanceof ICPPASTSimpleTypeTemplateParameter) {
				return resolveSimpleTypeTemplateParameter((ICPPASTSimpleTypeTemplateParameter) node);
			}
			if (node instanceof ICPPASTTemplatedTypeTemplateParameter) {
				return resolveTemplatedTypeTemplateParameter((ICPPASTTemplatedTypeTemplateParameter) node);
			}
			if (node instanceof ICPPASTTemplateId) {
				return resolveTemplateId((ICPPASTTemplateId) node);
			}
			// ICPPASTTypenameExpression is deprecated
			if (node instanceof ICPPASTUsingDeclaration) {
				return resolveUsingDeclaration((ICPPASTUsingDeclaration) node);
			}
			if (node instanceof ICPPASTUsingDirective) {
				return resolveUsingDirective((ICPPASTUsingDirective) node);
			}
			// if (node instanceof IGNUASTGotoStatement)
			// return resolveGnuGotoStatement((IGNUASTGotoStatement) node);
		} catch (URISyntaxException e) {
			err(e.getMessage());
		}

		return failedBinding("unknown");
	}

	public ISourceLocation resolveBinding(ICPPBinding binding, ISourceLocation origin) {
		try {
			return resolveICPPBinding(binding, origin);
		} catch (URISyntaxException e) {
			err(e.getMessage());
		}

		return failedBinding("unknown");
	}

	private ISourceLocation resolveUsingDeclaration(ICPPASTUsingDeclaration node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveUsingDirective(ICPPASTUsingDirective node) throws URISyntaxException {
		return resolveBinding(node, node.getQualifiedName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveTemplateId(ICPPASTTemplateId node) throws URISyntaxException {
		return resolveBinding(node, node.resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveTemplatedTypeTemplateParameter(ICPPASTTemplatedTypeTemplateParameter node)
			throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveSimpleTypeTemplateParameter(ICPPASTSimpleTypeTemplateParameter node)
			throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveQualifiedName(ICPPASTQualifiedName node) throws URISyntaxException {
		return resolveBinding(node, node.resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolvePointerToMember(ICPPASTPointerToMember node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveNamespaceDefinition(ICPPASTNamespaceDefinition node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveNamespaceAlias(ICPPASTNamespaceAlias node) throws URISyntaxException {
		return resolveBinding(node, node.getAlias().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveConstructorChainInitializer(ICPPASTConstructorChainInitializer node)
			throws URISyntaxException {
		return resolveBinding(node, node.getMemberInitializerId().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveCapture(ICPPASTCapture node) throws URISyntaxException {
		IASTName name = node.getIdentifier();
		if (name == null) {
			out("Resolving this capture; returning dummy value");
			return FIXME;
		}
		return resolveBinding(node, name.resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveBaseSpecifier(ICPPASTBaseSpecifier node) throws URISyntaxException {
		return resolveBinding(node, node.getNameSpecifier().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveAliasDeclaration(ICPPASTAliasDeclaration node) throws URISyntaxException {
		return resolveBinding(node, node.getAlias().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolvePreprocessorMacroDefinition(IASTPreprocessorMacroDefinition node) {
		throw new RuntimeException("NYI");
	}

	private ISourceLocation resolveNamedTypeSpecifier(IASTNamedTypeSpecifier node) throws URISyntaxException {
		IBinding binding = node.getName().resolveBinding();
		return resolveBinding(node,binding, getSourceLocation(node));
	}

	private ISourceLocation resolveLabelStatement(IASTLabelStatement node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveIdExpression(IASTIdExpression node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveGotoStatement(IASTGotoStatement node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveFieldReference(IASTFieldReference node) throws URISyntaxException {
		return resolveBinding(node, node.getFieldName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveEnumerator(IASTEnumerator node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveEnumerationSpecifier(IASTEnumerationSpecifier node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveElaboratedTypeSpecifier(IASTElaboratedTypeSpecifier node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveDeclarator(IASTDeclarator node) throws URISyntaxException {
		if (node.getName() == null) {
			out("resolveDeclarator has null name. " + node.getClass().getSimpleName() + ": " + node.getRawSignature());
			return FIXME;
		}
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	private ISourceLocation resolveCompositeTypeSpecifier(IASTCompositeTypeSpecifier node) throws URISyntaxException {
		return resolveBinding(node, node.getName().resolveBinding(), getSourceLocation(node));
	}

	public ISourceLocation makeBinding(String scheme, String authority, String path) {
		try {
			return vf.sourceLocation(scheme, authority, path);
		} catch (URISyntaxException e) {
			assert false;
			throw new RuntimeException("Should not happen", e);
		}
	}
}
