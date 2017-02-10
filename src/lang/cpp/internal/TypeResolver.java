package lang.cpp.internal;

import org.apache.commons.lang.StringUtils;
import org.eclipse.cdt.core.dom.ast.IArrayType;
import org.eclipse.cdt.core.dom.ast.IBasicType;
import org.eclipse.cdt.core.dom.ast.ICompositeType;
import org.eclipse.cdt.core.dom.ast.IEnumeration;
import org.eclipse.cdt.core.dom.ast.IFunctionType;
import org.eclipse.cdt.core.dom.ast.IPointerType;
import org.eclipse.cdt.core.dom.ast.IProblemBinding;
import org.eclipse.cdt.core.dom.ast.IProblemType;
import org.eclipse.cdt.core.dom.ast.IQualifierType;
import org.eclipse.cdt.core.dom.ast.IType;
import org.eclipse.cdt.core.dom.ast.ITypedef;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPAliasTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPParameterPackType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPReferenceType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateTypeParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTypeSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPUnaryTypeTransformation;
import org.eclipse.cdt.internal.core.dom.parser.ITypeContainer;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownType;
import org.eclipse.cdt.internal.core.index.IIndexType;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.value.IConstructor;
import org.rascalmpl.value.IValueFactory;

public class TypeResolver {
	private AST builder;
	private final IValueFactory vf;
	private IEvaluatorContext ctx;

	public TypeResolver(AST builder, IValueFactory vf, IEvaluatorContext ctx) {
		this.builder = builder;
		this.vf = vf;
		this.ctx = ctx;
	}

	private int prefix = 0;

	private String spaces() {
		return StringUtils.repeat(" ", prefix);
	}

	private void out(String msg) {
		ctx.getStdOut().println(spaces() + msg.replace("\n", "\n" + spaces()));
	}

	private void err(String msg) {
		ctx.getStdErr().println(spaces() + msg.replace("\n", "\n" + spaces()));
	}

	public IConstructor resolveType(IType type) {
		if (type instanceof IArrayType)
			return resolveIArrayType((IArrayType) type);
		if (type instanceof IBasicType)
			return resolveIBasicType((IBasicType) type);
		if (type instanceof ICompositeType)
			return resolveICompositeType((ICompositeType) type);
		if (type instanceof ICPPAliasTemplate)
			return resolveICPPAliasTemplate((ICPPAliasTemplate) type);
		if (type instanceof ICPPParameterPackType)
			return resolveICPPParameterPackType((ICPPParameterPackType) type);
		if (type instanceof ICPPReferenceType)
			return resolveICPPReferenceType((ICPPReferenceType) type);
		if (type instanceof ICPPTemplateTypeParameter)
			return resolveICPPTemplateTypeParameter((ICPPTemplateTypeParameter) type);
		if (type instanceof ICPPTypeSpecialization)
			return resolveICPPTypeSpecialization((ICPPTypeSpecialization) type);
		if (type instanceof ICPPUnaryTypeTransformation)
			return resolveICPPUnaryTypeTransformation((ICPPUnaryTypeTransformation) type);
		if (type instanceof ICPPUnknownType)
			return resolveICPPUnknownType((ICPPUnknownType) type);
		if (type instanceof IEnumeration)
			return resolveIEnumeration((IEnumeration) type);
		if (type instanceof IFunctionType)
			return resolveIFunctionType((IFunctionType) type);
		if (type instanceof IIndexType)
			return resolveIIndexType((IIndexType) type);
		if (type instanceof IPointerType)
			return resolveIPointerType((IPointerType) type);
		if (type instanceof IProblemBinding)
			return resolveIProblemBinding((IProblemBinding) type);
		if (type instanceof IProblemType)
			return resolveIProblemType((IProblemType) type);
		if (type instanceof IQualifierType)
			return resolveIQualifierType((IQualifierType) type);
		if (type instanceof ITypeContainer)
			return resolveITypeContainer((ITypeContainer) type);
		if (type instanceof ITypedef)
			return resolveITypedef((ITypedef) type);
		return null;
	}

	private IConstructor resolveIArrayType(IArrayType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveIBasicType(IBasicType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveICompositeType(ICompositeType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveICPPAliasTemplate(ICPPAliasTemplate type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveICPPParameterPackType(ICPPParameterPackType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveICPPReferenceType(ICPPReferenceType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveICPPTemplateTypeParameter(ICPPTemplateTypeParameter type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveICPPTypeSpecialization(ICPPTypeSpecialization type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveICPPUnaryTypeTransformation(ICPPUnaryTypeTransformation type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveICPPUnknownType(ICPPUnknownType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveIEnumeration(IEnumeration type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveIFunctionType(IFunctionType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveIIndexType(IIndexType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveIPointerType(IPointerType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveIProblemBinding(IProblemBinding type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveIProblemType(IProblemType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveIQualifierType(IQualifierType type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveITypeContainer(ITypeContainer type) {
		// TODO Auto-generated method stub
		return null;
	}

	private IConstructor resolveITypedef(ITypedef type) {
		// TODO Auto-generated method stub
		return null;
	}

}
