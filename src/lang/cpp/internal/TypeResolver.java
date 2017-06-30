package lang.cpp.internal;

import java.net.URISyntaxException;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;
import org.eclipse.cdt.core.dom.ast.IASTExpression;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.eclipse.cdt.core.dom.ast.IArrayType;
import org.eclipse.cdt.core.dom.ast.IBasicType;
import org.eclipse.cdt.core.dom.ast.IBinding;
import org.eclipse.cdt.core.dom.ast.ICompositeType;
import org.eclipse.cdt.core.dom.ast.IEnumeration;
import org.eclipse.cdt.core.dom.ast.IFunctionType;
import org.eclipse.cdt.core.dom.ast.IPointerType;
import org.eclipse.cdt.core.dom.ast.IProblemBinding;
import org.eclipse.cdt.core.dom.ast.IProblemType;
import org.eclipse.cdt.core.dom.ast.IQualifierType;
import org.eclipse.cdt.core.dom.ast.IType;
import org.eclipse.cdt.core.dom.ast.ITypedef;
import org.eclipse.cdt.core.dom.ast.c.ICBasicType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPAliasTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPBase;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPBasicType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassTemplatePartialSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPEnumerationSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPParameterPackType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPReferenceType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateArgument;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateParameterMap;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateTypeParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTypeSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPUnaryTypeTransformation;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPUnaryTypeTransformation.Operator;
import org.eclipse.cdt.internal.core.dom.parser.ITypeContainer;
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPTemplateTypeParameter;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPDeferredClassInstance;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownMemberClass;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownType;
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.TypeOfDependentExpression;
import org.eclipse.cdt.internal.core.index.IIndexType;
import org.eclipse.cdt.internal.core.pdom.dom.cpp.IPDOMCPPClassType;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.uri.URIUtil;

import io.usethesource.vallang.IConstructor;
import io.usethesource.vallang.IListWriter;
import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IValueFactory;

@SuppressWarnings("restriction")
public class TypeResolver {
	private AST builder;
	private final IValueFactory vf;
	private IEvaluatorContext ctx;
	private BindingsResolver br = new BindingsResolver();

	public TypeResolver(AST builder, IValueFactory vf) {
		this.builder = builder;
		this.vf = vf;
	}

	public void setIEvaluatorContext(IEvaluatorContext ctx) {
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

	private ISourceLocation getDecl(IBinding binding) {
		try {
			return br.resolveBinding(binding);
		} catch (URISyntaxException e) {
			err("Warning: could not resolve " + binding);
			return br.makeBinding("typUnknown", null, null);
		}
	}

	private ISourceLocation getOwner(IBinding binding) {
		try {
			return br.resolveOwner(binding);
		} catch (URISyntaxException e) {
			err("Warning: could not resolve " + binding);
			return br.makeBinding("ownerUnknown", null, null);
		}
	}

	public IConstructor resolveType(IASTNode node) {
		if (node instanceof IASTExpression)
			return resolveIASTExpression((IASTExpression) node);
		return builder.TypeSymbol_any();
	}

	public IConstructor resolveIASTExpression(IASTExpression node) {
		return resolveType(node.getExpressionType());
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
		IConstructor baseType = resolveType(type.getType());
		org.eclipse.cdt.core.dom.ast.IValue size = type.getSize();
		if (size == null || size.numberValue() == null)
			return builder.TypeSymbol_array(baseType);
		return builder.TypeSymbol_array(baseType, vf.integer(size.numberValue().intValue()));
	}

	private IConstructor resolveIBasicType(IBasicType type) {
		if (type instanceof ICBasicType)
			return resolveICBasicType((ICBasicType) type);
		if (type instanceof ICPPBasicType)
			return resolveICPPBasicType((ICPPBasicType) type);
		throw new RuntimeException("Unknown IBasicType subtype " + type.getClass().getSimpleName());
	}

	private IConstructor resolveICBasicType(ICBasicType type) {
		throw new RuntimeException("NYI: resolveICBasicType");
	}

	private IConstructor resolveICPPBasicType(ICPPBasicType type) {
		IListWriter modifiers = vf.listWriter();
		if (type.isSigned())
			modifiers.append(builder.TypeModifier_signed());
		if (type.isUnsigned())
			modifiers.append(builder.TypeModifier_unsigned());
		if (type.isShort())
			modifiers.append(builder.TypeModifier_short());
		if (type.isLong())
			modifiers.append(builder.TypeModifier_long());
		if (type.isLongLong())
			modifiers.append(builder.TypeModifier_longlong());
		if (type.isComplex())
			modifiers.append(builder.TypeModifier_complex());
		if (type.isImaginary())
			modifiers.append(builder.TypeModifier_imaginary());

		builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_unspecified());
		switch (type.getKind()) {
		case eUnspecified:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_unspecified());
		case eVoid:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_void());
		case eChar:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_char());
		case eWChar:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_wchar());
		case eInt:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_int());
		case eFloat:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_float());
		case eDouble:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_double());
		case eBoolean:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_boolean());
		case eChar16:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_boolean());
		case eChar32:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_char32());
		case eNullPtr:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_nullPtr());
		case eInt128:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_int128());
		case eFloat128:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_float128());
		case eDecimal32:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_decimal32());
		case eDecimal64:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_decimal64());
		case eDecimal128:
			return builder.TypeSymbol_basicType(modifiers.done(), builder.TypeSymbol_decimal128());
		default:
			throw new RuntimeException("Unknown ICPPBasicType kind " + type.getKind().name());
		}
	}

	private IConstructor resolveICompositeType(ICompositeType type) {
		if (type instanceof ICPPClassSpecialization)
			return resolveICPPClassSpecialization((ICPPClassSpecialization) type);
		if (type instanceof ICPPClassTemplate)
			return resolveICPPClassTemplate((ICPPClassTemplate) type);
		if (type instanceof ICPPDeferredClassInstance)
			return resolveICPPDeferredClassInstance((ICPPDeferredClassInstance) type);
		if (type instanceof ICPPASTCompositeTypeSpecifier)
			;
		if (type instanceof ICPPUnknownMemberClass)
			return resolveICPPUnknownMemberClass((ICPPUnknownMemberClass) type);
		if (type instanceof IPDOMCPPClassType)
			;
		if (type instanceof ICPPClassType)
			return resolveICPPClassType((ICPPClassType) type);
		throw new RuntimeException("NYI: resolveICompositeType");
	}

	private IConstructor resolveICPPClassType(ICPPClassType type) {
		switch (type.getKey()) {
		case ICPPClassType.k_struct:
			IListWriter fields = vf.listWriter();
			// Stream.of(type.getFields()).forEach(it -> out("\tField: " + it));
			// Stream.of(type.getFields()).forEach(it ->
			// fields.append(resolveType(it.getType(), src)));
			return builder.TypeSymbol_struct(fields.done());
		case ICPPClassType.k_union:
			return builder.TypeSymbol_union(getDecl(type));
		case ICPPClassType.k_class:
			return builder.TypeSymbol_class(getDecl(type));
		default:
			throw new RuntimeException("Unknown ICompositeType key " + type.getKey());
		}
	}

	private IConstructor resolveICPPClassSpecialization(ICPPClassSpecialization type) {
		ISourceLocation decl = getDecl(type.getSpecializedBinding());
		IListWriter templateParameters = vf.listWriter();
		ICPPTemplateParameterMap parameterMap = type.getTemplateParameterMap();
		Stream.of(parameterMap.getAllParameterPositions())
				.forEach(it -> templateParameters.append(resolveType(parameterMap.getArgument(it).getTypeValue())));
		return builder.TypeSymbol_classSpecialization(decl, templateParameters.done());
	}

	private IConstructor resolveICPPClassTemplate(ICPPClassTemplate type) {
		ICPPBase[] _bases = type.getBases();
		IListWriter baseClassTypes = vf.listWriter();
		Stream.of(_bases).forEach(it -> baseClassTypes.append(resolveType(it.getBaseClassType())));
		switch (type.getKey()) {
		case ICPPClassTemplate.k_struct:
			out("ICPPClassTemplate struct");
			break;
		case ICPPClassTemplate.k_union:
			out("ICPPClassTemplate union");
			break;
		case ICPPClassTemplate.k_class:
			ICPPClassTemplatePartialSpecialization[] specs = type.getPartialSpecializations();
			if (specs.length > 0)
				throw new RuntimeException("ICPPClassTemplate has partial specializations!");
			// FIXME
			return resolveICPPClassType((ICPPClassType) type);
		default:
			throw new RuntimeException("Unknown ICompositeType key " + type.getKey());
		}
		throw new RuntimeException("NYI: resolveICPPClassTemplate");
	}

	private IConstructor resolveICPPDeferredClassInstance(ICPPDeferredClassInstance type) {
		// FIXME
		return builder.TypeSymbol_deferredClassInstance(type.toString());
	}

	private IConstructor resolveICPPUnknownMemberClass(ICPPUnknownMemberClass type) {
		// FIXME
		return builder.TypeSymbol_unknownMemberClass(resolveType((IType) type.getOwner()), type.getName());
	}

	private IConstructor resolveICPPAliasTemplate(ICPPAliasTemplate type) {
		IType _type = type.getType();
		throw new RuntimeException("NYI: resolveICPPAliasTemplate");
	}

	private IConstructor resolveICPPParameterPackType(ICPPParameterPackType type) {
		return builder.TypeSymbol_parameterPackType(resolveType(type.getType()));
	}

	private IConstructor resolveICPPReferenceType(ICPPReferenceType type) {
		return builder.TypeSymbol_referenceType(resolveType(type.getType()));
	}

	private IConstructor resolveICPPTemplateTypeParameter(ICPPTemplateTypeParameter type) {
		if (type instanceof CPPTemplateTypeParameter)// FIXME
			return builder.TypeSymbol_templateTypeParameter(getOwner(type).toString(), type.getName());
		throw new RuntimeException(
				"NYI: resolveICPPTemplateTypeParameter " + type.getClass().getSimpleName() + ": " + type);
	}

	private IConstructor resolveICPPTypeSpecialization(ICPPTypeSpecialization type) {
		if (type instanceof ICPPClassSpecialization)
			return resolveICPPClassSpecialization((ICPPClassSpecialization) type);
		if (type instanceof ICPPEnumerationSpecialization)
			return resolveICPPEnumerationSpecialization((ICPPEnumerationSpecialization) type);
		throw new RuntimeException("resolveICPPTypeSpecialization encountered unknown subtype");
	}

	private IConstructor resolveICPPEnumerationSpecialization(ICPPEnumerationSpecialization type) {
		ISourceLocation specializedBinding = getDecl(type.getSpecializedBinding());
		ICPPTemplateParameterMap templateParameterMap = type.getTemplateParameterMap();
		IListWriter templateArguments = vf.listWriter();
		Stream.of(templateParameterMap.getAllParameterPositions()).forEach(it -> {
			ICPPTemplateArgument arg = templateParameterMap.getArgument(it);
			if (arg.isNonTypeValue())
				throw new RuntimeException("Bla");
			IType typeValue = arg.getTypeValue();
			err("TemplateArgument " + typeValue.getClass().getSimpleName());
			err("typeValue " + type);

			// templateArguments
			// .append(builder.TypeSymbol_templateArgument(it,
			// getDecl(templateParameterMap.getArgument(it))));
		});
		return builder.TypeSymbol_enumerationSpecialization(specializedBinding, templateArguments.done());
	}

	private IConstructor resolveICPPUnaryTypeTransformation(ICPPUnaryTypeTransformation type) {
		Operator operator = type.getOperator();
		IType operand = type.getOperand();
		throw new RuntimeException("NYI: resolveICPPUnaryTypeTransformation");
	}

	private IConstructor resolveICPPUnknownType(ICPPUnknownType type) {
		if (type instanceof TypeOfDependentExpression)
			return builder.TypeSymbol_typeOfDependentExpression(URIUtil.rootLocation("foo"));
		throw new RuntimeException("NYI: resolveICPPUnknownType (" + type.getClass().getSimpleName() + ")");
	}

	private IConstructor resolveIEnumeration(IEnumeration type) {
		try {
			ISourceLocation decl = br.resolveBinding(type);
			return builder.TypeSymbol_enumeration(decl);
		} catch (URISyntaxException e) {
			throw new RuntimeException("Could not resolve IEnumeration" + type);
		}
	}

	private IConstructor resolveIFunctionType(IFunctionType type) {
		IConstructor returnType = resolveType(type.getReturnType());
		IListWriter parameterTypes = vf.listWriter();
		Stream.of(type.getParameterTypes()).forEach(it -> parameterTypes.append(resolveType(it)));
		if (type.takesVarArgs())
			return builder.TypeSymbol_functionTypeVarArgs(returnType, parameterTypes.done());
		return builder.TypeSymbol_functionType(returnType, parameterTypes.done());

	}

	private IConstructor resolveIIndexType(IIndexType type) {
		throw new RuntimeException("NYI: resolveIIndexType");
	}

	private IConstructor resolveIPointerType(IPointerType type) {
		IListWriter modifiers = vf.listWriter();
		if (type.isConst())
			modifiers.append(builder.TypeModifier_const());
		if (type.isVolatile())
			modifiers.append(builder.TypeModifier_volatile());
		if (type.isRestrict())
			modifiers.append(builder.TypeModifier_restrict());
		return builder.TypeSymbol_pointerType(modifiers.done(), resolveType(type.getType()));
	}

	private IConstructor resolveIProblemBinding(IProblemBinding type) {
		err("Encountered IProblemBinding " + type.getClass().getSimpleName() + ": ");
		err("\t" + type.getID() + ": " + type.getMessage());
		return builder.TypeSymbol_problemBinding();
	}

	private IConstructor resolveIProblemType(IProblemType type) {
		err("Encountered IProblemType " + type.getClass().getSimpleName() + ": ");
		err("\t" + type.getID() + ": " + type.getMessage());
		return builder.TypeSymbol_problemType();
	}

	private IConstructor resolveIQualifierType(IQualifierType type) {
		IConstructor baseType = resolveType(type.getType());
		IListWriter modifiers = vf.listWriter();
		if (type.isConst())
			modifiers.append(builder.TypeModifier_const());
		if (type.isVolatile())
			modifiers.append(builder.TypeModifier_volatile());
		return builder.TypeSymbol_qualifierType(modifiers.done(), baseType);
	}

	private IConstructor resolveITypeContainer(ITypeContainer type) {
		return builder.TypeSymbol_typeContainer(resolveType(type.getType()));
	}

	private IConstructor resolveITypedef(ITypedef type) {
		IType _type = type.getType();
		throw new RuntimeException("NYI: resolveITypedef");
	}
}
