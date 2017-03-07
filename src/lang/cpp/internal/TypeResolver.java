package lang.cpp.internal;

import java.net.URISyntaxException;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;
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
import org.eclipse.cdt.core.dom.ast.cpp.ICPPParameterPackType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPReferenceType;
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
import org.rascalmpl.value.IConstructor;
import org.rascalmpl.value.IListWriter;
import org.rascalmpl.value.IMapWriter;
import org.rascalmpl.value.ISourceLocation;
import org.rascalmpl.value.IValueFactory;

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

	public IConstructor resolveType(IType type, ISourceLocation src) {
		if (type instanceof IArrayType)
			return resolveIArrayType((IArrayType) type, src);
		if (type instanceof IBasicType)
			return resolveIBasicType((IBasicType) type, src);
		if (type instanceof ICompositeType)
			return resolveICompositeType((ICompositeType) type, src);
		if (type instanceof ICPPAliasTemplate)
			return resolveICPPAliasTemplate((ICPPAliasTemplate) type, src);
		if (type instanceof ICPPParameterPackType)
			return resolveICPPParameterPackType((ICPPParameterPackType) type, src);
		if (type instanceof ICPPReferenceType)
			return resolveICPPReferenceType((ICPPReferenceType) type, src);
		if (type instanceof ICPPTemplateTypeParameter)
			return resolveICPPTemplateTypeParameter((ICPPTemplateTypeParameter) type, src);
		if (type instanceof ICPPTypeSpecialization)
			return resolveICPPTypeSpecialization((ICPPTypeSpecialization) type, src);
		if (type instanceof ICPPUnaryTypeTransformation)
			return resolveICPPUnaryTypeTransformation((ICPPUnaryTypeTransformation) type, src);
		if (type instanceof ICPPUnknownType)
			return resolveICPPUnknownType((ICPPUnknownType) type, src);
		if (type instanceof IEnumeration)
			return resolveIEnumeration((IEnumeration) type, src);
		if (type instanceof IFunctionType)
			return resolveIFunctionType((IFunctionType) type, src);
		if (type instanceof IIndexType)
			return resolveIIndexType((IIndexType) type, src);
		if (type instanceof IPointerType)
			return resolveIPointerType((IPointerType) type, src);
		if (type instanceof IProblemBinding)
			return resolveIProblemBinding((IProblemBinding) type, src);
		if (type instanceof IProblemType)
			return resolveIProblemType((IProblemType) type, src);
		if (type instanceof IQualifierType)
			return resolveIQualifierType((IQualifierType) type, src);
		if (type instanceof ITypeContainer)
			return resolveITypeContainer((ITypeContainer) type, src);
		if (type instanceof ITypedef)
			return resolveITypedef((ITypedef) type, src);
		return null;
	}

	private IConstructor resolveIArrayType(IArrayType type, ISourceLocation src) {
		IConstructor baseType = resolveType(type.getType(), src);
		org.eclipse.cdt.core.dom.ast.IValue size = type.getSize();
		if (size == null || size.numberValue() == null)
			return builder.TypeSymbol_array(baseType);
		return builder.TypeSymbol_array(baseType, vf.integer(size.numberValue().intValue()));
	}

	private IConstructor resolveIBasicType(IBasicType type, ISourceLocation src) {
		if (type instanceof ICBasicType)
			return resolveICBasicType((ICBasicType) type, src);
		if (type instanceof ICPPBasicType)
			return resolveICPPBasicType((ICPPBasicType) type, src);
		throw new RuntimeException("Unknown IBasicType subtype " + type.getClass().getSimpleName());
	}

	private IConstructor resolveICBasicType(ICBasicType type, ISourceLocation src) {
		throw new RuntimeException("NYI: resolveICBasicType");
	}

	private IConstructor resolveICPPBasicType(ICPPBasicType type, ISourceLocation src) {
		IListWriter modifiers = vf.listWriter();
		if (type.isSigned())
			modifiers.append(builder.Modifier_signed(src));
		if (type.isUnsigned())
			modifiers.append(builder.Modifier_unsigned(src));
		if (type.isShort())
			modifiers.append(builder.Modifier_short(src));
		if (type.isLong())
			modifiers.append(builder.Modifier_long(src));
		if (type.isLongLong())
			modifiers.append(builder.Modifier_longlong(src));
		if (type.isComplex())
			modifiers.append(builder.Modifier_complex(src));
		if (type.isImaginary())
			modifiers.append(builder.Modifier_imaginary(src));

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

	private IConstructor resolveICompositeType(ICompositeType type, ISourceLocation src) {
		if (type instanceof ICPPClassSpecialization)
			return resolveICPPClassSpecialization((ICPPClassSpecialization) type, src);
		if (type instanceof ICPPClassTemplate)
			return resolveICPPClassTemplate((ICPPClassTemplate) type, src);
		if (type instanceof ICPPDeferredClassInstance)
			return resolveICPPDeferredClassInstance((ICPPDeferredClassInstance) type, src);
		if (type instanceof ICPPASTCompositeTypeSpecifier)
			;
		if (type instanceof ICPPUnknownMemberClass)
			return resolveICPPUnknownMemberClass((ICPPUnknownMemberClass) type, src);
		if (type instanceof IPDOMCPPClassType)
			;
		if (type instanceof ICPPClassType)
			return resolveICPPClassType((ICPPClassType) type, src);
		throw new RuntimeException("NYI: resolveICompositeType");
	}

	private IConstructor resolveICPPClassType(ICPPClassType type, ISourceLocation src) {
		switch (type.getKey()) {
		case ICPPClassType.k_struct:
			IListWriter fields = vf.listWriter();
			Stream.of(type.getFields()).forEach(it -> out("\tField: " + it));
			// Stream.of(type.getFields()).forEach(it ->
			// fields.append(resolveType(it.getType(), src)));
			return builder.TypeSymbol_struct(fields.done());
		case ICPPClassType.k_union:
			out("ICPPClassTemplate union");
			break;
		case ICPPClassType.k_class:
			return builder.TypeSymbol_class(getDecl(type));
		default:
			throw new RuntimeException("Unknown ICompositeType key " + type.getKey());
		}
		throw new RuntimeException("NYI: resolveICPPClassType " + type.getClass().getSimpleName());
	}

	private IConstructor resolveICPPClassSpecialization(ICPPClassSpecialization type, ISourceLocation src) {
		ISourceLocation decl = getDecl(type.getSpecializedBinding());
		IMapWriter templateParameters = vf.mapWriter();
		ICPPTemplateParameterMap parameterMap = type.getTemplateParameterMap();
		Stream.of(parameterMap.getAllParameterPositions()).forEach(it -> templateParameters.put(vf.integer(it),
				resolveType(parameterMap.getArgument(it).getTypeValue(), src)));
		return builder.TypeSymbol_classSpecialization(decl, templateParameters.done());
	}

	private IConstructor resolveICPPClassTemplate(ICPPClassTemplate type, ISourceLocation src) {
		ICPPBase[] _bases = type.getBases();
		IListWriter baseClassTypes = vf.listWriter();
		Stream.of(_bases).forEach(it -> baseClassTypes.append(resolveType(it.getBaseClassType(), src)));
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
			return resolveICPPClassType((ICPPClassType) type, src);
		default:
			throw new RuntimeException("Unknown ICompositeType key " + type.getKey());
		}
		throw new RuntimeException("NYI: resolveICPPClassTemplate");
	}

	private IConstructor resolveICPPDeferredClassInstance(ICPPDeferredClassInstance type, ISourceLocation src) {
		// FIXME
		return builder.TypeSymbol_deferredClassInstance(type.toString());
	}

	private IConstructor resolveICPPUnknownMemberClass(ICPPUnknownMemberClass type, ISourceLocation src) {
		// FIXME
		return builder.TypeSymbol_unknownMemberClass(resolveType((IType) type.getOwner(), src), type.getName());
	}

	private IConstructor resolveICPPAliasTemplate(ICPPAliasTemplate type, ISourceLocation src) {
		IType _type = type.getType();
		throw new RuntimeException("NYI: resolveICPPAliasTemplate");
	}

	private IConstructor resolveICPPParameterPackType(ICPPParameterPackType type, ISourceLocation src) {
		return builder.TypeSymbol_parameterPackType(resolveType(type.getType(), src));
	}

	private IConstructor resolveICPPReferenceType(ICPPReferenceType type, ISourceLocation src) {
		return builder.TypeSymbol_referenceType(resolveType(type.getType(), src));
	}

	private IConstructor resolveICPPTemplateTypeParameter(ICPPTemplateTypeParameter type, ISourceLocation src) {
		if (type instanceof CPPTemplateTypeParameter)// FIXME
			return builder.TypeSymbol_templateTypeParameter(type.getOwner().getName(), type.getName());
		throw new RuntimeException(
				"NYI: resolveICPPTemplateTypeParameter " + type.getClass().getSimpleName() + ": " + type);
	}

	private IConstructor resolveICPPTypeSpecialization(ICPPTypeSpecialization type, ISourceLocation src) {
		throw new RuntimeException("NYI: resolveICPPTypeSpecialization");
	}

	private IConstructor resolveICPPUnaryTypeTransformation(ICPPUnaryTypeTransformation type, ISourceLocation src) {
		Operator operator = type.getOperator();
		IType operand = type.getOperand();
		throw new RuntimeException("NYI: resolveICPPUnaryTypeTransformation");
	}

	private IConstructor resolveICPPUnknownType(ICPPUnknownType type, ISourceLocation src) {
		if (type instanceof TypeOfDependentExpression)
			return builder.TypeSymbol_typeOfDependentExpression(src);
		throw new RuntimeException("NYI: resolveICPPUnknownType (" + type.getClass().getSimpleName() + ")");
	}

	private IConstructor resolveIEnumeration(IEnumeration type, ISourceLocation src) {
		try {
			ISourceLocation decl = br.resolveBinding(type);
			return builder.TypeSymbol_enumeration(decl);
		} catch (URISyntaxException e) {
			throw new RuntimeException("Could not resolve IEnumeration" + type);
		}
	}

	private IConstructor resolveIFunctionType(IFunctionType type, ISourceLocation src) {
		IConstructor returnType = resolveType(type.getReturnType(), src);
		IListWriter parameterTypes = vf.listWriter();
		Stream.of(type.getParameterTypes()).forEach(it -> parameterTypes.append(resolveType(it, src)));
		return builder.TypeSymbol_functionType(returnType, parameterTypes.done(), vf.bool(type.takesVarArgs()));
	}

	private IConstructor resolveIIndexType(IIndexType type, ISourceLocation src) {
		throw new RuntimeException("NYI: resolveIIndexType");
	}

	private IConstructor resolveIPointerType(IPointerType type, ISourceLocation src) {
		IListWriter modifiers = vf.listWriter();
		if (type.isConst())
			modifiers.append(builder.Modifier_const(src));
		if (type.isVolatile())
			modifiers.append(builder.Modifier_volatile(src));
		if (type.isRestrict())
			modifiers.append(builder.Modifier_restrict(src));
		return builder.TypeSymbol_pointerType(modifiers.done(), resolveType(type.getType(), src));
	}

	private IConstructor resolveIProblemBinding(IProblemBinding type, ISourceLocation src) {
		err("Encountered IProblemBinding " + type.getClass().getSimpleName() + ": ");
		err("\t" + type.getID() + ": " + type.getMessage());
		err("\t" + src);
		return builder.TypeSymbol_problemBinding();
	}

	private IConstructor resolveIProblemType(IProblemType type, ISourceLocation src) {
		err("Encountered IProblemType " + type.getClass().getSimpleName() + ": ");
		err("\t" + type.getID() + ": " + type.getMessage());
		err("\t" + src);
		return builder.TypeSymbol_problemType();
	}

	private IConstructor resolveIQualifierType(IQualifierType type, ISourceLocation src) {
		IConstructor baseType = resolveType(type.getType(), src);
		IListWriter modifiers = vf.listWriter();
		if (type.isConst())
			modifiers.append(builder.Modifier_const(src));
		if (type.isVolatile())
			modifiers.append(builder.Modifier_volatile(src));
		return builder.TypeSymbol_qualifierType(modifiers.done(), baseType);
	}

	private IConstructor resolveITypeContainer(ITypeContainer type, ISourceLocation src) {
		return builder.TypeSymbol_typeContainer(resolveType(type.getType(), src));
	}

	private IConstructor resolveITypedef(ITypedef type, ISourceLocation src) {
		IType _type = type.getType();
		throw new RuntimeException("NYI: resolveITypedef");
	}
}
