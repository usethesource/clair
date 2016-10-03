package lang.cpp.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;
import org.eclipse.cdt.core.dom.ast.ASTVisitor;
import org.eclipse.cdt.core.dom.ast.DOMException;
import org.eclipse.cdt.core.dom.ast.IASTASMDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTArrayDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTArrayModifier;
import org.eclipse.cdt.core.dom.ast.IASTAttribute;
import org.eclipse.cdt.core.dom.ast.IASTAttributeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression;
import org.eclipse.cdt.core.dom.ast.IASTBinaryTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTBreakStatement;
import org.eclipse.cdt.core.dom.ast.IASTCaseStatement;
import org.eclipse.cdt.core.dom.ast.IASTCastExpression;
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTCompoundStatement;
import org.eclipse.cdt.core.dom.ast.IASTConditionalExpression;
import org.eclipse.cdt.core.dom.ast.IASTContinueStatement;
import org.eclipse.cdt.core.dom.ast.IASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTDeclarationStatement;
import org.eclipse.cdt.core.dom.ast.IASTDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTDefaultStatement;
import org.eclipse.cdt.core.dom.ast.IASTDoStatement;
import org.eclipse.cdt.core.dom.ast.IASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier.IASTEnumerator;
import org.eclipse.cdt.core.dom.ast.IASTEqualsInitializer;
import org.eclipse.cdt.core.dom.ast.IASTExpression;
import org.eclipse.cdt.core.dom.ast.IASTExpressionList;
import org.eclipse.cdt.core.dom.ast.IASTExpressionStatement;
import org.eclipse.cdt.core.dom.ast.IASTFieldDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTFieldReference;
import org.eclipse.cdt.core.dom.ast.IASTForStatement;
import org.eclipse.cdt.core.dom.ast.IASTFunctionCallExpression;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDefinition;
import org.eclipse.cdt.core.dom.ast.IASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.IASTIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTIfStatement;
import org.eclipse.cdt.core.dom.ast.IASTImplicitName;
import org.eclipse.cdt.core.dom.ast.IASTInitializer;
import org.eclipse.cdt.core.dom.ast.IASTInitializerClause;
import org.eclipse.cdt.core.dom.ast.IASTInitializerList;
import org.eclipse.cdt.core.dom.ast.IASTLabelStatement;
import org.eclipse.cdt.core.dom.ast.IASTLiteralExpression;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNamedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTNullStatement;
import org.eclipse.cdt.core.dom.ast.IASTParameterDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTPointer;
import org.eclipse.cdt.core.dom.ast.IASTPointerOperator;
import org.eclipse.cdt.core.dom.ast.IASTProblem;
import org.eclipse.cdt.core.dom.ast.IASTProblemDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTProblemExpression;
import org.eclipse.cdt.core.dom.ast.IASTProblemStatement;
import org.eclipse.cdt.core.dom.ast.IASTReturnStatement;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTStandardFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTStatement;
import org.eclipse.cdt.core.dom.ast.IASTSwitchStatement;
import org.eclipse.cdt.core.dom.ast.IASTToken;
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.IASTTypeId;
import org.eclipse.cdt.core.dom.ast.IASTTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTTypeIdInitializerExpression;
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression;
import org.eclipse.cdt.core.dom.ast.IASTWhileStatement;
import org.eclipse.cdt.core.dom.ast.IArrayType;
import org.eclipse.cdt.core.dom.ast.IBasicType;
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind;
import org.eclipse.cdt.core.dom.ast.IBinding;
import org.eclipse.cdt.core.dom.ast.ICompositeType;
import org.eclipse.cdt.core.dom.ast.IEnumeration;
import org.eclipse.cdt.core.dom.ast.IField;
import org.eclipse.cdt.core.dom.ast.IPointerType;
import org.eclipse.cdt.core.dom.ast.IProblemBinding;
import org.eclipse.cdt.core.dom.ast.IProblemType;
import org.eclipse.cdt.core.dom.ast.IQualifierType;
import org.eclipse.cdt.core.dom.ast.IScope;
import org.eclipse.cdt.core.dom.ast.IType;
import org.eclipse.cdt.core.dom.ast.ITypedef;
import org.eclipse.cdt.core.dom.ast.c.ICASTArrayModifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignatedInitializer;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignator;
import org.eclipse.cdt.core.dom.ast.c.ICASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTAliasDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTArrayDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTArraySubscriptExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTBinaryExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCapture;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCastExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCatchHandler;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTClassVirtSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCompositeTypeSpecifier.ICPPASTBaseSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTConstructorChainInitializer;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTConstructorInitializer;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTConversionName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDecltypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDeleteExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDesignatedInitializer;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDesignator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTEnumerationSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTExplicitTemplateInstantiation;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTExpressionList;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFieldDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFieldReference;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionCallExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionDeclarator.RefQualifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionWithTryBlock;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTInitializerClause;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLinkageSpecification;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLiteralExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNameSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamespaceAlias;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamespaceDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNaryTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNewExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTOperatorName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTPackExpansionExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTParameterDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTQualifiedName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTRangeBasedForStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTReferenceOperator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTSimpleDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTSimpleTypeConstructorExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTSimpleTypeTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTStaticAssertDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateId;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplatedTypeTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTryBlockStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTypeTransformationSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUnaryExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDirective;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVirtSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVirtSpecifier.SpecifierKind;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVisibilityLabel;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPAliasTemplate;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPEnumeration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPEnumerationSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunctionScope;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPParameterPackType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPReferenceType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateArgument;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTemplateTypeParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPTypeSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPUnaryTypeTransformation;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPUnaryTypeTransformation.Operator;
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator;
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode;
import org.eclipse.cdt.internal.core.dom.parser.ITypeContainer;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPUnknownType;
import org.eclipse.cdt.internal.core.index.IIndexType;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.value.IConstructor;
import org.rascalmpl.value.IListWriter;
import org.rascalmpl.value.IValue;
import org.rascalmpl.value.IValueFactory;

public class CdtToRascalVisitor extends ASTVisitor {
	private IValueFactory vf;
	private AST builder;
	private IEvaluatorContext ctx;
	private Stack<IConstructor> stack = new Stack<IConstructor>();

	public CdtToRascalVisitor(IValueFactory vf) {
		super(true);
		this.vf = vf;
		this.builder = new AST(vf);
	}

	public void setIEvaluatorContext(IEvaluatorContext ctx) {
		this.ctx = ctx;
	}

	public IValue convert(IASTTranslationUnit tu) {
		tu.accept(this);
		if (stack.size() == 1)
			return stack.pop();
		if (stack.size() == 0)
			throw new RuntimeException("Stack empty after converting, error");
		IConstructor ast = stack.pop();
		err("Superfluous nodes on the stack after converting:");
		stack.iterator().forEachRemaining(it -> err(it.toString()));
		return ast;
	}

	private void out(String msg) {
		ctx.getStdOut().println(spaces() + msg);
	}

	private void err(String msg) {
		ctx.getStdErr().println(spaces() + msg);
	}

	@Override
	public int visit(IASTTranslationUnit tu) {
		IListWriter declarations = vf.listWriter();
		for (IASTDeclaration node : tu.getDeclarations()) {
			node.accept(this);
			declarations.append(stack.pop());
		}

		stack.push(builder.Declaration_translationUnit(declarations.done()));

		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTName name) {
		IBinding _binding = name.getBinding();
		int _role = name.getRoleOfName(true);
		boolean _isQualified = name.isQualified();

		if (name instanceof IASTImplicitName)
			visit((IASTImplicitName) name);
		else if (name instanceof ICPPASTName)
			visit((ICPPASTName) name);
		else {
			err("No sub-interfaced IASTName? " + name.getClass().getName() + ": " + name.getRawSignature());
			throw new RuntimeException("NYI");
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTImplicitName name) {
		err("IASTImplicitName " + name.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTName name) {
		if (name instanceof ICPPASTConversionName)
			visit((ICPPASTConversionName) name);
		else if (name instanceof ICPPASTOperatorName)
			visit((ICPPASTOperatorName) name);
		else if (name instanceof ICPPASTQualifiedName)
			visit((ICPPASTQualifiedName) name);
		else if (name instanceof ICPPASTTemplateId)
			visit((ICPPASTTemplateId) name);
		else {// TODO is this correct?
			stack.push(builder.Expression_name(name.toString()));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTConversionName name) {
		err("ICPPASTConversionName: " + name.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTOperatorName name) {
		err("ICPPASTOperatorName: " + name.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTQualifiedName name) {
		ICPPASTNameSpecifier[] _qualifier = name.getQualifier();
		IASTName _lastName = name.getLastName();
		boolean fullyQualified = name.isFullyQualified();
		boolean conversionOrOperator = name.isConversionOrOperator();

		if (_qualifier.length != 1) {
			err("ERROR: ICPPASTQualifiedName #qualifiers!=1, exiting");
			throw new RuntimeException("NYI");
		}

		_qualifier[0].accept(this);
		IConstructor qualifier = stack.pop();
		_lastName.accept(this);
		IConstructor lastName = stack.pop();
		if (fullyQualified || conversionOrOperator)
			err("WARNING: ICPPASTQualifiedName has unimplemented field set");
		stack.push(builder.Expression_qualifiedName(qualifier, lastName));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTemplateId name) {
		err("ICPPASTTemplateId: " + name.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(IASTDeclaration declaration) {
		if (declaration instanceof IASTASMDeclaration)
			visit((IASTASMDeclaration) declaration);
		else if (declaration instanceof IASTFunctionDefinition)
			visit((IASTFunctionDefinition) declaration);
		else if (declaration instanceof IASTSimpleDeclaration)
			visit((IASTSimpleDeclaration) declaration);
		else if (declaration instanceof ICPPASTAliasDeclaration)
			visit((ICPPASTAliasDeclaration) declaration);
		else if (declaration instanceof ICPPASTExplicitTemplateInstantiation)
			visit((ICPPASTExplicitTemplateInstantiation) declaration);
		else if (declaration instanceof ICPPASTLinkageSpecification)
			visit((ICPPASTLinkageSpecification) declaration);
		else if (declaration instanceof ICPPASTNamespaceAlias)
			visit((ICPPASTNamespaceAlias) declaration);
		// In ASTVisitor interface, not needed?
		// else if (declaration instanceof ICPPASTNamespaceDefinition)
		// visit((ICPPASTNamespaceDefinition) declaration);
		else if (declaration instanceof ICPPASTStaticAssertDeclaration)
			visit((ICPPASTStaticAssertDeclaration) declaration);
		else if (declaration instanceof ICPPASTTemplateDeclaration)
			visit((ICPPASTTemplateDeclaration) declaration);
		else if (declaration instanceof ICPPASTTemplateSpecialization)
			visit((ICPPASTTemplateSpecialization) declaration);
		else if (declaration instanceof ICPPASTUsingDeclaration)
			visit((ICPPASTUsingDeclaration) declaration);
		else if (declaration instanceof ICPPASTUsingDirective)
			visit((ICPPASTUsingDirective) declaration);
		else if (declaration instanceof ICPPASTVisibilityLabel)
			visit((ICPPASTVisibilityLabel) declaration);
		else if (declaration instanceof IASTProblemDeclaration)
			// should not happen
			visit((IASTProblemDeclaration) declaration);
		else {
			throw new RuntimeException(
					"Declaration: encountered non-implemented subtype " + declaration.getClass().getName());
		}

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTVisibilityLabel declaration) {
		int visibility = declaration.getVisibility();
		switch (visibility) {
		case ICPPASTVisibilityLabel.v_public:
			stack.push(builder.Modifier_public());
			break;
		case ICPPASTVisibilityLabel.v_protected:
			stack.push(builder.Modifier_protected());
			break;
		case ICPPASTVisibilityLabel.v_private:
			stack.push(builder.Modifier_private());
			break;
		default:
			throw new RuntimeException("Unknown CPPVisibilityLabel code " + visibility + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTUsingDirective declaration) {
		IASTName qualifiedName = declaration.getQualifiedName();
		qualifiedName.accept(this);
		stack.push(builder.Declaration_usingDirective(stack.pop()));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTUsingDeclaration declaration) {
		out("CPPUsingDeclaration: " + declaration.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTTemplateSpecialization declaration) {
		out("CPPTemplateSpecialization: " + declaration.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTTemplateDeclaration declaration) {
		boolean isExported = declaration.isExported();
		IASTDeclaration _declaration = declaration.getDeclaration();
		ICPPASTTemplateParameter[] _templateParameters = declaration.getTemplateParameters();
		IScope scope = declaration.getScope();
		IListWriter templateParameters = vf.listWriter();
		Stream.of(_templateParameters).forEach(it -> {
			it.accept(this);
			templateParameters.append(stack.pop());
		});
		_declaration.accept(this);
		stack.push(builder.Declaration_template(stack.pop(), templateParameters.done()));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTStaticAssertDeclaration declaration) {
		out("CPPStaticAssertDeclaration: " + declaration.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTNamespaceAlias declaration) {
		out("NamespaceAlias: " + declaration.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTLinkageSpecification declaration) {
		out("LinkageSpecification: " + declaration.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTExplicitTemplateInstantiation declaration) {
		out("CPPExplicitTemplateInstantiation: " + declaration.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTAliasDeclaration declaration) {
		out("CPPAliasDeclaration: " + declaration.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTProblemDeclaration declaration) {
		err("ProblemDeclaration: " + declaration.getProblem().getMessageWithLocation());
		err("ProblemDeclaration: " + declaration.getRawSignature());
		throw new RuntimeException("ERROR");
	}

	public int visit(IASTASMDeclaration declaration) {
		stack.push(builder.Declaration_asmDeclaration(declaration.getAssembly()));
		throw new RuntimeException("NYI");
	}

	static int prefix = 0;

	static String spaces() {
		return StringUtils.repeat(" ", prefix);
	}

	public synchronized int visit(IASTSimpleDeclaration declaration) {
		IASTDeclSpecifier _declSpecifier = declaration.getDeclSpecifier();
		IASTDeclarator[] _declarators = declaration.getDeclarators();

		IASTAttributeSpecifier[] attributeSpecifiers = declaration.getAttributeSpecifiers();
		IASTAttribute[] attributes = declaration.getAttributes();

		if (attributeSpecifiers.length > 0)
			err("WARNING: IASTSimpleDeclaration: attributeSpecifiers not empty");
		if (attributes.length > 0)
			err("WARNING: IASTSimpleDeclaration: attributes not empty");

		_declSpecifier.accept(this);
		IConstructor declSpecifier = stack.pop();
		IListWriter declarators = vf.listWriter();
		for (IASTDeclarator declarator : _declarators) {
			declarator.accept(this);
			declarators.append(stack.pop());
		}
		stack.push(builder.Declaration_simpleDeclaration(declSpecifier, declarators.done()));
		return PROCESS_ABORT;
	}

	public int visit(IASTFunctionDefinition definition) {
		if (definition instanceof ICPPASTFunctionDefinition) {
			IASTDeclSpecifier _declSpecifier = definition.getDeclSpecifier();
			IASTFunctionDeclarator _declarator = definition.getDeclarator();
			IASTStatement _body = definition.getBody();
			ICPPASTConstructorChainInitializer[] _memberInitializers = ((ICPPASTFunctionDefinition) definition)
					.getMemberInitializers();
			boolean isDefaulted = ((ICPPASTFunctionDefinition) definition).isDefaulted();
			boolean isDeleted = ((ICPPASTFunctionDefinition) definition).isDeleted();

			_declSpecifier.accept(this);
			IConstructor declSpecifier = stack.pop();
			_declarator.accept(this);
			IConstructor declarator = stack.pop();

			IListWriter memberInitializers = vf.listWriter();
			Stream.of(_memberInitializers).forEach(it -> {
				it.accept(this);
				memberInitializers.append(stack.pop());
			});

			if (isDefaulted && isDeleted)
				err("WARNING: IASTFunctionDefinition both deleted and defaulted");
			if (isDefaulted)
				stack.push(builder.Declaration_defaultedFunctionDefinition(declSpecifier, memberInitializers.done(),
						declarator));
			else if (isDeleted)
				stack.push(builder.Declaration_deletedFunctionDefinition(declSpecifier, memberInitializers.done(),
						declarator));
			else if (definition instanceof ICPPASTFunctionWithTryBlock) {
				ICPPASTCatchHandler[] _catchHandlers = ((ICPPASTFunctionWithTryBlock) definition).getCatchHandlers();
				IListWriter catchHandlers = vf.listWriter();
				Stream.of(_catchHandlers).forEach(it -> {
					it.accept(this);
					catchHandlers.append(stack.pop());
				});
				throw new RuntimeException("NYI");
			} else {
				_body.accept(this);
				stack.push(builder.Declaration_functionDefinition(declSpecifier, memberInitializers.done(), declarator,
						stack.pop()));
			}
		} else { // C Function definition
			if (true)
				throw new RuntimeException("QUE??");
			IASTDeclSpecifier _declSpecifier = definition.getDeclSpecifier();
			IASTFunctionDeclarator _declarator = definition.getDeclarator();
			IASTStatement _body = definition.getBody();

			_declSpecifier.accept(this);
			IConstructor declSpecifier = stack.pop();
			_declarator.accept(this);
			IConstructor declarator = stack.pop();
			_body.accept(this);
			IConstructor body = stack.pop();
			stack.push(builder.Declaration_functionDefinition(declSpecifier, declarator, body));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTInitializer initializer) {
		if (initializer instanceof IASTEqualsInitializer)
			visit((IASTEqualsInitializer) initializer);
		else if (initializer instanceof IASTInitializerList)
			visit((IASTInitializerList) initializer);
		else if (initializer instanceof ICASTDesignatedInitializer)
			visit((ICASTDesignatedInitializer) initializer);
		else if (initializer instanceof ICPPASTConstructorChainInitializer)
			visit((ICPPASTConstructorChainInitializer) initializer);
		else if (initializer instanceof ICPPASTConstructorInitializer)
			visit((ICPPASTConstructorInitializer) initializer);
		else if (initializer instanceof ICPPASTDesignatedInitializer)
			visit((ICPPASTDesignatedInitializer) initializer);
		else {
			throw new RuntimeException(
					"Initializer: encountered unknown subtype " + initializer.getClass().getSimpleName());
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTEqualsInitializer initializer) {
		IASTInitializerClause initializerClause = initializer.getInitializerClause();
		initializerClause.accept(this);
		stack.push(builder.Declaration_equalsInitializer(stack.pop()));
		return PROCESS_ABORT;
	}

	public int visit(IASTInitializerClause initializerClause) {
		if (initializerClause instanceof IASTExpression)
			visit((IASTExpression) initializerClause);
		else if (initializerClause instanceof IASTInitializerList)
			visit((IASTInitializerList) initializerClause);
		else if (initializerClause instanceof ICASTDesignatedInitializer)
			visit((ICASTDesignatedInitializer) initializerClause);
		else if (initializerClause instanceof ICPPASTInitializerClause)
			visit((ICPPASTInitializerClause) initializerClause);
		else
			throw new RuntimeException(
					"Unknown IASTInitializerClause subclass " + initializerClause.getClass().getName() + ". Exiting");
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTInitializerClause initializer) {
		err("ICPPASTInitializerClause: " + initializer.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTInitializerList initializer) {
		// err("IASTInitializerList: " +
		// initializer.getRawSignature());
		int size = initializer.getSize();
		IASTInitializerClause[] _clauses = initializer.getClauses();
		IListWriter clauses = vf.listWriter();
		Stream.of(_clauses).forEach(it -> {
			it.accept(this);
			clauses.append(stack.pop());
		});
		stack.push(builder.Declaration_initializerList(clauses.done()));
		return PROCESS_ABORT;
	}

	public int visit(ICASTDesignatedInitializer initializer) {
		err("ICASTDesignatedInitializer: " + initializer.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTConstructorChainInitializer initializer) {
		IASTName _memberInitializerId = initializer.getMemberInitializerId();
		IASTInitializer _memberInitializer = initializer.getInitializer();
		_memberInitializerId.accept(this);
		IConstructor memberInitializerId = stack.pop();
		_memberInitializer.accept(this);
		IConstructor memberInitializer = stack.pop();
		stack.push(builder.Declaration_constructorChainInitializer(memberInitializerId, memberInitializer));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTConstructorInitializer initializer) {
		IASTInitializerClause[] _arguments = initializer.getArguments();
		IListWriter arguments = vf.listWriter();
		Stream.of(_arguments).forEach(it -> {
			it.accept(this);
			arguments.append(stack.pop());
		});
		stack.push(builder.Expression_constructorInitializer(arguments.done()));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTDesignatedInitializer initializer) {
		err("ICPPASTDesignatedInitializer: " + initializer.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(IASTParameterDeclaration parameterDeclaration) {
		if (parameterDeclaration instanceof ICPPASTParameterDeclaration) {
			ICPPASTParameterDeclaration declaration = (ICPPASTParameterDeclaration) parameterDeclaration;
			IASTDeclSpecifier _declSpecifier = declaration.getDeclSpecifier();
			ICPPASTDeclarator _declarator = declaration.getDeclarator();

			_declSpecifier.accept(this);
			IConstructor declSpecifier = stack.pop();
			if (_declarator == null)
				stack.push(builder.Declaration_parameter(declSpecifier));
			else {
				_declarator.accept(this);
				stack.push(builder.Declaration_parameter(declSpecifier, stack.pop()));
			}
		} else {
			IASTDeclSpecifier _declSpecifier = parameterDeclaration.getDeclSpecifier();
			IASTDeclarator _declarator = parameterDeclaration.getDeclarator();

			_declSpecifier.accept(this);
			IConstructor declSpecifier = stack.pop();
			if (_declarator == null)
				stack.push(builder.Declaration_parameter(declSpecifier));
			else {
				_declarator.accept(this);
				stack.push(builder.Declaration_parameter(declSpecifier, stack.pop()));
			}
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTDeclarator declarator) {
		if (declarator instanceof IASTArrayDeclarator)
			visit((IASTArrayDeclarator) declarator);
		else if (declarator instanceof IASTFieldDeclarator)
			visit((IASTFieldDeclarator) declarator);
		else if (declarator instanceof IASTFunctionDeclarator)
			visit((IASTFunctionDeclarator) declarator);
		else if (declarator instanceof ICPPASTDeclarator)
			visit((ICPPASTDeclarator) declarator);
		else {
			IASTPointerOperator[] _pointerOperators = declarator.getPointerOperators();
			IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
			IASTName _name = declarator.getName();
			IASTInitializer _initializer = declarator.getInitializer();

			List<IConstructor> pointerOperators = new ArrayList<IConstructor>();
			for (IASTPointerOperator op : _pointerOperators) {
				op.accept(this);
				pointerOperators.add(stack.pop());
			}
			IConstructor nestedDeclarator = null;
			if (_nestedDeclarator != null) {
				_nestedDeclarator.accept(this);
				nestedDeclarator = stack.pop();
			}
			_name.accept(this);
			IConstructor name = stack.pop();
			IConstructor initializer = null;
			if (_initializer == null) {

			} else {
				_initializer.accept(this);
				initializer = stack.pop();
			}

			throw new RuntimeException("NYI");
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTArrayDeclarator declarator) {
		if (declarator instanceof IASTArrayDeclarator) {
			IASTArrayModifier[] _arrayModifiers = declarator.getArrayModifiers();
			IASTPointerOperator[] _pointerOperators = declarator.getPointerOperators();
			IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
			IASTName _name = declarator.getName();
			IASTInitializer initializer = declarator.getInitializer();
			IListWriter arrayModifiers = vf.listWriter();
			Stream.of(_arrayModifiers).forEach(it -> {
				it.accept(this);
				arrayModifiers.append(stack.pop());
			});
			IListWriter pointerOperators = vf.listWriter();
			Stream.of(_pointerOperators).forEach(it -> {
				it.accept(this);
				pointerOperators.append(stack.pop());
			});
			IConstructor nestedDeclarator;
			if (_nestedDeclarator != null) {
				_nestedDeclarator.accept(this);
				nestedDeclarator = stack.pop();
			}
			_name.accept(this);
			IConstructor name = stack.pop();
			if (_pointerOperators.length > 0 || _nestedDeclarator != null)
				err("WARNING: IASTArrayDeclarator encountered unimplemented field");
			stack.push(builder.Declaration_arraydeclarator(name, arrayModifiers.done()));
		} else
			throw new RuntimeException("NYI");
		return PROCESS_ABORT;
	}

	public int visit(IASTFieldDeclarator declarator) {
		err("FieldDeclarator: " + declarator.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTFunctionDeclarator declarator) {
		if (declarator instanceof IASTStandardFunctionDeclarator)
			visit((IASTStandardFunctionDeclarator) declarator);
		else if (declarator instanceof ICASTKnRFunctionDeclarator)
			visit((ICASTKnRFunctionDeclarator) declarator);
		else
			throw new RuntimeException(
					"Unknown FunctionDeclarator subtype " + declarator.getClass().getName() + ". Exiting");
		return PROCESS_ABORT;
	}

	public int visit(IASTStandardFunctionDeclarator declarator) {
		// Scope NYI
		if (declarator instanceof ICPPASTFunctionDeclarator)
			visit((ICPPASTFunctionDeclarator) declarator);
		else {
			IASTName _name = declarator.getName();
			IScope _functionScope = declarator.getFunctionScope();
			IASTParameterDeclaration[] _parameters = declarator.getParameters();
			boolean _takesVarArgs = declarator.takesVarArgs();

			_name.accept(this);
			IConstructor name = stack.pop();
			IListWriter parameters = vf.listWriter();
			for (IASTParameterDeclaration parameter : _parameters) {
				parameter.accept(this);
				parameters.append(stack.pop());
			}
			stack.push(builder.Expression_functionDeclarator(name, parameters.done()));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICASTKnRFunctionDeclarator declarator) {
		err("CKnRFunctionDeclarator: " + declarator.getRawSignature());
		IASTName[] names = declarator.getParameterNames();
		IASTDeclaration[] declarations = declarator.getParameterDeclarations();
		Map<IASTName, IASTDeclarator> map = new HashMap<IASTName, IASTDeclarator>();
		for (IASTName name : names)
			map.put(name, declarator.getDeclaratorForParameterName(name));

		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTDeclarator declarator) {
		if (declarator instanceof ICPPASTArrayDeclarator)
			visit((ICPPASTArrayDeclarator) declarator);
		else if (declarator instanceof ICPPASTFieldDeclarator)
			visit((ICPPASTFieldDeclarator) declarator);
		else if (declarator instanceof ICPPASTFunctionDeclarator)
			visit((ICPPASTFunctionDeclarator) declarator);
		else {
			IASTPointerOperator[] _pointerOperators = declarator.getPointerOperators();
			IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
			IASTName _name = declarator.getName();
			IASTInitializer _initializer = declarator.getInitializer();

			IListWriter pointerOperators = vf.listWriter();
			for (IASTPointerOperator op : _pointerOperators) {
				op.accept(this);
				pointerOperators.append(stack.pop());
			}
			IConstructor nestedDeclarator = null;
			if (_nestedDeclarator != null) {
				_nestedDeclarator.accept(this);
				nestedDeclarator = stack.pop();
			}
			_name.accept(this);
			IConstructor name = stack.pop();
			IConstructor initializer = null;
			if (_initializer == null) {
				stack.push(builder.Declaration_declarator(name, pointerOperators.done()));
			} else {
				_initializer.accept(this);
				initializer = stack.pop();
				stack.push(builder.Declaration_declarator(name, pointerOperators.done(), initializer));
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTArrayDeclarator declarator) {
		err("CPPArrayDeclarator: " + declarator.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTFieldDeclarator declarator) {
		err("CPPFieldDeclarator: " + declarator.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTFunctionDeclarator declarator) {
		// err("CPPFunctionDeclarator: " +
		// declarator.getRawSignature());
		IASTName _name = declarator.getName();
		boolean isConst = declarator.isConst();
		boolean isVolatile = declarator.isVolatile();
		boolean isMutable = declarator.isMutable();
		boolean isPureVirtual = declarator.isPureVirtual();
		RefQualifier refQualifier = declarator.getRefQualifier();
		IASTParameterDeclaration[] _parameters = declarator.getParameters();
		IASTTypeId[] exceptionSpecification = declarator.getExceptionSpecification();
		ICPPASTExpression noexceptExpression = declarator.getNoexceptExpression();
		IASTTypeId trailingReturnType = declarator.getTrailingReturnType();
		ICPPFunctionScope functionScope = declarator.getFunctionScope();
		boolean isOverride = declarator.isOverride();
		boolean isFinal = declarator.isFinal();
		ICPPASTVirtSpecifier[] _virtSpecifiers = declarator.getVirtSpecifiers();
		IScope _functionScope = declarator.getFunctionScope();
		boolean _takesVarArgs = declarator.takesVarArgs();

		if (isConst || isVolatile || isMutable || isPureVirtual || isOverride || isFinal || _takesVarArgs)
			err("WARNING: ICPPASTFunctionDeclarator has unimplemented field");
		if (refQualifier != null)
			err("WARNING: ICPPASTFunctionDeclarator has refQualifier, unimplemented");
		if (exceptionSpecification != null || noexceptExpression != null || trailingReturnType != null)
			err("WARNING: ICPPASTFunctionDeclarator has unimplemented field set");
		_name.accept(this);
		IConstructor name = stack.pop();
		IListWriter parameters = vf.listWriter();
		for (IASTParameterDeclaration parameter : _parameters) {
			parameter.accept(this);
			parameters.append(stack.pop());
		}
		IListWriter virtSpecifiers = vf.listWriter();
		Stream.of(_virtSpecifiers).forEach(it -> {
			it.accept(this);
			virtSpecifiers.append(stack.pop());
		});
		stack.push(builder.Expression_functionDeclarator(name, parameters.done(), virtSpecifiers.done()));
		return PROCESS_ABORT;
	}

	void info(String msg) {
		out(msg);
	}

	@Override
	public int visit(IASTDeclSpecifier declSpec) {
		if (declSpec instanceof IASTCompositeTypeSpecifier)
			visit((IASTCompositeTypeSpecifier) declSpec);
		else if (declSpec instanceof IASTElaboratedTypeSpecifier)
			visit((IASTElaboratedTypeSpecifier) declSpec);
		else if (declSpec instanceof IASTEnumerationSpecifier)
			visit((IASTEnumerationSpecifier) declSpec);
		else if (declSpec instanceof IASTNamedTypeSpecifier)
			visit((IASTNamedTypeSpecifier) declSpec);
		else if (declSpec instanceof IASTSimpleDeclSpecifier)
			visit((IASTSimpleDeclSpecifier) declSpec);
		else if (declSpec instanceof ICASTDeclSpecifier)
			visit((ICASTDeclSpecifier) declSpec);
		else if (declSpec instanceof ICPPASTDeclSpecifier)
			visit((ICPPASTDeclSpecifier) declSpec);
		// else if (declSpec instanceof IGPPASTDeclSpecifier) Deprecated
		// visit((IGPPASTDeclSpecifier) declSpec);
		else
			throw new RuntimeException("Unknown sub-class encountered: " + declSpec.getClass().getName() + ". Exiting");
		return PROCESS_ABORT;
	}

	public int visit(IASTCompositeTypeSpecifier declSpec) {
		if (declSpec instanceof ICASTCompositeTypeSpecifier)
			visit((ICASTCompositeTypeSpecifier) declSpec);
		else if (declSpec instanceof ICPPASTCompositeTypeSpecifier)
			visit((ICPPASTCompositeTypeSpecifier) declSpec);
		else
			throw new RuntimeException(
					"Unknown IASTCompositeTypeSpecifier subinterface " + declSpec.getClass().getName());
		return PROCESS_ABORT;
	}

	public int visit(ICASTCompositeTypeSpecifier declSpec) {
		int key = declSpec.getKey();
		IASTName _name = declSpec.getName();
		IASTDeclaration[] _members = declSpec.getMembers();
		IScope _scope = declSpec.getScope();
		_name.accept(this);
		IConstructor name = stack.pop();
		IListWriter members = vf.listWriter();
		Stream.of(_members).forEach(it -> {
			it.accept(this);
			members.append(stack.pop());
		});

		if (true)
			throw new RuntimeException("Unfinished");

		switch (key) {
		case IASTCompositeTypeSpecifier.k_struct:
			stack.push(builder.Declaration_struct(name, members.done()));
			break;
		case IASTCompositeTypeSpecifier.k_union:
			stack.push(builder.Declaration_union(name, members.done()));
			break;
		default:
			throw new RuntimeException("Unknown IASTCompositeTypeSpecifier code " + key + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTCompositeTypeSpecifier declSpec) {
		ICPPASTBaseSpecifier[] _baseSpecifiers = declSpec.getBaseSpecifiers();
		int key = declSpec.getKey();
		IASTName _name = declSpec.getName();
		IASTDeclaration[] _members = declSpec.getMembers();
		IScope _scope = declSpec.getScope();
		boolean isFinal = declSpec.isFinal();
		ICPPASTClassVirtSpecifier virtSpecifier = declSpec.getVirtSpecifier();

		if (virtSpecifier != null)
			err("WARNING: ICPPASTCompositeTypeSpecifier has virtSpecifier: " + virtSpecifier.getRawSignature());
		_name.accept(this);
		IConstructor name = stack.pop();
		IListWriter members = vf.listWriter();
		Stream.of(_members).forEach(it -> {
			it.accept(this);
			members.append(stack.pop());
		});
		IListWriter baseSpecifiers = vf.listWriter();
		Stream.of(_baseSpecifiers).forEach(it -> {
			it.accept(this);
			baseSpecifiers.append(stack.pop());
		});

		switch (key) {
		case ICPPASTCompositeTypeSpecifier.k_struct:
			stack.push(builder.Declaration_struct(name, baseSpecifiers.done(), members.done()));
			break;
		case ICPPASTCompositeTypeSpecifier.k_union:
			stack.push(builder.Declaration_union(name, baseSpecifiers.done(), members.done()));
			break;
		case ICPPASTCompositeTypeSpecifier.k_class:
			stack.push(builder.Declaration_class(name, baseSpecifiers.done(), members.done()));
			break;
		default:
			throw new RuntimeException("Unknown IASTCompositeTypeSpecifier code " + key + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTElaboratedTypeSpecifier declSpec) {
		if (declSpec instanceof ICASTElaboratedTypeSpecifier) {
			out("ElaboratedTypeSpecifier: " + declSpec.getRawSignature());
			throw new RuntimeException("NYI");
		} else if (declSpec instanceof ICPPASTElaboratedTypeSpecifier) {
			int kind = declSpec.getKind();
			IASTName _name = declSpec.getName();
			if (declSpec.isConst() || declSpec.isVolatile() || declSpec.isRestrict() || declSpec.isInline())
				err("WARNING: IASTElaboratedTypeSpecifier encountered unimplemented flag");
			_name.accept(this);
			switch (kind) {
			case ICPPASTElaboratedTypeSpecifier.k_enum:
				stack.push(builder.Declaration_etsEnum(stack.pop()));
				break;
			case ICPPASTElaboratedTypeSpecifier.k_struct:
				stack.push(builder.Declaration_etsStruct(stack.pop()));
				break;
			case ICPPASTElaboratedTypeSpecifier.k_union:
				stack.push(builder.Declaration_etsUnion(stack.pop()));
				break;
			case ICPPASTElaboratedTypeSpecifier.k_class:
				stack.push(builder.Declaration_etsClass(stack.pop()));
				break;
			default:
				throw new RuntimeException("IASTElaboratedTypeSpecifier encountered unknown kind " + kind);
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTEnumerationSpecifier declSpec) {
		IASTName _name = declSpec.getName();
		IASTEnumerator[] _enumerators = declSpec.getEnumerators();

		_name.accept(this);
		IConstructor name = stack.pop();
		IListWriter enumerators = vf.listWriter();
		Stream.of(_enumerators).forEach(it -> {
			it.accept(this);
			enumerators.append(stack.pop());
		});

		stack.push(builder.Declaration_enum(name.getName(), enumerators.done()));

		return PROCESS_ABORT;
	}

	public int visit(IASTNamedTypeSpecifier declSpec) {
		// int storageClass = declSpec.getStorageClass();
		boolean isConst = declSpec.isConst();
		boolean isVolatile = declSpec.isVolatile();
		boolean isRestrict = declSpec.isRestrict();
		boolean isInline = declSpec.isInline();

		IListWriter modifiers = vf.listWriter();
		if (isConst)
			modifiers.append(builder.Modifier_const());
		if (isVolatile)
			modifiers.append(builder.Modifier_volatile());
		if (isRestrict)
			modifiers.append(builder.Modifier_restrict());
		if (isInline)
			err("WARNING: IASTNamedTypeSpecifier has isInline=true, not implemented");
		declSpec.getName().accept(this);
		stack.push(builder.Expression_namedTypeSpecifier(stack.pop(), modifiers.done()));
		return PROCESS_ABORT;
	}

	public int visit(IASTSimpleDeclSpecifier declSpec) {
		// TODO: implement modifiers
		int type = declSpec.getType();
		boolean isSigned = declSpec.isSigned();
		boolean isUnsigned = declSpec.isUnsigned();
		boolean isShort = declSpec.isShort();
		boolean isLong = declSpec.isLong();
		boolean isLongLong = declSpec.isLongLong();
		boolean isComplex = declSpec.isComplex();
		boolean isImaginary = declSpec.isImaginary();
		IASTExpression declTypeExpression = declSpec.getDeclTypeExpression();

		IListWriter modifiers = vf.listWriter();
		if (isSigned)
			modifiers.append(builder.Modifier_signed());
		if (isUnsigned)
			modifiers.append(builder.Modifier_unsigned());
		if (isShort)
			modifiers.append(builder.Modifier_short());
		if (isLong)
			modifiers.append(builder.Modifier_long());
		if (isLongLong)
			modifiers.append(builder.Modifier_longlong());
		if (isComplex)
			modifiers.append(builder.Modifier_complex());
		if (isImaginary)
			modifiers.append(builder.Modifier_imaginary());

		switch (type) {
		case IASTSimpleDeclSpecifier.t_unspecified:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_unspecified()));
			break;
		case IASTSimpleDeclSpecifier.t_void:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_void()));
			break;
		case IASTSimpleDeclSpecifier.t_char:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_char()));
			break;
		case IASTSimpleDeclSpecifier.t_int:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_integer()));
			break;
		case IASTSimpleDeclSpecifier.t_float:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_float()));
			break;
		case IASTSimpleDeclSpecifier.t_double:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_double()));
			break;
		case IASTSimpleDeclSpecifier.t_bool:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_bool()));
			break;
		case IASTSimpleDeclSpecifier.t_wchar_t:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_wchar_t()));
			break;
		case IASTSimpleDeclSpecifier.t_typeof:
			declTypeExpression.accept(this);
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_typeof(), stack.pop()));
			break;
		case IASTSimpleDeclSpecifier.t_decltype:
			declTypeExpression.accept(this);
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_decltype(), stack.pop()));
			break;
		case IASTSimpleDeclSpecifier.t_auto:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_auto()));
			break;
		case IASTSimpleDeclSpecifier.t_char16_t:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_char16_t()));
			break;
		case IASTSimpleDeclSpecifier.t_char32_t:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_char32_t()));
			break;
		case IASTSimpleDeclSpecifier.t_int128:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_int128()));
			break;
		case IASTSimpleDeclSpecifier.t_float128:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_float128()));
			break;
		case IASTSimpleDeclSpecifier.t_decimal32:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_decimal128()));
			break;
		case IASTSimpleDeclSpecifier.t_decimal64:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_decimal64()));
			break;
		case IASTSimpleDeclSpecifier.t_decimal128:
			stack.push(builder.Declaration_declSpecifier(modifiers.done(), builder.Type_decimal128()));
			break;
		default:
			throw new RuntimeException("Unknown IASTSimpleDeclSpecifier kind " + type + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(ICASTDeclSpecifier declSpec) {
		out("CDeclSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTDeclSpecifier declSpec) {
		if (declSpec instanceof ICPPASTCompositeTypeSpecifier)
			visit((ICPPASTCompositeTypeSpecifier) declSpec);
		else if (declSpec instanceof ICPPASTElaboratedTypeSpecifier)
			visit((ICPPASTElaboratedTypeSpecifier) declSpec);
		else if (declSpec instanceof ICPPASTEnumerationSpecifier)
			visit((ICPPASTEnumerationSpecifier) declSpec);
		else if (declSpec instanceof ICPPASTNamedTypeSpecifier)
			visit((ICPPASTNamedTypeSpecifier) declSpec);
		else if (declSpec instanceof ICPPASTSimpleDeclSpecifier)
			visit((ICPPASTSimpleDeclSpecifier) declSpec);
		else if (declSpec instanceof ICPPASTTypeTransformationSpecifier)
			visit((ICPPASTTypeTransformationSpecifier) declSpec);
		else {
			throw new RuntimeException("NYI");
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTElaboratedTypeSpecifier declSpec) {
		out("CPPElaboratedTypeSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTEnumerationSpecifier declSpec) {
		out("CPPEnumerationSpecifier: " + declSpec.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNamedTypeSpecifier declSpec) {
		out("CPPNamedTypeSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTSimpleDeclSpecifier declSpec) {
		visit((IASTSimpleDeclSpecifier) declSpec);
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTypeTransformationSpecifier declSpec) {
		err("ICPPASTTypeTransformationSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(IASTArrayModifier arrayModifier) {
		if (arrayModifier instanceof ICASTArrayModifier) {
			throw new RuntimeException("NYI");
		} else {
			IASTExpression _constantExpression = arrayModifier.getConstantExpression();
			IASTAttributeSpecifier[] _attributeSpecifiers = arrayModifier.getAttributeSpecifiers();
			IASTAttribute[] _attributes = arrayModifier.getAttributes();

			_constantExpression.accept(this);
			if (_attributeSpecifiers.length > 0 || _attributes.length > 0)
				err("WARNING: IASTArrayModifier has unimplemented field set");
			stack.push(builder.Expression_arrayModifier(stack.pop()));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTPointerOperator ptrOperator) {
		if (ptrOperator instanceof IASTPointer)
			visit((IASTPointer) ptrOperator);
		else if (ptrOperator instanceof ICPPASTReferenceOperator)
			visit((ICPPASTReferenceOperator) ptrOperator);
		else
			throw new RuntimeException(
					"Unknown IASTPointerOperator subtype +" + ptrOperator.getClass().getName() + ". Exiting");
		return PROCESS_ABORT;
	}

	public int visit(IASTPointer pointer) {
		boolean isConst = pointer.isConst();
		boolean isVolatile = pointer.isVolatile();
		boolean isRestrict = pointer.isRestrict();
		if (isConst || isVolatile || isRestrict)
			err("WARNING: IASTPointer encountered unimplemented field set");
		stack.push(builder.Declaration_pointer());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTReferenceOperator referenceOperator) {
		boolean isRValueReference = referenceOperator.isRValueReference();
		if (isRValueReference)
			err("WARNING: ICPPASTReferenceOperator has isRValueReference=true ignored");
		stack.push(builder.Declaration_reference());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTAttribute attribute) {
		err("Attribute: " + attribute.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(IASTAttributeSpecifier specifier) {
		err("Specifier: " + specifier.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(IASTToken token) {
		err("Token: " + new String(token.getTokenCharImage()));
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(IASTExpression expression) {
		if (expression instanceof IASTBinaryExpression)
			visit((IASTBinaryExpression) expression);
		else if (expression instanceof IASTBinaryTypeIdExpression)// TODO
			visit((IASTBinaryTypeIdExpression) expression);
		else if (expression instanceof IASTCastExpression)
			visit((IASTCastExpression) expression);
		else if (expression instanceof IASTConditionalExpression)
			visit((IASTConditionalExpression) expression);
		else if (expression instanceof IASTExpressionList)// TODO
			visit((IASTExpressionList) expression);
		else if (expression instanceof IASTFieldReference)// TODO
			visit((IASTFieldReference) expression);
		else if (expression instanceof IASTFunctionCallExpression)
			visit((IASTFunctionCallExpression) expression);
		else if (expression instanceof IASTIdExpression)
			visit((IASTIdExpression) expression);
		else if (expression instanceof IASTLiteralExpression)
			visit((IASTLiteralExpression) expression);
		else if (expression instanceof IASTTypeIdExpression)
			visit((IASTTypeIdExpression) expression);
		else if (expression instanceof IASTTypeIdInitializerExpression)
			visit((IASTTypeIdInitializerExpression) expression);
		else if (expression instanceof IASTUnaryExpression)
			visit((IASTUnaryExpression) expression);
		else if (expression instanceof ICPPASTArraySubscriptExpression)
			visit((ICPPASTArraySubscriptExpression) expression);
		else if (expression instanceof ICPPASTBinaryExpression)
			// Move up
			visit((ICPPASTBinaryExpression) expression);
		else if (expression instanceof ICPPASTCastExpression)
			visit((ICPPASTCastExpression) expression);
		else if (expression instanceof ICPPASTDeleteExpression)
			visit((ICPPASTDeleteExpression) expression);
		else if (expression instanceof ICPPASTExpressionList)
			visit((ICPPASTExpressionList) expression);
		else if (expression instanceof ICPPASTFieldReference)
			visit((ICPPASTFieldReference) expression);
		else if (expression instanceof ICPPASTFunctionCallExpression)
			visit((ICPPASTFunctionCallExpression) expression);
		else if (expression instanceof ICPPASTLambdaExpression)
			visit((ICPPASTLambdaExpression) expression);
		else if (expression instanceof ICPPASTLiteralExpression)
			visit((ICPPASTLiteralExpression) expression);
		else if (expression instanceof ICPPASTNaryTypeIdExpression)
			visit((ICPPASTNaryTypeIdExpression) expression);
		else if (expression instanceof ICPPASTNewExpression)
			visit((ICPPASTNewExpression) expression);
		else if (expression instanceof ICPPASTPackExpansionExpression)
			visit((ICPPASTPackExpansionExpression) expression);
		else if (expression instanceof ICPPASTSimpleTypeConstructorExpression)
			visit((ICPPASTSimpleTypeConstructorExpression) expression);
		else if (expression instanceof ICPPASTTypeIdExpression)
			visit((ICPPASTTypeIdExpression) expression);
		else if (expression instanceof ICPPASTUnaryExpression)
			visit((ICPPASTUnaryExpression) expression);
		else if (expression instanceof IASTProblemExpression)
			// Should not happen
			visit((IASTProblemExpression) expression);
		else {
			throw new RuntimeException(
					"Expression: encountered non-implemented subtype " + expression.getClass().getName());
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTUnaryExpression expression) {
		out("CPPUnaryExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTBinaryExpression expression) {
		out("CPPBinaryExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTTypeIdExpression expression) {
		out("CPPTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTSimpleTypeConstructorExpression expression) {
		out("CPPSimpleTypeConstructorExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTPackExpansionExpression expression) {
		out("CPPPackExpansionExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTNewExpression expression) {
		boolean isGlobal = expression.isGlobal();
		boolean isArrayAllocation = expression.isArrayAllocation();
		boolean isNewTypeId = expression.isNewTypeId();
		IASTInitializerClause[] placementArguments = expression.getPlacementArguments();
		if (isGlobal || isArrayAllocation || isNewTypeId || placementArguments != null)
			err("WARNING: ICPPASTNewExpression has unimplemented field set. isGlobal=" + isGlobal
					+ ", isArrayAllocation=" + isArrayAllocation + ", isNewTypeId=" + isNewTypeId
					+ ", #placementArguments=" + (placementArguments == null ? null : placementArguments.length));
		IASTTypeId _typeId = expression.getTypeId();
		IASTInitializer _initializer = expression.getInitializer();

		_typeId.accept(this);
		IConstructor typeId = stack.pop();
		if (_initializer == null)
			stack.push(builder.Expression_new(typeId));
		else {
			_initializer.accept(this);
			IConstructor initializer = stack.pop();
			stack.push(builder.Expression_new(typeId, initializer));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNaryTypeIdExpression expression) {
		out("CPPNaryTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTLiteralExpression expression) {
		// This may never be reached
		visit((IASTLiteralExpression) expression);
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTLambdaExpression expression) {
		out("CPPLambdaExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTFunctionCallExpression expression) {
		out("CPPFunctionCallExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTFieldReference expression) {
		out("CPPFieldReference: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTExpressionList expression) {
		out("CPPExpressionList: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTDeleteExpression expression) {
		boolean isGlobal = expression.isGlobal();
		boolean isVectored = expression.isVectored();
		if (isGlobal || isVectored)
			err("WARNING: ICPPASTDeleteExpression encountered unimplemented field set: isGlobal=" + isGlobal
					+ ", isVectored=" + isVectored);
		IASTExpression operand = expression.getOperand();
		operand.accept(this);
		stack.push(builder.Expression_delete(stack.pop()));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTCastExpression expression) {
		out("CPPCastExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTArraySubscriptExpression expression) {
		ICPPASTExpression _arrayExpression = expression.getArrayExpression();
		ICPPASTInitializerClause _argument = expression.getArgument();

		_arrayExpression.accept(this);
		IConstructor arrayExpression = stack.pop();
		_argument.accept(this);
		IConstructor argument = stack.pop();

		stack.push(builder.Expression_arraySubscriptExpression(arrayExpression, argument));

		return PROCESS_ABORT;
	}

	public int visit(IASTTypeIdInitializerExpression expression) {
		out("TypeIdInitializerExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTTypeIdExpression expression) {
		out("TypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTProblemExpression expression) {
		out("ProblemExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTFunctionCallExpression expression) {
		IASTExpression _functionName = expression.getFunctionNameExpression();
		IASTInitializerClause[] _arguments = expression.getArguments();

		_functionName.accept(this);
		IConstructor functionName = stack.pop();
		IListWriter arguments = vf.listWriter();
		for (IASTInitializerClause argument : _arguments) {
			argument.accept(this);
			arguments.append(stack.pop());
		}
		stack.push(builder.Expression_functionCall(functionName, arguments.done()));
		return PROCESS_ABORT;
	}

	public IConstructor convertType(IType cdtType) {
		if (cdtType instanceof IArrayType) {
			IType _type = ((IArrayType) cdtType).getType();
			org.eclipse.cdt.core.dom.ast.IValue _size = ((IArrayType) cdtType).getSize();
			stack.push(builder.Type_arrayType(convertType(_type), _size.numericalValue().intValue()));
		} else if (cdtType instanceof IBasicType) {
			Kind kind = ((IBasicType) cdtType).getKind();
			boolean isSigned = ((IBasicType) cdtType).isSigned();
			boolean isUnsigned = ((IBasicType) cdtType).isUnsigned();
			boolean isShort = ((IBasicType) cdtType).isShort();
			boolean isLong = ((IBasicType) cdtType).isLong();
			boolean isLongLong = ((IBasicType) cdtType).isLongLong();
			boolean isComplex = ((IBasicType) cdtType).isComplex();
			boolean isImaginary = ((IBasicType) cdtType).isImaginary();

			IListWriter modifiers = vf.listWriter();
			if (isSigned)
				modifiers.append(builder.Modifier_signed());
			if (isUnsigned)
				modifiers.append(builder.Modifier_unsigned());
			if (isShort)
				modifiers.append(builder.Modifier_short());
			if (isLong)
				modifiers.append(builder.Modifier_long());
			if (isLongLong)
				modifiers.append(builder.Modifier_longlong());
			if (isComplex)
				modifiers.append(builder.Modifier_complex());
			if (isImaginary)
				modifiers.append(builder.Modifier_imaginary());

			switch (kind) {
			case eBoolean:
				return builder.Type_basicType(builder.Type_bool(), modifiers.done());
			case eChar:
				return builder.Type_basicType(builder.Type_char(), modifiers.done());
			case eChar16:
				return builder.Type_basicType(builder.Type_char16_t(), modifiers.done());
			case eChar32:
				return builder.Type_basicType(builder.Type_char32_t(), modifiers.done());
			case eDecimal128:
				return builder.Type_basicType(builder.Type_decimal128(), modifiers.done());
			case eDecimal32:
				return builder.Type_basicType(builder.Type_decimal32(), modifiers.done());
			case eDecimal64:
				return builder.Type_basicType(builder.Type_decimal64(), modifiers.done());
			case eDouble:
				return builder.Type_basicType(builder.Type_double(), modifiers.done());
			case eFloat:
				return builder.Type_basicType(builder.Type_float(), modifiers.done());
			case eFloat128:
				return builder.Type_basicType(builder.Type_float128(), modifiers.done());
			case eInt:
				return builder.Type_basicType(builder.Type_integer(), modifiers.done());
			case eInt128:
				return builder.Type_basicType(builder.Type_int128(), modifiers.done());
			case eNullPtr:
				return builder.Type_basicType(builder.Type_nullptr(), modifiers.done());
			case eUnspecified:
				return builder.Type_basicType(builder.Type_unspecified(), modifiers.done());
			case eVoid:
				return builder.Type_basicType(builder.Type_void(), modifiers.done());
			case eWChar:
				return builder.Type_basicType(builder.Type_wchar_t(), modifiers.done());
			default:
				throw new RuntimeException("Unknown basictype kind encountered: " + kind + ". Exiting");
			}
		} else if (cdtType instanceof ICompositeType) { // check subinterfaces
			int key = ((ICompositeType) cdtType).getKey();
			boolean isAnonymous = ((ICompositeType) cdtType).isAnonymous();
			IField[] _fields = ((ICompositeType) cdtType).getFields();
			IScope scope = ((ICompositeType) cdtType).getCompositeScope();
		} else if (cdtType instanceof ICPPAliasTemplate) {
			org.eclipse.cdt.core.dom.ast.IType _type = ((ICPPAliasTemplate) cdtType).getType();
			ICPPTemplateParameter[] _parameters = ((ICPPAliasTemplate) cdtType).getTemplateParameters();
		} else if (cdtType instanceof ICPPParameterPackType) {
			// Not actually a type?
			org.eclipse.cdt.core.dom.ast.IType _type = ((ICPPParameterPackType) cdtType).getType();
		} else if (cdtType instanceof ICPPReferenceType) {
			org.eclipse.cdt.core.dom.ast.IType _type = ((ICPPReferenceType) cdtType).getType();
			boolean isRValueReference = ((ICPPReferenceType) cdtType).isRValueReference();
		} else if (cdtType instanceof ICPPTemplateTypeParameter) {
			try {
				org.eclipse.cdt.core.dom.ast.IType _default = ((ICPPTemplateTypeParameter) cdtType).getDefault();
				short parameterPosition = ((ICPPTemplateTypeParameter) cdtType).getParameterPosition();
				short templateNestingLevel = ((ICPPTemplateTypeParameter) cdtType).getTemplateNestingLevel();
				int parameterId = ((ICPPTemplateTypeParameter) cdtType).getParameterID();
				ICPPTemplateArgument defaultValue = ((ICPPTemplateTypeParameter) cdtType).getDefaultValue();
				boolean isParameterPack = ((ICPPTemplateTypeParameter) cdtType).isParameterPack();
			} catch (DOMException e) {
				e.printStackTrace(ctx.getStdErr());
			}
		} else if (cdtType instanceof ICPPTypeSpecialization) {
			if (cdtType instanceof ICPPClassSpecialization) {

			} else if (cdtType instanceof ICPPEnumerationSpecialization) {

			} else
				throw new RuntimeException(
						"Unknown ICPPTypeSpecialization subclass " + cdtType.getClass().getName() + ". Exiting");
		} else if (cdtType instanceof ICPPUnaryTypeTransformation) {
			Operator _operator = ((ICPPUnaryTypeTransformation) cdtType).getOperator();
			org.eclipse.cdt.core.dom.ast.IType _operand = ((ICPPUnaryTypeTransformation) cdtType).getOperand();
		} else if (cdtType instanceof ICPPUnknownType) {
			// Do not implement?
		} else if (cdtType instanceof IEnumeration) {
			if (cdtType instanceof ICPPEnumeration) {

			} else {

			}
		} else if (cdtType instanceof IIndexType) {
			// Do not implement?
		} else if (cdtType instanceof IPointerType) {
			org.eclipse.cdt.core.dom.ast.IType _type = ((IPointerType) cdtType).getType();
			boolean isConst = ((IPointerType) cdtType).isConst();
			boolean isVolatile = ((IPointerType) cdtType).isVolatile();
			boolean isRestruct = ((IPointerType) cdtType).isRestrict();
		} else if (cdtType instanceof IProblemBinding) {
			throw new RuntimeException("IProblemBinding: " + ((IProblemBinding) cdtType).getMessage());
		} else if (cdtType instanceof IProblemType) {
			throw new RuntimeException("IProblemType: " + ((IProblemType) cdtType).getMessage());
		} else if (cdtType instanceof IQualifierType) {
			boolean isConst = ((IQualifierType) cdtType).isConst();
			boolean isVolatile = ((IQualifierType) cdtType).isVolatile();
			org.eclipse.cdt.core.dom.ast.IType _type = ((IQualifierType) cdtType).getType();
		} else if (cdtType instanceof ITypeContainer) {
			// Do not implement? CDT internal
		} else if (cdtType instanceof ITypedef) {
			org.eclipse.cdt.core.dom.ast.IType _type = ((ITypedef) cdtType).getType();
		} else { // unsubinterfaced classes

		}
		throw new RuntimeException("NYI");
	}

	public int visit(IASTFieldReference expression) {
		if (expression instanceof ICPPASTFieldReference) {
			ICPPASTFieldReference reference = (ICPPASTFieldReference) expression;
			IASTExpression _fieldOwner = reference.getFieldOwner();
			IType _fieldOwnerType = reference.getFieldOwnerType();// CPPClassType
			IASTName _fieldName = reference.getFieldName();

			_fieldOwner.accept(this);
			IConstructor fieldOwner = stack.pop();
			// _fieldOwnerType.accept(this);
			// IConstructor fieldOwnerType = stack.pop();
			_fieldName.accept(this);
			IConstructor fieldName = stack.pop();
			// HIER
			stack.push(builder.Expression_fieldReference(fieldOwner, fieldName, builder.Type_unspecified()));// TODO
		} else {
			IASTExpression fieldOwner = expression.getFieldOwner();
			IASTName fieldName = expression.getFieldName();
			boolean isPointerDereference = expression.isPointerDereference();
			throw new RuntimeException("NYI");
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTExpressionList expression) {
		out("ExpressionList: " + expression.getRawSignature());
		IASTExpression[] expressions = expression.getExpressions();
		throw new RuntimeException("NYI");
	}

	public int visit(IASTBinaryTypeIdExpression expression) {
		out("BinaryTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTConditionalExpression expression) {
		IASTExpression _condition = expression.getLogicalConditionExpression();
		IASTExpression _positive = expression.getPositiveResultExpression();
		IASTExpression _negative = expression.getNegativeResultExpression();

		_condition.accept(this);
		IConstructor condition = stack.pop();
		_positive.accept(this);
		IConstructor positive = stack.pop();
		_negative.accept(this);
		IConstructor negative = stack.pop();

		stack.push(builder.Expression_conditional(condition, positive, negative));

		return PROCESS_ABORT;
	}

	public int visit(IASTCastExpression expression) {
		// int _operator = expression.getOperator();
		IASTExpression _operand = expression.getOperand();
		IASTTypeId typeId = expression.getTypeId();
		_operand.accept(this);
		IConstructor operand = stack.pop();
		typeId.accept(this);
		IConstructor type = stack.pop();

		stack.push(builder.Expression_cast(type, operand));
		return PROCESS_ABORT;
	}

	public int visit(IASTUnaryExpression expression) {
		int operator = expression.getOperator();
		IASTExpression _operand = expression.getOperand();
		_operand.accept(this);
		IConstructor $expression = stack.pop();

		switch (operator) {
		case IASTUnaryExpression.op_prefixIncr:
			stack.push(builder.Expression_prefixIncr($expression));
			break;
		case IASTUnaryExpression.op_prefixDecr:
			stack.push(builder.Expression_prefixDecr($expression));
			break;
		case IASTUnaryExpression.op_plus:
			stack.push(builder.Expression_plus($expression));
			break;
		case IASTUnaryExpression.op_minus:
			stack.push(builder.Expression_minus($expression));
			break;
		case IASTUnaryExpression.op_star:
			stack.push(builder.Expression_star($expression));
			break;
		case IASTUnaryExpression.op_amper:
			stack.push(builder.Expression_amper($expression));
			break;
		case IASTUnaryExpression.op_tilde:
			stack.push(builder.Expression_tilde($expression));
			break;
		case IASTUnaryExpression.op_not:
			stack.push(builder.Expression_not($expression));
			break;
		case IASTUnaryExpression.op_sizeof:
			stack.push(builder.Expression_sizeof($expression));
			break;
		case IASTUnaryExpression.op_postFixIncr:
			stack.push(builder.Expression_postfixIncr($expression));
			break;
		case IASTUnaryExpression.op_postFixDecr:
			stack.push(builder.Expression_postfixDecr($expression));
			break;
		case IASTUnaryExpression.op_bracketedPrimary:
			stack.push(builder.Expression_bracketed($expression));
			break;
		case IASTUnaryExpression.op_throw:
			stack.push(builder.Expression_throw($expression));
			break;
		case IASTUnaryExpression.op_typeid:
			stack.push(builder.Expression_typeid($expression));
			break;
		// case IASTUnaryExpression.op_typeof: (14) typeOf is deprecated
		case IASTUnaryExpression.op_alignOf:
			stack.push(builder.Expression_alignOf($expression));
			break;
		case IASTUnaryExpression.op_sizeofParameterPack:
			stack.push(builder.Expression_sizeofParameterPack($expression));
			break;
		case IASTUnaryExpression.op_noexcept:
			stack.push(builder.Expression_noexcept($expression));
			break;
		case IASTUnaryExpression.op_labelReference:
			stack.push(builder.Expression_labelReference($expression));
			break;
		default:
			throw new RuntimeException("Unknown unary operator " + operator + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTLiteralExpression expression) {
		int kind = expression.getKind();
		String value = expression.toString();
		switch (kind) {
		case IASTLiteralExpression.lk_integer_constant:
			stack.push(builder.Expression_integerConstant(value));
			break;
		case IASTLiteralExpression.lk_float_constant:
			stack.push(builder.Expression_floatConstant(value));
			break;
		case IASTLiteralExpression.lk_char_constant:
			stack.push(builder.Expression_charConstant(value));
			break;
		case IASTLiteralExpression.lk_string_literal:
			stack.push(builder.Expression_stringLiteral(value));
			break;
		case IASTLiteralExpression.lk_this:
			stack.push(builder.Expression_this());
			break;
		case IASTLiteralExpression.lk_true:
			stack.push(builder.Expression_true());
			break;
		case IASTLiteralExpression.lk_false:
			stack.push(builder.Expression_false());
			break;
		case IASTLiteralExpression.lk_nullptr:
			stack.push(builder.Expression_nullptr());
			break;
		default:
			throw new RuntimeException("Encountered unknown literal kind " + kind + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTIdExpression expression) {
		stack.push(builder.Expression_name(expression.getName().toString()));
		return PROCESS_ABORT;
	}

	public int visit(IASTBinaryExpression expression) {
		IASTExpression _lhs = expression.getOperand1();
		_lhs.accept(this);
		IConstructor lhs = stack.pop();
		int op = expression.getOperator();
		IASTExpression _rhs = expression.getOperand2();
		_rhs.accept(this);
		IConstructor rhs = stack.pop();

		switch (op) {
		case IASTBinaryExpression.op_multiply:
			stack.push(builder.Expression_multiply(lhs, rhs));
			break;
		case IASTBinaryExpression.op_divide:
			stack.push(builder.Expression_divide(lhs, rhs));
			break;
		case IASTBinaryExpression.op_modulo:
			stack.push(builder.Expression_modulo(lhs, rhs));
			break;
		case IASTBinaryExpression.op_plus:
			stack.push(builder.Expression_plus(lhs, rhs));
			break;
		case IASTBinaryExpression.op_minus:
			stack.push(builder.Expression_minus(lhs, rhs));
			break;
		case IASTBinaryExpression.op_shiftLeft:
			stack.push(builder.Expression_shiftLeft(lhs, rhs));
			break;
		case IASTBinaryExpression.op_shiftRight:
			stack.push(builder.Expression_shiftRight(lhs, rhs));
			break;
		case IASTBinaryExpression.op_lessThan:
			stack.push(builder.Expression_lessThan(lhs, rhs));
			break;
		case IASTBinaryExpression.op_greaterThan:
			stack.push(builder.Expression_greaterThan(lhs, rhs));
			break;
		case IASTBinaryExpression.op_lessEqual:
			stack.push(builder.Expression_lessEqual(lhs, rhs));
			break;
		case IASTBinaryExpression.op_greaterEqual:
			stack.push(builder.Expression_greaterEqual(lhs, rhs));
			break;
		case IASTBinaryExpression.op_binaryAnd:
			stack.push(builder.Expression_binaryAnd(lhs, rhs));
			break;
		case IASTBinaryExpression.op_binaryXor:
			stack.push(builder.Expression_binaryXor(lhs, rhs));
			break;
		case IASTBinaryExpression.op_binaryOr:
			stack.push(builder.Expression_binaryOr(lhs, rhs));
			break;
		case IASTBinaryExpression.op_logicalAnd:
			stack.push(builder.Expression_logicalAnd(lhs, rhs));
			break;
		case IASTBinaryExpression.op_logicalOr:
			stack.push(builder.Expression_logicalOr(lhs, rhs));
			break;
		case IASTBinaryExpression.op_assign:
			stack.push(builder.Expression_assign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_multiplyAssign:
			stack.push(builder.Expression_multiplyAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_divideAssign:
			stack.push(builder.Expression_divideAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_moduloAssign:
			stack.push(builder.Expression_moduloAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_plusAssign:
			stack.push(builder.Expression_plusAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_minusAssign:
			stack.push(builder.Expression_minusAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_shiftLeftAssign:
			stack.push(builder.Expression_shiftLeftAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_shiftRightAssign:
			stack.push(builder.Expression_shiftRightAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_binaryAndAssign:
			stack.push(builder.Expression_binaryAndAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_binaryXorAssign:
			stack.push(builder.Expression_binaryXorAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_binaryOrAssign:
			stack.push(builder.Expression_binaryOrAssign(lhs, rhs));
			break;
		case IASTBinaryExpression.op_equals:
			stack.push(builder.Expression_equals(lhs, rhs));
			break;
		case IASTBinaryExpression.op_notequals:
			stack.push(builder.Expression_notEquals(lhs, rhs));
			break;
		case IASTBinaryExpression.op_pmdot:
			stack.push(builder.Expression_pmDot(lhs, rhs));
			break;
		case IASTBinaryExpression.op_pmarrow:
			stack.push(builder.Expression_pmArrow(lhs, rhs));
			break;
		case IASTBinaryExpression.op_max:
			stack.push(builder.Expression_max(lhs, rhs));
			break;
		case IASTBinaryExpression.op_min:
			stack.push(builder.Expression_min(lhs, rhs));
			break;
		case IASTBinaryExpression.op_ellipses:
			stack.push(builder.Expression_ellipses(lhs, rhs));
			break;
		default:
			throw new RuntimeException("Operator " + op + " unknown, exiting");
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTStatement statement) {
		// err("Statement: " + statement.getRawSignature() +
		// ", " + statement.getClass().getName());
		if (statement instanceof IASTBreakStatement)
			visit((IASTBreakStatement) statement);
		else if (statement instanceof IASTCaseStatement)
			visit((IASTCaseStatement) statement);
		else if (statement instanceof IASTCompoundStatement)
			visit((IASTCompoundStatement) statement);
		else if (statement instanceof IASTContinueStatement)
			visit((IASTContinueStatement) statement);
		else if (statement instanceof IASTDeclarationStatement)
			visit((IASTDeclarationStatement) statement);
		else if (statement instanceof IASTDefaultStatement)
			visit((IASTDefaultStatement) statement);
		else if (statement instanceof IASTDoStatement)
			visit((IASTDoStatement) statement);
		else if (statement instanceof IASTExpressionStatement)
			visit((IASTExpressionStatement) statement);
		else if (statement instanceof IASTForStatement)
			visit((IASTForStatement) statement);
		else if (statement instanceof IASTGotoStatement)// TODO
			visit((IASTGotoStatement) statement);
		else if (statement instanceof IASTIfStatement)
			visit((IASTIfStatement) statement);
		else if (statement instanceof IASTLabelStatement)// TODO
			visit((IASTLabelStatement) statement);
		else if (statement instanceof IASTNullStatement)// TODO
			visit((IASTNullStatement) statement);
		else if (statement instanceof IASTReturnStatement)// TODO
			visit((IASTReturnStatement) statement);
		else if (statement instanceof IASTSwitchStatement)
			visit((IASTSwitchStatement) statement);
		else if (statement instanceof IASTWhileStatement)
			visit((IASTWhileStatement) statement);
		else if (statement instanceof ICPPASTCatchHandler)
			visit((ICPPASTCatchHandler) statement);
		else if (statement instanceof ICPPASTRangeBasedForStatement)
			visit((ICPPASTRangeBasedForStatement) statement);
		else if (statement instanceof ICPPASTTryBlockStatement)
			visit((ICPPASTTryBlockStatement) statement);
		else if (statement instanceof IGNUASTGotoStatement) // needed?
			visit((IGNUASTGotoStatement) statement);
		else if (statement instanceof IASTProblemStatement)
			// Should not happen, will hopefully extract some useful hints
			visit((IASTProblemStatement) statement);
		else {
			throw new RuntimeException(
					"Statement: encountered non-implemented subtype " + statement.getClass().getName());
		}
		return PROCESS_ABORT;
	}

	public int visit(IGNUASTGotoStatement statement) {
		err("IGNUAstGotoStatement: " + statement.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTTryBlockStatement statement) {
		err("CPPTryBlockStatement: " + statement.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTRangeBasedForStatement statement) {
		err("CPPRangeBasedForStatement: " + statement.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTCatchHandler statement) {
		err("CPPCatchHandler: " + statement.getRawSignature());
		boolean isCatchAll = statement.isCatchAll();
		IASTStatement _catchBody = statement.getCatchBody();
		IASTDeclaration _declaration = statement.getDeclaration();
		IScope scope = statement.getScope();
		throw new RuntimeException("NYI");
	}

	public int visit(IASTReturnStatement statement) {
		IASTExpression returnValue = statement.getReturnValue();
		IASTInitializerClause returnArgument = statement.getReturnArgument();
		// TODO: returnArgument?
		if (returnValue == null)
			stack.push(builder.Statement_return());
		else {
			returnValue.accept(this);
			stack.push(builder.Statement_return(stack.pop()));
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTNullStatement statement) {
		stack.push(builder.Statement_nullStatement());
		return PROCESS_ABORT;
	}

	public int visit(IASTLabelStatement statement) {
		IASTName _name = statement.getName();
		IASTStatement _nestedStatement = statement.getNestedStatement();

		_name.accept(this);
		IConstructor name = stack.pop();
		_nestedStatement.accept(this);
		IConstructor nestedStatement = stack.pop();

		stack.push(builder.Statement_label(name.getName(), nestedStatement));
		return PROCESS_ABORT;
	}

	public int visit(IASTGotoStatement statement) {
		IASTName _name = statement.getName();
		_name.accept(this);
		IConstructor name = stack.pop();
		stack.push(builder.Statement_goto(name.getName()));
		return PROCESS_ABORT;
	}

	public int visit(IASTDoStatement statement) {
		IASTStatement _body = statement.getBody();
		IASTExpression _condition = statement.getCondition();

		_body.accept(this);
		IConstructor body = stack.pop();
		_condition.accept(this);
		IConstructor condition = stack.pop();
		stack.push(builder.Statement_do(condition, body));

		return PROCESS_ABORT;
	}

	public int visit(IASTContinueStatement statement) {
		stack.push(builder.Statement_continue());
		return PROCESS_ABORT;
	}

	public int visit(IASTWhileStatement statement) {
		IASTExpression _condition = statement.getCondition();
		IASTStatement _body = statement.getBody();

		_condition.accept(this);
		IConstructor condition = stack.pop();
		_body.accept(this);
		IConstructor body = stack.pop();
		stack.push(builder.Statement_while(condition, body));
		return PROCESS_ABORT;
	}

	public int visit(IASTDefaultStatement statement) {
		stack.push(builder.Statement_defaultCase());
		return PROCESS_ABORT;
	}

	public int visit(IASTBreakStatement statement) {
		stack.push(builder.Statement_break());
		return PROCESS_ABORT;
	}

	public int visit(IASTCaseStatement statement) {
		IASTExpression _expression = statement.getExpression();
		_expression.accept(this);
		IConstructor expression = stack.pop();
		stack.push(builder.Statement_case(expression));
		return PROCESS_ABORT;
	}

	public int visit(IASTSwitchStatement statement) {
		IASTExpression _controller = statement.getControllerExpression();
		IASTStatement _body = statement.getBody();

		_controller.accept(this);
		IConstructor controller = stack.pop();
		_body.accept(this);
		IConstructor body = stack.pop();

		stack.push(builder.Statement_switch(controller, body));

		return PROCESS_ABORT;
	}

	public int visit(IASTProblemStatement statement) {
		err(statement.getProblem().getMessage());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTForStatement statement) {
		IASTStatement _initializer = statement.getInitializerStatement();
		IASTExpression _condition = statement.getConditionExpression();
		IASTExpression _iteration = statement.getIterationExpression();
		IASTStatement _body = statement.getBody();

		_initializer.accept(this);
		IConstructor initializer = stack.pop();
		_condition.accept(this);
		IConstructor condition = stack.pop();
		_iteration.accept(this);
		IConstructor iteration = stack.pop();
		_body.accept(this);
		IConstructor body = stack.pop();

		stack.push(builder.Statement_for(initializer, condition, iteration, body));

		return PROCESS_ABORT;
	}

	public int visit(IASTCompoundStatement compoundStatement) {
		IASTStatement[] _statements = compoundStatement.getStatements();
		IListWriter statements = vf.listWriter();
		for (IASTStatement statement : _statements) {
			statement.accept(this);
			statements.append(stack.pop());
		}
		stack.push(builder.Statement_compoundStatement(statements.done()));
		return PROCESS_ABORT;
	}

	public int visit(IASTDeclarationStatement statement) {
		IASTDeclaration _declaration = statement.getDeclaration();
		_declaration.accept(this);
		stack.push(builder.Statement_declarationStatement(stack.pop()));
		return PROCESS_ABORT;
	}

	public int visit(IASTExpressionStatement statement) {
		IASTExpression _expression = statement.getExpression();
		_expression.accept(this);
		stack.push(builder.Statement_expressionStatement(stack.pop()));
		return PROCESS_ABORT;
	}

	public int visit(IASTIfStatement statement) {
		IASTExpression _condition = statement.getConditionExpression();
		IASTStatement _thenClause = statement.getThenClause();
		IASTStatement _elseClause = statement.getElseClause();

		_condition.accept(this);
		IConstructor condition = stack.pop();
		_thenClause.accept(this);
		IConstructor thenClause = stack.pop();

		if (_elseClause == null) {
			stack.push(builder.Statement_if(condition, thenClause));
		} else {
			_elseClause.accept(this);
			IConstructor elseClause = stack.pop();
			stack.push(builder.Statement_if(condition, thenClause, elseClause));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTTypeId typeId) {
		IASTDeclSpecifier _declSpecifier = typeId.getDeclSpecifier();
		IASTDeclarator _abstractDeclarator = typeId.getAbstractDeclarator();

		_declSpecifier.accept(this);
		// TODO: abstractDeclarator?
		IConstructor declSpecifier = stack.pop();
		// if (_abstractDeclarator.equals(String.class))
		stack.push(builder.Type_typeId(declSpecifier));
		// else {
		// out("QUE? " + _abstractDeclarator.getRawSignature());
		// out("name " + _abstractDeclarator.getName().toString());
		// throw new RuntimeException("NYI");
		// }
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTEnumerator enumerator) {
		IASTName _name = enumerator.getName();
		IASTExpression _value = enumerator.getValue();

		_name.accept(this);
		IConstructor name = stack.pop();
		if (_value == null)
			stack.push(builder.Declaration_enumerator(name.toString()));
		else {
			_value.accept(this);
			stack.push(builder.Declaration_enumerator(name.toString(), stack.pop()));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTProblem problem) {
		err("Problem: " + problem.getMessage());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(ICPPASTBaseSpecifier baseSpecifier) {
		boolean isVirtual = baseSpecifier.isVirtual();
		if (isVirtual)
			err("WARNING: ICPPASTBaseSpecifier has isVirtual set, not implemented");
		int visibility = baseSpecifier.getVisibility();
		ICPPASTNameSpecifier _nameSpecifier = baseSpecifier.getNameSpecifier();
		if (_nameSpecifier == null) {
			switch (visibility) {
			case ICPPASTBaseSpecifier.v_public:
				stack.push(builder.Declaration_baseSpecifier(builder.Modifier_public()));
				break;
			case ICPPASTBaseSpecifier.v_protected:
				stack.push(builder.Declaration_baseSpecifier(builder.Modifier_protected()));
				break;
			case ICPPASTBaseSpecifier.v_private:
				stack.push(builder.Declaration_baseSpecifier(builder.Modifier_private()));
				break;
			default:
				throw new RuntimeException("Unknown BaseSpecifier visibility code " + visibility + ". Exiting");
			}
		} else {
			_nameSpecifier.accept(this);
			switch (visibility) {
			case ICPPASTBaseSpecifier.v_public:
				stack.push(builder.Declaration_baseSpecifier(builder.Modifier_public(), stack.pop()));
				break;
			case ICPPASTBaseSpecifier.v_protected:
				stack.push(builder.Declaration_baseSpecifier(builder.Modifier_protected(), stack.pop()));
				break;
			case ICPPASTBaseSpecifier.v_private:
				stack.push(builder.Declaration_baseSpecifier(builder.Modifier_private(), stack.pop()));
				break;
			default:
				throw new RuntimeException("Unknown BaseSpecifier visibility code " + visibility + ". Exiting");
			}
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTNamespaceDefinition namespaceDefinition) {
		err("NamespaceDefinition: " + namespaceDefinition.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(ICPPASTTemplateParameter templateParameter) {
		boolean isParameterPack = templateParameter.isParameterPack();
		if (isParameterPack)
			err("WARNING: ICPPASTTemplateParameter has isParameterPack=true, unimplemented");
		if (templateParameter instanceof ICPPASTParameterDeclaration) {
			IASTDeclSpecifier _declSpecifier = ((ICPPASTParameterDeclaration) templateParameter).getDeclSpecifier();
			ICPPASTDeclarator _declarator = ((ICPPASTParameterDeclaration) templateParameter).getDeclarator();
			_declSpecifier.accept(this);
			IConstructor declSpecifier = stack.pop();
			if (_declarator == null)
				stack.push(builder.Declaration_parameter(declSpecifier));
			else {
				_declarator.accept(this);
				IConstructor declarator = stack.pop();
				stack.push(builder.Declaration_parameter(declSpecifier, declarator));
			}
		} else if (templateParameter instanceof ICPPASTSimpleTypeTemplateParameter) {
			int parameterType = ((ICPPASTSimpleTypeTemplateParameter) templateParameter).getParameterType();
			IASTTypeId defaultType = ((ICPPASTSimpleTypeTemplateParameter) templateParameter).getDefaultType();
			IASTName _name = ((ICPPASTSimpleTypeTemplateParameter) templateParameter).getName();
			_name.accept(this);
			IConstructor name = stack.pop();
			switch (parameterType) {
			case ICPPASTSimpleTypeTemplateParameter.st_class:
				stack.push(builder.Declaration_sttClass(name));
				break;
			case ICPPASTSimpleTypeTemplateParameter.st_typename:
				stack.push(builder.Declaration_sttTypename(name));
				break;
			default:
				throw new RuntimeException(
						"ICPPASTTemplateParameter encountered non-implemented parameter type " + parameterType);
			}
			if (defaultType != null)
				err("WARNING: ICPPASTTemplateParameter has defaultType, not implemented");
		} else if (templateParameter instanceof ICPPASTTemplatedTypeTemplateParameter) {
			throw new RuntimeException("NYI");
		} else
			throw new RuntimeException("ICPPASTTemplateParameter encountered unknown subtype "
					+ templateParameter.getClass().getName() + ". Exiting");
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTCapture capture) {
		err("Capture: " + capture.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(ICASTDesignator designator) {
		err("Designator: " + designator.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(ICPPASTDesignator designator) {
		err("DesignatorCPP: " + designator.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(ICPPASTVirtSpecifier virtSpecifier) {
		SpecifierKind kind = virtSpecifier.getKind();
		switch (kind) {
		case Final:
			stack.push(builder.Declaration_virtSpecifier(builder.Modifier_final()));
			break;
		case Override:
			stack.push(builder.Declaration_virtSpecifier(builder.Modifier_override()));
			break;
		default:
			throw new RuntimeException("ICPPASTVirtSpecifier encountered unknown SpecifierKind " + kind.name());
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTClassVirtSpecifier classVirtSpecifier) {
		err("ClassVirtSpecifier: " + classVirtSpecifier.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(ICPPASTDecltypeSpecifier decltypeSpecifier) {
		err("DecltypeSpecifier: " + decltypeSpecifier.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(ASTAmbiguousNode astAmbiguousNode) {
		err("AstAmbiguousNode: " + astAmbiguousNode.getRawSignature());
		throw new RuntimeException("NYI");
	}
}
