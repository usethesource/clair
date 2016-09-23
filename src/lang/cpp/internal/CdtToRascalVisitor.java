package lang.cpp.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.eclipse.cdt.core.dom.ast.ASTVisitor;
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
import org.eclipse.cdt.core.dom.ast.IASTInitializer;
import org.eclipse.cdt.core.dom.ast.IASTInitializerClause;
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
import org.eclipse.cdt.core.dom.ast.IScope;
import org.eclipse.cdt.core.dom.ast.c.ICASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTAliasDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTArrayDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTArraySubscriptExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTBinaryExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCapture;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCastExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCatchHandler;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTClassVirtSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCompositeTypeSpecifier.ICPPASTBaseSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDecltypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDeleteExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDesignator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTExplicitTemplateInstantiation;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTExpressionList;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFieldDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFieldReference;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionCallExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLinkageSpecification;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLiteralExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamespaceAlias;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamespaceDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNaryTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNewExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTPackExpansionExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTRangeBasedForStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTReferenceOperator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTSimpleTypeConstructorExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTStaticAssertDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTryBlockStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUnaryExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDirective;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVirtSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVisibilityLabel;
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator;
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.value.IConstructor;
import org.rascalmpl.value.IListWriter;
import org.rascalmpl.value.IValue;
import org.rascalmpl.value.IValueFactory;

public class CdtToRascalVisitor extends ASTVisitor {
	private IValueFactory vf;
	private AST builder;
	private IEvaluatorContext ctx;
	private Stack<IValue> stack = new Stack<IValue>();

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
		return stack.pop();
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

		// IBinding _binding = name.getBinding();
		// int _role = name.getRoleOfName(true);
		// IASTCompletionContext _completionContext =
		// name.getCompletionContext();
		// ILinkage _linkage = name.getLinkage();
		// IASTImageLocation _imageLocation = name.getImageLocation();
		// IASTName _lastName = name.getLastName();
		// char[] _lookupKey = name.getLookupKey();
		// ctx.getStdErr().println(_lookupKey);
		// IBinding _preBinding = name.getPreBinding();
		// boolean _isQualified = name.isQualified();

		stack.push(builder.Expression_name(name.toString()));
		return PROCESS_ABORT;
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
			ctx.getStdErr()
					.println("Declaration: encountered non-implemented subtype " + declaration.getClass().getName());
			stack.push(builder.Declaration_class(null));
		}

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTVisibilityLabel declaration) {
		ctx.getStdOut().println("CPPVisibilityLabel: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTUsingDirective declaration) {
		ctx.getStdOut().println("CPPUsingDirective: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTUsingDeclaration declaration) {
		ctx.getStdOut().println("CPPUsingDeclaration: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTemplateSpecialization declaration) {
		ctx.getStdOut().println("CPPTemplateSpecialization: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTemplateDeclaration declaration) {
		ctx.getStdOut().println("CPPTemplateDeclaration: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTStaticAssertDeclaration declaration) {
		ctx.getStdOut().println("CPPStaticAssertDeclaration: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNamespaceAlias declaration) {
		ctx.getStdOut().println("NamespaceAlias: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTLinkageSpecification declaration) {
		ctx.getStdOut().println("LinkageSpecification: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTExplicitTemplateInstantiation declaration) {
		ctx.getStdOut().println("CPPExplicitTemplateInstantiation: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTAliasDeclaration declaration) {
		ctx.getStdOut().println("CPPAliasDeclaration: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTProblemDeclaration declaration) {
		ctx.getStdErr().println("ProblemDeclaration: " + declaration.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTASMDeclaration declaration) {
		stack.push(builder.Declaration_asmDeclaration(declaration.getAssembly()));
		return PROCESS_ABORT;
	}

	public int visit(IASTSimpleDeclaration declaration) {
		IASTDeclSpecifier _declSpecifier = declaration.getDeclSpecifier();
		_declSpecifier.accept(this);
		IConstructor declSpecifier = (IConstructor) stack.pop();
		IASTDeclarator[] _declarators = declaration.getDeclarators();
		IListWriter declarators = vf.listWriter();
		for (IASTDeclarator declarator : _declarators) {
			declarator.accept(this);
			declarators.append(stack.pop());
		}
		stack.push(builder.Declaration_simpleDeclaration(declSpecifier, vf.list(declarators.done())));
		return PROCESS_ABORT;
	}

	public int visit(IASTFunctionDefinition definition) {
		IASTDeclSpecifier _declSpecifier = definition.getDeclSpecifier();
		IASTFunctionDeclarator _declarator = definition.getDeclarator();
		IASTStatement _body = definition.getBody();

		_declSpecifier.accept(this);
		IConstructor declSpecifier = (IConstructor) stack.pop();
		_declarator.accept(this);
		IConstructor declarator = (IConstructor) stack.pop();
		_body.accept(this);
		IConstructor body = (IConstructor) stack.pop();

		stack.push(builder.Declaration_functionDefinition(declSpecifier, declarator, body));

		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTInitializer initializer) {
		ctx.getStdOut().println(initializer.getClass().getName());
		ctx.getStdErr().println("Initializer: " + initializer.getRawSignature());
		if (initializer instanceof IASTEqualsInitializer)
			visit((IASTEqualsInitializer) initializer);
		else {
			ctx.getStdErr()
					.println("Initializer: encountered unknown subtype " + initializer.getClass().getSimpleName());
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTEqualsInitializer initializer) {
		IASTInitializerClause initializerClause = initializer.getInitializerClause();
		if (!(initializerClause instanceof IASTExpression)) {
			ctx.getStdErr().println("EqualsInitializer: encountered unknown initializerClause "
					+ initializerClause.getClass().getName());
		}
		IASTExpression clause = (IASTExpression) initializerClause;
		clause.accept(this);
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTParameterDeclaration parameterDeclaration) {
		ctx.getStdErr().println("ParameterDeclaration: " + parameterDeclaration.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTDeclarator declarator) {
		ctx.getStdOut().println(declarator.getRawSignature());
		if (declarator instanceof IASTArrayDeclarator)
			visit((IASTArrayDeclarator) declarator);
		else if (declarator instanceof IASTFieldDeclarator)
			visit((IASTFieldDeclarator) declarator);
		else if (declarator instanceof IASTFunctionDeclarator)
			visit((IASTFunctionDeclarator) declarator);
		else if (declarator instanceof ICPPASTDeclarator)
			visit((ICPPASTDeclarator) declarator);
		else {
			ctx.getStdErr().println("CASTDeclarator? " + declarator.getClass().getName());
			IASTPointerOperator[] _pointerOperators = declarator.getPointerOperators();
			IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
			IASTName _name = declarator.getName();
			IASTInitializer _initializer = declarator.getInitializer();

			List<IConstructor> pointerOperators = new ArrayList<IConstructor>();
			for (IASTPointerOperator op : _pointerOperators) {
				op.accept(this);
				pointerOperators.add((IConstructor) stack.pop());
			}
			IConstructor nestedDeclarator = null;
			if (_nestedDeclarator != null) {
				_nestedDeclarator.accept(this);
				nestedDeclarator = (IConstructor) stack.pop();
			}
			_name.accept(this);
			IConstructor name = (IConstructor) stack.pop();
			IConstructor initializer = null;
			if (_initializer == null) {

			} else {
				_initializer.accept(this);
				initializer = (IConstructor) stack.pop();
			}

			stack.push(vf.string("TODODeclarator:" + declarator.getRawSignature()));
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTArrayDeclarator declarator) {
		ctx.getStdErr().println("ArrayDeclarator: " + declarator.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTFieldDeclarator declarator) {
		ctx.getStdErr().println("FieldDeclarator: " + declarator.getRawSignature());
		return PROCESS_ABORT;
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
		ctx.getStdErr().println("Scope NYI. Implement CPPStandardFunctionDeclarator?");
		IScope _functionScope = declarator.getFunctionScope();
		IASTParameterDeclaration[] _parameters = declarator.getParameters();
		boolean _takesVarArgs = declarator.takesVarArgs();

		IListWriter parameters = vf.listWriter();
		for (IASTParameterDeclaration parameter : _parameters) {
			parameter.accept(this);
			parameters.append(builder.Expression_nyi("parameternyi"));
		}

		stack.push(builder.Expression_functionDeclarator(parameters.done()));

		return PROCESS_ABORT;
	}

	public int visit(ICASTKnRFunctionDeclarator declarator) {
		ctx.getStdErr().println("CKnRFunctionDeclarator: " + declarator.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTDeclarator declarator) {
		// array field function
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

			List<IConstructor> pointerOperators = new ArrayList<IConstructor>();
			for (IASTPointerOperator op : _pointerOperators) {
				op.accept(this);
				pointerOperators.add((IConstructor) stack.pop());
			}
			IConstructor nestedDeclarator = null;
			if (_nestedDeclarator != null) {
				_nestedDeclarator.accept(this);
				nestedDeclarator = (IConstructor) stack.pop();
			}
			_name.accept(this);
			IConstructor name = (IConstructor) stack.pop();
			IConstructor initializer = null;
			if (_initializer == null) {

			} else {
				_initializer.accept(this);
				initializer = (IConstructor) stack.pop();
			}

			stack.push(vf.string("TODOCPPDeclarator:" + declarator.getRawSignature()));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTArrayDeclarator declarator) {
		ctx.getStdErr().println("CPPArrayDeclarator: " + declarator.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTFieldDeclarator declarator) {
		ctx.getStdErr().println("CPPFieldDeclarator: " + declarator.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTFunctionDeclarator declarator) {
		ctx.getStdErr().println("CPPFunctionDeclarator: " + declarator.getRawSignature());
		return PROCESS_ABORT;
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
		ctx.getStdOut().println("CompositeTypeSpecifier: " + declSpec.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTElaboratedTypeSpecifier declSpec) {
		ctx.getStdOut().println("ElaboratedTypeSpecifier: " + declSpec.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTEnumerationSpecifier declSpec) {
		ctx.getStdOut().println("EnumerationSpecifier: " + declSpec.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTNamedTypeSpecifier declSpec) {
		ctx.getStdOut().println("NamedTypeSpecifier: " + declSpec.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTSimpleDeclSpecifier declSpec) {
		// TODO: implement modifiers
		ctx.getStdOut().println("SimpleDeclSpecifier not fully implemented yet");
		int type = declSpec.getType();
		boolean isSigned = declSpec.isSigned();
		boolean isUnsigned = declSpec.isUnsigned();
		boolean isShort = declSpec.isShort();
		boolean isLong = declSpec.isLong();
		boolean isLongLOng = declSpec.isLongLong();
		boolean isComplex = declSpec.isComplex();
		boolean isImaginary = declSpec.isImaginary();
		IASTExpression declTypeExpression = declSpec.getDeclTypeExpression();

		switch (type) {
		case IASTSimpleDeclSpecifier.t_unspecified:
			stack.push(builder.Modifier_unspecified());
			break;
		case IASTSimpleDeclSpecifier.t_void:
			stack.push(builder.Type_void());
			break;
		case IASTSimpleDeclSpecifier.t_char:
			stack.push(builder.Type_char());
			break;
		case IASTSimpleDeclSpecifier.t_int:
			stack.push(builder.Type_int());
			break;
		case IASTSimpleDeclSpecifier.t_float:
			stack.push(builder.Type_float());
			break;
		case IASTSimpleDeclSpecifier.t_double:
			stack.push(builder.Type_double());
			break;
		case IASTSimpleDeclSpecifier.t_bool:
			stack.push(builder.Type_bool());
			break;
		case IASTSimpleDeclSpecifier.t_wchar_t:
			stack.push(builder.Type_wchar_t());
			break;
		case IASTSimpleDeclSpecifier.t_typeof:
			stack.push(builder.Type_typeof());
			break;
		case IASTSimpleDeclSpecifier.t_decltype:
			stack.push(builder.Type_decltype());
			break;
		case IASTSimpleDeclSpecifier.t_auto:
			stack.push(builder.Type_auto());
			break;
		case IASTSimpleDeclSpecifier.t_char16_t:
			stack.push(builder.Type_char16_t());
			break;
		case IASTSimpleDeclSpecifier.t_char32_t:
			stack.push(builder.Type_char32_t());
			break;
		case IASTSimpleDeclSpecifier.t_int128:
			stack.push(builder.Type_int128());
			break;
		case IASTSimpleDeclSpecifier.t_float128:
			stack.push(builder.Type_float128());
			break;
		case IASTSimpleDeclSpecifier.t_decimal32:
			stack.push(builder.Type_decimal32());
			break;
		case IASTSimpleDeclSpecifier.t_decimal64:
			stack.push(builder.Type_decimal64());
			break;
		case IASTSimpleDeclSpecifier.t_decimal128:
			stack.push(builder.Type_decimal128());
			break;
		default:
			throw new RuntimeException("Unknown IASTSimpleDeclSpecifier kind " + type + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(ICASTDeclSpecifier declSpec) {
		ctx.getStdOut().println("CDeclSpecifier: " + declSpec.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTDeclSpecifier declSpec) {
		ctx.getStdOut().println("CPPDeclSpecifier: " + declSpec.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTArrayModifier arrayModifier) {
		ctx.getStdErr().println("ArrayModifier: " + arrayModifier.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTPointerOperator ptrOperator) {
		// Stream.of(Thread.currentThread().getStackTrace()).forEach(it ->
		// ctx.getStdOut().println(it));
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
		ctx.getStdErr().println("Pointer: " + pointer.getRawSignature());
		boolean isConst = pointer.isConst();
		boolean isVolatile = pointer.isVolatile();
		boolean isRestrict = pointer.isRestrict();
		stack.push(builder.Declaration_pointerNYI());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTReferenceOperator referenceOperator) {
		ctx.getStdErr().println("CPPReferenceOperator: " + referenceOperator.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTAttribute attribute) {
		ctx.getStdErr().println("Attribute: " + attribute.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTAttributeSpecifier specifier) {
		ctx.getStdErr().println("Specifier: " + specifier.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTToken token) {
		ctx.getStdErr().println("Token: " + new String(token.getTokenCharImage()));
		return PROCESS_ABORT;
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
			ctx.getStdErr()
					.println("Expression: encountered non-implemented subtype " + expression.getClass().getName());
			stack.push(vf.string("TODO:" + expression.getRawSignature()));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTUnaryExpression expression) {
		ctx.getStdOut().println("CPPUnaryExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTypeIdExpression expression) {
		ctx.getStdOut().println("CPPTypeIdExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTSimpleTypeConstructorExpression expression) {
		ctx.getStdOut().println("CPPSimpleTypeConstructorExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTPackExpansionExpression expression) {
		ctx.getStdOut().println("CPPPackExpansionExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNewExpression expression) {
		ctx.getStdOut().println("NewExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNaryTypeIdExpression expression) {
		ctx.getStdOut().println("CPPNaryTypeIdExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTLiteralExpression expression) {
		ctx.getStdOut().println("CPPLiteralExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTLambdaExpression expression) {
		ctx.getStdOut().println("CPPLambdaExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTFunctionCallExpression expression) {
		ctx.getStdOut().println("CPPFunctionCallExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTFieldReference expression) {
		ctx.getStdOut().println("CPPFieldReference: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTExpressionList expression) {
		ctx.getStdOut().println("CPPExpressionList: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTDeleteExpression expression) {
		ctx.getStdOut().println("CPPDeleteExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTCastExpression expression) {
		ctx.getStdOut().println("CPPCastExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTArraySubscriptExpression expression) {
		ctx.getStdOut().println("CPPArraySubscriptExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTTypeIdInitializerExpression expression) {
		ctx.getStdOut().println("TypeIdInitializerExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTTypeIdExpression expression) {
		ctx.getStdOut().println("TypeIdExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTProblemExpression expression) {
		ctx.getStdOut().println("ProblemExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTFunctionCallExpression expression) {
		IASTExpression _functionName = expression.getFunctionNameExpression();
		IASTInitializerClause[] _arguments = expression.getArguments();

		_functionName.accept(this);
		IConstructor functionName = (IConstructor) stack.pop();
		IListWriter arguments = vf.listWriter();
		for (IASTInitializerClause argument : _arguments) {
			argument.accept(this);
			arguments.append(stack.pop());
		}
		stack.push(builder.Expression_functionCall(functionName, arguments.done()));
		return PROCESS_ABORT;
	}

	public int visit(IASTFieldReference expression) {
		ctx.getStdOut().println("FieldReference: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTExpressionList expression) {
		ctx.getStdOut().println("ExpressionList: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTBinaryTypeIdExpression expression) {
		ctx.getStdOut().println("BinaryTypeIdExpression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTConditionalExpression expression) {
		IASTExpression _condition = expression.getLogicalConditionExpression();
		IASTExpression _positive = expression.getPositiveResultExpression();
		IASTExpression _negative = expression.getNegativeResultExpression();

		_condition.accept(this);
		IConstructor condition = (IConstructor) stack.pop();
		_positive.accept(this);
		IConstructor positive = (IConstructor) stack.pop();
		_negative.accept(this);
		IConstructor negative = (IConstructor) stack.pop();

		stack.push(builder.Expression_conditional(condition, positive, negative));

		return PROCESS_ABORT;
	}

	public int visit(IASTCastExpression expression) {
		// int _operator = expression.getOperator();
		IASTExpression _operand = expression.getOperand();
		IASTTypeId typeId = expression.getTypeId();
		_operand.accept(this);
		IConstructor operand = (IConstructor) stack.pop();
		typeId.accept(this);
		IConstructor type = (IConstructor) stack.pop();

		stack.push(builder.Expression_cast(type, operand));
		return PROCESS_ABORT;
	}

	public int visit(IASTUnaryExpression expression) {
		int operator = expression.getOperator();
		IASTExpression _operand = expression.getOperand();
		_operand.accept(this);
		IConstructor $expression = (IConstructor) stack.pop();

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
		ctx.getStdOut().println("IdExpression: " + expression.getName().toString());
		stack.push(builder.Expression_name(expression.getName().toString()));
		return PROCESS_ABORT;
	}

	public int visit(IASTBinaryExpression expression) {
		IASTExpression _lhs = expression.getOperand1();
		_lhs.accept(this);
		IConstructor lhs = (IConstructor) stack.pop();
		int op = expression.getOperator();
		IASTExpression _rhs = expression.getOperand2();
		_rhs.accept(this);
		IConstructor rhs = (IConstructor) stack.pop();

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
		// ctx.getStdErr().println("Statement: " + statement.getRawSignature() +
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
			ctx.getStdErr().println("Statement: encountered non-implemented subtype " + statement.getClass().getName());
			stack.push(vf.bool(false));
		}
		return PROCESS_ABORT;
	}

	public int visit(IGNUASTGotoStatement statement) {
		ctx.getStdErr().println("IGNUAstGotoStatement: " + statement.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTryBlockStatement statement) {
		ctx.getStdErr().println("CPPTryBlockStatement: " + statement.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTRangeBasedForStatement statement) {
		ctx.getStdErr().println("CPPRangeBasedForStatement: " + statement.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTCatchHandler statement) {
		ctx.getStdErr().println("CPPCatchHandler: " + statement.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTReturnStatement statement) {
		ctx.getStdErr().println("IASTReturnStatement: " + statement.getRawSignature());
		IASTExpression returnValue = statement.getReturnValue();
		IASTInitializerClause returnArgument = statement.getReturnArgument();
		// TODO: returnArgument?
		if (returnValue == null)
			stack.push(builder.Statement_return());
		else {
			returnValue.accept(this);
			stack.push(builder.Statement_return((IConstructor) stack.pop()));
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTNullStatement statement) {
		stack.push(builder.Statement_nullStatement());
		return PROCESS_ABORT;
	}

	public int visit(IASTLabelStatement statement) {
		ctx.getStdErr().println("IASTLabelStatement: " + statement.getRawSignature());
		IASTName name = statement.getName();
		IASTStatement nestedStatement = statement.getNestedStatement();
		return PROCESS_ABORT;
	}

	public int visit(IASTGotoStatement statement) {
		ctx.getStdErr().println("IASTGotoStatement: " + statement.getRawSignature());
		IASTName name = statement.getName();
		return PROCESS_ABORT;
	}

	public int visit(IASTDoStatement statement) {
		IASTStatement _body = statement.getBody();
		IASTExpression _condition = statement.getCondition();

		_body.accept(this);
		IConstructor body = (IConstructor) stack.pop();
		_condition.accept(this);
		IConstructor condition = (IConstructor) stack.pop();
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
		IConstructor condition = (IConstructor) stack.pop();
		_body.accept(this);
		IConstructor body = (IConstructor) stack.pop();
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
		IConstructor expression = (IConstructor) stack.pop();
		stack.push(builder.Statement_case(expression));
		return PROCESS_ABORT;
	}

	public int visit(IASTSwitchStatement statement) {
		IASTExpression _controller = statement.getControllerExpression();
		IASTStatement _body = statement.getBody();

		_controller.accept(this);
		IConstructor controller = (IConstructor) stack.pop();
		_body.accept(this);
		IConstructor body = (IConstructor) stack.pop();

		stack.push(builder.Statement_switch(controller, body));

		return PROCESS_ABORT;
	}

	public int visit(IASTProblemStatement statement) {
		ctx.getStdErr().println(statement.getProblem().getMessage());
		return PROCESS_ABORT;
	}

	public int visit(IASTForStatement statement) {
		IASTStatement _initializer = statement.getInitializerStatement();
		IASTExpression _condition = statement.getConditionExpression();
		IASTExpression _iteration = statement.getIterationExpression();
		IASTStatement _body = statement.getBody();

		_initializer.accept(this);
		IConstructor initializer = (IConstructor) stack.pop();
		_condition.accept(this);
		IConstructor condition = (IConstructor) stack.pop();
		_iteration.accept(this);
		IConstructor iteration = (IConstructor) stack.pop();
		_body.accept(this);
		IConstructor body = (IConstructor) stack.pop();

		stack.push(builder.Statement_for(initializer, condition, iteration, body));

		return PROCESS_ABORT;
	}

	public int visit(IASTCompoundStatement compoundStatement) {
		IASTStatement[] _statements = compoundStatement.getStatements();
		IListWriter statements = vf.listWriter();
		for (IASTStatement statement : _statements) {
			statement.accept(this);
			statements.append((IConstructor) stack.pop());
		}
		stack.push(builder.Statement_compoundStatement(statements.done()));
		return PROCESS_ABORT;
	}

	public int visit(IASTDeclarationStatement statement) {
		IASTDeclaration _declaration = statement.getDeclaration();
		_declaration.accept(this);
		stack.push(builder.Statement_declarationStatement((IConstructor) stack.pop()));
		return PROCESS_ABORT;
	}

	public int visit(IASTExpressionStatement statement) {
		IASTExpression _expression = statement.getExpression();
		_expression.accept(this);
		stack.push(builder.Statement_expressionStatement((IConstructor) stack.pop()));
		return PROCESS_ABORT;
	}

	public int visit(IASTIfStatement statement) {
		IASTExpression _condition = statement.getConditionExpression();
		IASTStatement _thenClause = statement.getThenClause();
		IASTStatement _elseClause = statement.getElseClause();

		_condition.accept(this);
		IConstructor condition = (IConstructor) stack.pop();
		_thenClause.accept(this);
		IConstructor thenClause = (IConstructor) stack.pop();

		if (_elseClause == null) {
			stack.push(builder.Statement_if(condition, thenClause));
		} else {
			_elseClause.accept(this);
			IConstructor elseClause = (IConstructor) stack.pop();
			stack.push(builder.Statement_if(condition, thenClause, elseClause));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTTypeId typeId) {
		ctx.getStdErr().println("TypeId: " + typeId.getRawSignature());
		stack.push(builder.Expression_typeid(builder.Type_char()));// TODO
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTEnumerator enumerator) {
		ctx.getStdErr().println("Enumerator: " + enumerator.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTProblem problem) {
		ctx.getStdErr().println("Problem: " + problem.getMessage());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTBaseSpecifier baseSpecifier) {
		ctx.getStdErr().println("BaseSpecifier: " + baseSpecifier.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTNamespaceDefinition namespaceDefinition) {
		ctx.getStdErr().println("NamespaceDefinition: " + namespaceDefinition.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTTemplateParameter templateParameter) {
		ctx.getStdErr().println("TemplateParameter: " + templateParameter.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTCapture capture) {
		ctx.getStdErr().println("Capture: " + capture.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICASTDesignator designator) {
		ctx.getStdErr().println("Designator: " + designator.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTDesignator designator) {
		ctx.getStdErr().println("DesignatorCPP: " + designator.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTVirtSpecifier virtSpecifier) {
		ctx.getStdErr().println("VirtSpecifier: " + virtSpecifier.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTClassVirtSpecifier classVirtSpecifier) {
		ctx.getStdErr().println("ClassVirtSpecifier: " + classVirtSpecifier.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTDecltypeSpecifier decltypeSpecifier) {
		ctx.getStdErr().println("DecltypeSpecifier: " + decltypeSpecifier.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ASTAmbiguousNode astAmbiguousNode) {
		ctx.getStdErr().println("AstAmbiguousNode: " + astAmbiguousNode.getRawSignature());
		return PROCESS_ABORT;
	}
}
