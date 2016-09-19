package lang.cpp.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.eclipse.cdt.core.dom.ast.ASTVisitor;
import org.eclipse.cdt.core.dom.ast.IASTArrayModifier;
import org.eclipse.cdt.core.dom.ast.IASTAttribute;
import org.eclipse.cdt.core.dom.ast.IASTAttributeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression;
import org.eclipse.cdt.core.dom.ast.IASTCompoundStatement;
import org.eclipse.cdt.core.dom.ast.IASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTDeclarationStatement;
import org.eclipse.cdt.core.dom.ast.IASTDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier.IASTEnumerator;
import org.eclipse.cdt.core.dom.ast.IASTExpression;
import org.eclipse.cdt.core.dom.ast.IASTExpressionStatement;
import org.eclipse.cdt.core.dom.ast.IASTForStatement;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDefinition;
import org.eclipse.cdt.core.dom.ast.IASTIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTIfStatement;
import org.eclipse.cdt.core.dom.ast.IASTInitializer;
import org.eclipse.cdt.core.dom.ast.IASTLiteralExpression;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTParameterDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTPointerOperator;
import org.eclipse.cdt.core.dom.ast.IASTProblem;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTStatement;
import org.eclipse.cdt.core.dom.ast.IASTToken;
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.IASTTypeId;
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCapture;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTClassVirtSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCompositeTypeSpecifier.ICPPASTBaseSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDecltypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDesignator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTNamespaceDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVirtSpecifier;
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.value.IConstructor;
import org.rascalmpl.value.IString;
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
		List<IValue> declarations = new ArrayList<IValue>();
		for (IASTDeclaration node : tu.getDeclarations()) {
			node.accept(this);
			declarations.add(stack.pop());
		}

		stack.push(builder.Declaration_translationUnit(vf.list(declarations.toArray(new IValue[declarations.size()]))));

		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTName name) {
		ctx.getStdErr().println("Name: " + name.getLastName().getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTDeclaration declaration) {
		if (declaration instanceof IASTFunctionDefinition) {
			visit((IASTFunctionDefinition) declaration);
		} else if (declaration instanceof IASTSimpleDeclaration) {
			visit((IASTSimpleDeclaration) declaration);
		} else {
			ctx.getStdErr()
					.println("Declaration: encountered non-implemented subtype " + declaration.getClass().getName());
			stack.push(builder.Declaration_class(null));
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTSimpleDeclaration declaration) {
		IASTDeclSpecifier _declSpecifier = declaration.getDeclSpecifier();
		_declSpecifier.accept(this);
		IString declSpecifier = (IString) stack.pop();
		IASTDeclarator[] _declarators = declaration.getDeclarators();
		List<IValue> declarators = new ArrayList<IValue>();
		for (IASTDeclarator declarator : _declarators) {
			declarator.accept(this);
			declarators.add(stack.pop());
		}
		stack.push(builder.Declaration_simpleDeclaration(declSpecifier.getValue(),
				vf.list(declarators.toArray(new IValue[declarators.size()]))));
		return PROCESS_ABORT;
	}

	public int visit(IASTFunctionDefinition definition) {
		IASTDeclSpecifier _declSpecifier = definition.getDeclSpecifier();
		IASTFunctionDeclarator _declarator = definition.getDeclarator();
		IASTStatement _body = definition.getBody();

		_declSpecifier.accept(this);
		IString declSpecifier = (IString) stack.pop();
		_declarator.accept(this);
		IString declarator = (IString) stack.pop();
		_body.accept(this);
		IConstructor body = (IConstructor) stack.pop();

		stack.push(builder.Declaration_functionDefinition(declSpecifier.getValue(), declarator.getValue(), body));

		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTInitializer initializer) {
		ctx.getStdErr().println("Initializer: " + initializer.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTParameterDeclaration parameterDeclaration) {
		ctx.getStdErr().println("ParameterDeclaration: " + parameterDeclaration.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTDeclarator declarator) {
		// ctx.getStdErr().println("Declarator: " +
		// declarator.getRawSignature());
		stack.push(vf.string("TODO:" + declarator.getRawSignature()));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTDeclSpecifier declSpec) {
		// ctx.getStdErr().println("DeclSpecifier: " +
		// declSpec.getRawSignature());
		stack.push(vf.string("TODO:" + declSpec.getRawSignature()));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTArrayModifier arrayModifier) {
		ctx.getStdErr().println("ArrayModifier: " + arrayModifier.getRawSignature());
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTPointerOperator ptrOperator) {
		ctx.getStdErr().println("PtrOperator: " + ptrOperator.getRawSignature());
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
		else if (expression instanceof IASTIdExpression)
			visit((IASTIdExpression) expression);
		else if (expression instanceof IASTLiteralExpression)
			visit((IASTLiteralExpression) expression);
		else if (expression instanceof IASTUnaryExpression)
			visit((IASTUnaryExpression) expression);
		else {
			ctx.getStdErr()
					.println("Expression: encountered non-implemented subtype " + expression.getClass().getName());
			stack.push(vf.string("TODO:" + expression.getRawSignature()));
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTUnaryExpression expression) {
		int operator = expression.getOperator();
		IASTExpression _operand = expression.getOperand();
		_operand.accept(this);
		IConstructor $expression = (IConstructor) stack.pop();

		switch (operator) {
		case 0:
			stack.push(builder.Expression_prefixIncr($expression));
			break;
		case 1:
			stack.push(builder.Expression_prefixDecr($expression));
			break;
		case 2:
			stack.push(builder.Expression_plus($expression));
			break;
		case 3:
			stack.push(builder.Expression_minus($expression));
			break;
		case 4:
			stack.push(builder.Expression_star($expression));
			break;
		case 5:
			stack.push(builder.Expression_amper($expression));
			break;
		case 6:
			stack.push(builder.Expression_tilde($expression));
			break;
		case 7:
			stack.push(builder.Expression_not($expression));
			break;
		case 8:
			stack.push(builder.Expression_sizeof($expression));
			break;
		case 9:
			stack.push(builder.Expression_postfixIncr($expression));
			break;
		case 10:
			stack.push(builder.Expression_postfixDecr($expression));
			break;
		case 11:
			stack.push(builder.Expression_bracketed($expression));
			break;
		case 12:
			stack.push(builder.Expression_throw($expression));
			break;
		case 13:
			stack.push(builder.Expression_typeid($expression));
			break;
		// case 14: typeId is deprecated
		case 15:
			stack.push(builder.Expression_alignOf($expression));
			break;
		case 16:
			stack.push(builder.Expression_sizeofParameterPack($expression));
			break;
		case 17:
			stack.push(builder.Expression_noexcept($expression));
			break;
		case 18:
			stack.push(builder.Expression_labelReference($expression));
			break;
		default:
			throw new RuntimeException("Unknown unary operator " + operator + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTLiteralExpression expression) {
		ctx.getStdOut()
				.println("LiteralExpression: " + expression.getRawSignature() + ", " + expression.getExpressionType());
		stack.push(builder.Expression_integerLiteral(vf.integer(42)));
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
		case 1:
			stack.push(builder.Expression_multiply(lhs, rhs));
			break;
		case 2:
			stack.push(builder.Expression_divide(lhs, rhs));
			break;
		case 3:
			stack.push(builder.Expression_modulo(lhs, rhs));
			break;
		case 4:
			stack.push(builder.Expression_plus(lhs, rhs));
			break;
		case 5:
			stack.push(builder.Expression_minus(lhs, rhs));
			break;
		case 6:
			stack.push(builder.Expression_shiftLeft(lhs, rhs));
			break;
		case 7:
			stack.push(builder.Expression_shiftRight(lhs, rhs));
			break;
		case 8:
			stack.push(builder.Expression_lessThan(lhs, rhs));
			break;
		case 9:
			stack.push(builder.Expression_greaterThan(lhs, rhs));
			break;
		case 10:
			stack.push(builder.Expression_lessEqual(lhs, rhs));
			break;
		case 11:
			stack.push(builder.Expression_greaterEqual(lhs, rhs));
			break;
		case 12:
			stack.push(builder.Expression_binaryAnd(lhs, rhs));
			break;
		case 13:
			stack.push(builder.Expression_binaryXor(lhs, rhs));
			break;
		case 14:
			stack.push(builder.Expression_binaryOr(lhs, rhs));
			break;
		case 15:
			stack.push(builder.Expression_logicalAnd(lhs, rhs));
			break;
		case 16:
			stack.push(builder.Expression_logicalOr(lhs, rhs));
			break;
		case 17:
			stack.push(builder.Expression_assign(lhs, rhs));
			break;
		case 18:
			stack.push(builder.Expression_multiplyAssign(lhs, rhs));
			break;
		case 19:
			stack.push(builder.Expression_divideAssign(lhs, rhs));
			break;
		case 20:
			stack.push(builder.Expression_moduloAssign(lhs, rhs));
			break;
		case 21:
			stack.push(builder.Expression_plusAssign(lhs, rhs));
			break;
		case 22:
			stack.push(builder.Expression_minusAssign(lhs, rhs));
			break;
		case 23:
			stack.push(builder.Expression_shiftLeftAssign(lhs, rhs));
			break;
		case 24:
			stack.push(builder.Expression_shiftRightAssign(lhs, rhs));
			break;
		case 25:
			stack.push(builder.Expression_binaryAndAssign(lhs, rhs));
			break;
		case 26:
			stack.push(builder.Expression_binaryXorAssign(lhs, rhs));
			break;
		case 27:
			stack.push(builder.Expression_binaryOrAssign(lhs, rhs));
			break;
		case 28:
			stack.push(builder.Expression_equals(lhs, rhs));
			break;
		case 29:
			stack.push(builder.Expression_notEquals(lhs, rhs));
			break;
		case 30:
			stack.push(builder.Expression_pmDot(lhs, rhs));
			break;
		case 31:
			stack.push(builder.Expression_pmArrow(lhs, rhs));
			break;
		case 32:
			stack.push(builder.Expression_max(lhs, rhs));
			break;
		case 33:
			stack.push(builder.Expression_min(lhs, rhs));
			break;
		case 34:
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
		if (statement instanceof IASTCompoundStatement) {
			visit((IASTCompoundStatement) statement);
		} else if (statement instanceof IASTDeclarationStatement) {
			visit((IASTDeclarationStatement) statement);
		} else if (statement instanceof IASTExpressionStatement) {
			visit((IASTExpressionStatement) statement);
		} else if (statement instanceof IASTIfStatement) {
			visit((IASTIfStatement) statement);
		} else if (statement instanceof IASTForStatement) {
			visit((IASTForStatement) statement);
		} else {
			ctx.getStdErr().println("Statement: encountered non-implemented subtype " + statement.getClass().getName());
			stack.push(vf.bool(false));
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTForStatement forStatement) {
		IASTStatement _initializer = forStatement.getInitializerStatement();
		IASTExpression _condition = forStatement.getConditionExpression();
		IASTExpression _iteration = forStatement.getIterationExpression();
		IASTStatement _body = forStatement.getBody();

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
		List<IValue> statements = new ArrayList<IValue>();
		for (IASTStatement statement : _statements) {
			statement.accept(this);
			statements.add((IConstructor) stack.pop());
		}
		stack.push(builder.Statement_compoundStatement(vf.list(statements.toArray(new IValue[statements.size()]))));
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
