package lang.cpp.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
import org.eclipse.cdt.core.dom.ast.IASTFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDefinition;
import org.eclipse.cdt.core.dom.ast.IASTIfStatement;
import org.eclipse.cdt.core.dom.ast.IASTInitializer;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.eclipse.cdt.core.dom.ast.IASTParameterDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTPointerOperator;
import org.eclipse.cdt.core.dom.ast.IASTProblem;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTStatement;
import org.eclipse.cdt.core.dom.ast.IASTToken;
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.IASTTypeId;
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
		ctx.getStdErr().println("TranslationUnit: " + tu.getRawSignature());
		ctx.getStdErr().println(tu.getLinkage().getLinkageName());

		ctx.getStdErr().println(tu.getDeclarations().length + " declarations");
		List<IValue> declarations = new ArrayList<IValue>();
		for (IASTDeclaration node : tu.getDeclarations()) {
			node.accept(this);
			declarations.add(stack.pop());
		}
		Collections.reverse(declarations);

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
		ctx.getStdErr().println("Declaration: " + declaration.getRawSignature());
		ctx.getStdErr().println(declaration.getClass().getName());

		List<IASTNode> nodes = Arrays.asList(declaration.getChildren());
		ctx.getStdErr().println("Declaration has " + nodes.size() + " children");

		if (declaration instanceof IASTFunctionDefinition) {
			visit((IASTFunctionDefinition) declaration);
		} else if (declaration instanceof IASTSimpleDeclaration) {
			visit((IASTSimpleDeclaration) declaration);
		} else
			stack.push(builder.Declaration_class(null));

		// IASTNode _declSpecifier = nodes.get(0);
		// _declSpecifier.accept(this);
		// IValue declSpecifier = (IString) stack.pop();
		// IASTNode _declarator = nodes.get(1);
		// _declarator.accept(this);
		// IValue declarator = stack.pop();
		// IASTNode _statement = nodes.get(2);
		// _statement.accept(this);
		// IValue statement = stack.pop();

		// stack.push(builder.Declaration_declaration(declSpecifier.toString(),
		// declarator.toString(), null));
		return PROCESS_ABORT;
	}

	public int visit(IASTSimpleDeclaration declaration) {
		ctx.getStdOut().println("*SimpleDeclaration*");
		IASTDeclSpecifier _declSpecifier = declaration.getDeclSpecifier();
		_declSpecifier.accept(this);
		IString declSpecifier = (IString) stack.pop();
		IASTDeclarator[] _declarators = declaration.getDeclarators();
		List<IValue> declarators = new ArrayList<IValue>();
		for (IASTDeclarator declarator : _declarators) {
			declarator.accept(this);
			declarators.add(stack.pop());
		}
		Collections.reverse(declarators);
		stack.push(builder.Declaration_simpleDeclaration(declSpecifier.getValue(),
				vf.list(declarators.toArray(new IValue[declarators.size()]))));
		return PROCESS_ABORT;
	}

	public int visit(IASTFunctionDefinition definition) {
		ctx.getStdErr().println("Functiondefinition" + definition.getRawSignature());
		IASTDeclSpecifier _declSpecifier = definition.getDeclSpecifier();
		IASTFunctionDeclarator _declarator = definition.getDeclarator();
		IASTStatement _body = definition.getBody();
		ctx.getStdOut().println("DeclSpecifier: [" + _body.getRawSignature() + "], " + _body.getClass().getName());

		_declSpecifier.accept(this);
		IString declSpecifier = (IString) stack.pop();
		_declarator.accept(this);
		IString declarator = (IString) stack.pop();
		_body.accept(this);
		IConstructor body = (IConstructor) stack.pop();

		stack.push(builder.Declaration_functionDefinition(declSpecifier.getValue(), declarator.getValue(), body));
		ctx.getStdOut().println("Finished functiondefinition");

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
		ctx.getStdErr().println("Declarator: " + declarator.getRawSignature());
		stack.push(vf.string("TODO:" + declarator.getRawSignature()));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTDeclSpecifier declSpec) {
		ctx.getStdErr().println("DeclSpecifier: " + declSpec.getRawSignature());
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
		ctx.getStdErr().println("Expression: " + expression.getRawSignature());
		return PROCESS_ABORT;
	}

	public int visit(IASTBinaryExpression expression) {
		ctx.getStdErr().println("BinaryExpression: " + expression.getRawSignature());
		IASTExpression _lhs = expression.getOperand1();
		ctx.getStdOut().println("Operand1: " + _lhs.getClass().getName());
		_lhs.accept(this);
		IConstructor lhs = (IConstructor) stack.pop();
		int op = expression.getOperator();
		IASTExpression _rhs = expression.getOperand2();
		ctx.getStdOut().println("Operand2: " + _lhs.getClass().getName());
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
		ctx.getStdErr().println("Statement: " + statement.getRawSignature());
		ctx.getStdErr().println(statement.getClass().getName());
		ctx.getStdErr().println(statement.getClass().getSimpleName());
		if (statement instanceof IASTCompoundStatement) {
			visit((IASTCompoundStatement) statement);
		} else if (statement instanceof IASTDeclarationStatement) {
			visit((IASTDeclarationStatement) statement);
		} else if (statement instanceof IASTIfStatement) {
			visit((IASTIfStatement) statement);
		} else
			stack.push(vf.bool(false));
		return PROCESS_ABORT;
	}

	public int visit(IASTCompoundStatement compoundStatement) {
		ctx.getStdOut().println("Compound statement with " + compoundStatement.getChildren().length + " children");
		IASTStatement[] _statements = compoundStatement.getStatements();
		List<IValue> statements = new ArrayList<IValue>();
		for (IASTStatement statement : _statements) {
			ctx.getStdOut().println("Child: " + statement.getRawSignature());
			statement.accept(this);
			statements.add(stack.pop());
		}
		Collections.reverse(statements);
		ctx.getStdOut()
				.println("FOOOOO " + vf.list(statements.toArray(new IValue[statements.size()])).getClass().getName());
		stack.push(builder.Statement_compoundStatement(vf.list(statements.toArray(new IValue[statements.size()]))));
		return PROCESS_ABORT;
	}

	public int visit(IASTDeclarationStatement statement) {
		ctx.getStdOut().println("DeclarationStatement: " + statement.getRawSignature());
		IASTDeclaration _declaration = statement.getDeclaration();
		_declaration.accept(this);
		IConstructor declaration = (IConstructor) stack.pop();
		stack.push(builder.Statement_declarationStatement(declaration));
		return PROCESS_ABORT;
	}

	public int visit(IASTIfStatement ifStatement) {
		// TODO
		stack.push(
				builder.Statement_if(builder.Expression_null(), builder.Statement_break(), builder.Statement_break()));
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
