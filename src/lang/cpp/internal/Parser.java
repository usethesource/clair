package lang.cpp.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import org.eclipse.cdt.core.dom.ast.ASTVisitor;
import org.eclipse.cdt.core.dom.ast.IASTArrayModifier;
import org.eclipse.cdt.core.dom.ast.IASTAttribute;
import org.eclipse.cdt.core.dom.ast.IASTAttributeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier.IASTEnumerator;
import org.eclipse.cdt.core.dom.ast.IASTExpression;
import org.eclipse.cdt.core.dom.ast.IASTInitializer;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.eclipse.cdt.core.dom.ast.IASTParameterDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTPointerOperator;
import org.eclipse.cdt.core.dom.ast.IASTProblem;
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
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.index.IIndex;
import org.eclipse.cdt.core.model.ILanguage;
import org.eclipse.cdt.core.parser.DefaultLogService;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.IParserLogService;
import org.eclipse.cdt.core.parser.IScannerInfo;
import org.eclipse.cdt.core.parser.IncludeFileContentProvider;
import org.eclipse.cdt.core.parser.ScannerInfo;
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode;
import org.eclipse.core.runtime.CoreException;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.utils.RuntimeExceptionFactory;
import org.rascalmpl.library.Prelude;
import org.rascalmpl.value.ISourceLocation;
import org.rascalmpl.value.IString;
import org.rascalmpl.value.IValue;
import org.rascalmpl.value.IValueFactory;

public class Parser {
	private IValueFactory vf;
	private AST builder;
	private IEvaluatorContext ctx;

	public Parser(IValueFactory vf) {
		this.vf = vf;
		this.builder = new AST(vf);
	}

	public IValue parseCpp(ISourceLocation file, IEvaluatorContext ctx) {
		if (ctx != null)
			this.ctx = ctx;
		try {
			String input = ((IString) new Prelude(vf).readFile(file)).getValue();
			IValue result = parse(file.getPath(), input.toCharArray());

			if (result == null) {
				throw RuntimeExceptionFactory.parseError(file, null, null);
			}

			return result;
		} catch (CoreException e) {
			throw RuntimeExceptionFactory.io(vf.string(e.getMessage()), null, null);
		}
	}

	private IValue parse(String path, char[] code) throws CoreException {
		FileContent fc = FileContent.create(path, code);
		Map<String, String> macroDefinitions = new HashMap<String, String>();
		String[] includeSearchPaths = new String[0];
		IScannerInfo si = new ScannerInfo(macroDefinitions, includeSearchPaths);
		IncludeFileContentProvider ifcp = IncludeFileContentProvider.getEmptyFilesProvider();
		IIndex idx = null;
		int options = ILanguage.OPTION_IS_SOURCE_UNIT;
		IParserLogService log = new DefaultLogService();
		IASTTranslationUnit tu = GPPLanguage.getDefault().getASTTranslationUnit(fc, si, ifcp, idx, options, log);

		return convertCdtToRascal(tu);
	}

	public synchronized IValue convertCdtToRascal(IASTTranslationUnit translationUnit) throws CoreException {
		Stack<IValue> stack = new Stack<IValue>();

		ASTVisitor visitor = new ASTVisitor(true) {

			@Override
			public int visit(IASTTranslationUnit tu) {
				ctx.getStdErr().println("TranslationUnit: " + tu.getRawSignature());
				ctx.getStdErr().println(tu.getChildren().length + " children");
				int stackSizeBefore = stack.size();
				for (IASTNode node : tu.getChildren())
					node.accept(this);

				List<IValue> children = new ArrayList<IValue>();
				for (int i = 0; i < tu.getChildren().length; i++)
					children.add(stack.pop());
				Collections.reverse(children);

				if (stackSizeBefore != stack.size())
					throw new RuntimeException("Illegal stack modification detected: had " + stackSizeBefore
							+ ", now have " + stack.size());
				stack.push(builder.Declaration_translationUnit(vf.list(children.toArray(new IValue[children.size()]))));

				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTName name) {
				ctx.getStdErr().println("Name: " + name.getLastName().getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTDeclaration declaration) {
				ctx.getStdErr().println("Declaration: " + declaration.getRawSignature());

				List<IASTNode> nodes = Arrays.asList(declaration.getChildren());
				ctx.getStdErr().println("Declaration has " + nodes.size() + " children");

				IASTNode _declSpecifier = nodes.get(0);
				_declSpecifier.accept(this);
				IValue declSpecifier = (IString) stack.pop();
				IASTNode _declarator = nodes.get(1);
				_declarator.accept(this);
				IValue declarator = stack.pop();
				IASTNode _statement = nodes.get(2);
				_statement.accept(this);
				IValue statement = stack.pop();

				stack.push(builder.Declaration_declaration(declSpecifier.toString(), declarator.toString(),
						vf.list(statement)));
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTInitializer initializer) {
				ctx.getStdErr().println("Initializer: " + initializer.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTParameterDeclaration parameterDeclaration) {
				ctx.getStdErr().println("ParameterDeclaration: " + parameterDeclaration.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTDeclarator declarator) {
				ctx.getStdErr().println("Declarator: " + declarator.getRawSignature());
				stack.push(vf.string(declarator.getRawSignature()));
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTDeclSpecifier declSpec) {
				ctx.getStdErr().println("DeclSpecifier: " + declSpec.getRawSignature());
				stack.push(vf.string(declSpec.getRawSignature()));
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTArrayModifier arrayModifier) {
				ctx.getStdErr().println("ArrayModifier: " + arrayModifier.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTPointerOperator ptrOperator) {
				ctx.getStdErr().println("PtrOperator: " + ptrOperator.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTAttribute attribute) {
				ctx.getStdErr().println("Attribute: " + attribute.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTAttributeSpecifier specifier) {
				ctx.getStdErr().println("Specifier: " + specifier.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTToken token) {
				ctx.getStdErr().println("Token: " + new String(token.getTokenCharImage()));
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTExpression expression) {
				ctx.getStdErr().println("Expression: " + expression.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTStatement statement) {
				ctx.getStdErr().println("Statement: " + statement.getRawSignature());
				stack.push(vf.string("FOO"));
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTTypeId typeId) {
				ctx.getStdErr().println("TypeId: " + typeId.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTEnumerator enumerator) {
				ctx.getStdErr().println("Enumerator: " + enumerator.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(IASTProblem problem) {
				ctx.getStdErr().println("Problem: " + problem.getMessage());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ICPPASTBaseSpecifier baseSpecifier) {
				ctx.getStdErr().println("BaseSpecifier: " + baseSpecifier.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ICPPASTNamespaceDefinition namespaceDefinition) {
				ctx.getStdErr().println("NamespaceDefinition: " + namespaceDefinition.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ICPPASTTemplateParameter templateParameter) {
				ctx.getStdErr().println("TemplateParameter: " + templateParameter.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ICPPASTCapture capture) {
				ctx.getStdErr().println("Capture: " + capture.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ICASTDesignator designator) {
				ctx.getStdErr().println("Designator: " + designator.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ICPPASTDesignator designator) {
				ctx.getStdErr().println("DesignatorCPP: " + designator.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ICPPASTVirtSpecifier virtSpecifier) {
				ctx.getStdErr().println("VirtSpecifier: " + virtSpecifier.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ICPPASTClassVirtSpecifier classVirtSpecifier) {
				ctx.getStdErr().println("ClassVirtSpecifier: " + classVirtSpecifier.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ICPPASTDecltypeSpecifier decltypeSpecifier) {
				ctx.getStdErr().println("DecltypeSpecifier: " + decltypeSpecifier.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

			@Override
			public int visit(ASTAmbiguousNode astAmbiguousNode) {
				ctx.getStdErr().println("AstAmbiguousNode: " + astAmbiguousNode.getRawSignature());
				return ASTVisitor.PROCESS_ABORT;
			}

		};
		translationUnit.accept(visitor);
		return stack.pop();
	}
}
