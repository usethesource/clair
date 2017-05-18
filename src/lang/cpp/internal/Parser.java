package lang.cpp.internal;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;
import org.eclipse.cdt.core.dom.ast.ASTVisitor;
import org.eclipse.cdt.core.dom.ast.IASTASMDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTArrayDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTArrayModifier;
import org.eclipse.cdt.core.dom.ast.IASTArraySubscriptExpression;
import org.eclipse.cdt.core.dom.ast.IASTAttribute;
import org.eclipse.cdt.core.dom.ast.IASTAttributeList;
import org.eclipse.cdt.core.dom.ast.IASTAttributeOwner;
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
import org.eclipse.cdt.core.dom.ast.IASTFileLocation;
import org.eclipse.cdt.core.dom.ast.IASTForStatement;
import org.eclipse.cdt.core.dom.ast.IASTFunctionCallExpression;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDefinition;
import org.eclipse.cdt.core.dom.ast.IASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.IASTIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTIfExistsStatement;
import org.eclipse.cdt.core.dom.ast.IASTIfNotExistsStatement;
import org.eclipse.cdt.core.dom.ast.IASTIfStatement;
import org.eclipse.cdt.core.dom.ast.IASTImplicitName;
import org.eclipse.cdt.core.dom.ast.IASTInitializer;
import org.eclipse.cdt.core.dom.ast.IASTInitializerClause;
import org.eclipse.cdt.core.dom.ast.IASTInitializerList;
import org.eclipse.cdt.core.dom.ast.IASTLabelStatement;
import org.eclipse.cdt.core.dom.ast.IASTLiteralExpression;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNamedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.eclipse.cdt.core.dom.ast.IASTNullStatement;
import org.eclipse.cdt.core.dom.ast.IASTParameterDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTPointer;
import org.eclipse.cdt.core.dom.ast.IASTPointerOperator;
import org.eclipse.cdt.core.dom.ast.IASTProblem;
import org.eclipse.cdt.core.dom.ast.IASTProblemDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTProblemExpression;
import org.eclipse.cdt.core.dom.ast.IASTProblemStatement;
import org.eclipse.cdt.core.dom.ast.IASTProblemTypeId;
import org.eclipse.cdt.core.dom.ast.IASTReturnStatement;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTStandardFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTStatement;
import org.eclipse.cdt.core.dom.ast.IASTSwitchStatement;
import org.eclipse.cdt.core.dom.ast.IASTToken;
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.IASTTryExceptStatement;
import org.eclipse.cdt.core.dom.ast.IASTTryFinallyStatement;
import org.eclipse.cdt.core.dom.ast.IASTTypeId;
import org.eclipse.cdt.core.dom.ast.IASTTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTTypeIdInitializerExpression;
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression;
import org.eclipse.cdt.core.dom.ast.IASTWhileStatement;
import org.eclipse.cdt.core.dom.ast.IType;
import org.eclipse.cdt.core.dom.ast.c.ICASTArrayModifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignatedInitializer;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignator;
import org.eclipse.cdt.core.dom.ast.c.ICASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTAliasDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTArrayDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTArrayDesignator;
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
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFieldDesignator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFieldReference;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTForStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionCallExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionDefinition;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTFunctionWithTryBlock;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTIfStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTInitializerClause;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLambdaExpression.CaptureDefault;
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
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTPointerToMember;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTQualifiedName;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTRangeBasedForStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTReferenceOperator;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTSimpleDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTSimpleTypeConstructorExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTSimpleTypeTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTStaticAssertDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTSwitchStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateId;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplateSpecialization;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTemplatedTypeTemplateParameter;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTryBlockStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTypeId;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTypeTransformationSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUnaryExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDirective;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVirtSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVirtSpecifier.SpecifierKind;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVisibilityLabel;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTWhileStatement;
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.IGPPASTArrayRangeDesignator;
import org.eclipse.cdt.core.dom.parser.cpp.GPPScannerExtensionConfiguration;
import org.eclipse.cdt.core.index.IIndex;
import org.eclipse.cdt.core.index.IIndexFileLocation;
import org.eclipse.cdt.core.model.ILanguage;
import org.eclipse.cdt.core.parser.DefaultLogService;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.IParserLogService;
import org.eclipse.cdt.core.parser.IScanner;
import org.eclipse.cdt.core.parser.IScannerInfo;
import org.eclipse.cdt.core.parser.IncludeFileContentProvider;
import org.eclipse.cdt.core.parser.ParserLanguage;
import org.eclipse.cdt.core.parser.ScannerInfo;
import org.eclipse.cdt.internal.core.dom.IIncludeFileResolutionHeuristics;
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode;
import org.eclipse.cdt.internal.core.dom.parser.IASTAmbiguousStatement;
import org.eclipse.cdt.internal.core.index.CIndex;
import org.eclipse.cdt.internal.core.index.IIndexFragment;
import org.eclipse.cdt.internal.core.parser.IMacroDictionary;
import org.eclipse.cdt.internal.core.parser.scanner.CPreprocessor;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContent;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContentProvider;
import org.eclipse.core.runtime.CoreException;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.utils.RuntimeExceptionFactory;
import org.rascalmpl.library.Prelude;
import org.rascalmpl.uri.URIUtil;

import io.usethesource.vallang.IConstructor;
import io.usethesource.vallang.IList;
import io.usethesource.vallang.IListWriter;
import io.usethesource.vallang.IMap;
import io.usethesource.vallang.IMapWriter;
import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IString;
import io.usethesource.vallang.IValue;
import io.usethesource.vallang.IValueFactory;

@SuppressWarnings("restriction")
public class Parser extends ASTVisitor {
	private IValueFactory vf;
	private AST builder;
	private IEvaluatorContext ctx;
	private Stack<IConstructor> stack = new Stack<IConstructor>();
	private BindingsResolver br = new BindingsResolver();
	private TypeResolver tr;

	boolean doTypeLogging = false;
	ISourceLocation sourceLoc;

	public Parser(IValueFactory vf) {
		super(true);
		this.shouldVisitAmbiguousNodes = true;
		this.shouldVisitImplicitNames = true;
		this.includeInactiveNodes = true;
		this.shouldVisitTokens = true;

		this.vf = vf;
		this.builder = new AST(vf);
		this.tr = new TypeResolver(builder, vf) {
			@Override
			public IConstructor resolveType(IType type, ISourceLocation src) {
				return builder.TypeSymbol_any();
			}
		};

	}

	public IMap parseFiles(IList files, IList includePath, IMap additionalMacros, IEvaluatorContext ctx) {
		setIEvaluatorContext(ctx);
		IMapWriter map = vf.mapWriter();
		out("Converting " + files.length() + " files");
		int c = 0;
		for (IValue _file : files) {
			if (c++ > 5) // FIXME
				continue;
			ISourceLocation file = (ISourceLocation) _file;
			out("Converting " + file.toString());
			IValue ast = new Parser(vf).parseCpp(file, includePath, additionalMacros, ctx);
			map.put(file, ast);
		}
		out("Done converting!");
		return map.done();
	}

	Map<String, Set<String>> dependencies = new HashMap<String, Set<String>>();

	private void addDependency(String from, String to) {
		if (!dependencies.containsKey(from))
			dependencies.put(from, new HashSet<String>());
		dependencies.get(from).add(to);
	}

	public IValue parseCpp(ISourceLocation file, IList includePath, IMap additionalMacros, IEvaluatorContext ctx) {
		try {
			setIEvaluatorContext(ctx);
			sourceLoc = file;
			FileContent fc = FileContent.create(file.getAuthority() + file.getPath(),
					((IString) new Prelude(vf).readFile(file)).getValue().toCharArray());

			Map<String, String> macros = new HashMap<String, String>();
			Iterator<Entry<IValue, IValue>> it = additionalMacros.entryIterator();
			while (it.hasNext()) {
				Entry<IValue, IValue> entry = (Entry<IValue, IValue>) it.next();
				macros.put(entry.getKey().toString().replace("\"", ""), entry.getValue().toString().replace("\"", ""));
			}

			// from WinDiscoveredPathInfo.java:
			macros.put("_M_IX86", "600");
			macros.put("_WIN32", "1");
			// macros.put("_MSC_VER", "1400");
			macros.put("__cdecl", "");
			macros.put("__fastcall", "");
			macros.put("__restrict", "");
			macros.put("__sptr", "");
			macros.put("__stdcall", "");
			macros.put("__unaligned", "");
			macros.put("__uptr", "");
			macros.put("__w64", "");
			macros.put("__forceinline", "__inline");
			macros.put("__int8", "char");
			macros.put("__int16", "short");
			macros.put("__int32", "int");
			macros.put("__int64", "long long");

			// additional:
			macros.put("_MSC_VER", "1700");
			macros.put("__cplusplus", "");
			macros.put("__thiscall", "");
			macros.put("_CHAR16T", "");
			macros.put("_NATIVE_WCHAR_T_DEFINED", "1");
			macros.put("__nullptr", "nullptr");
			macros.put("_MSC_EXTENSIONS", "1");
			macros.put("__inline", "inline");
			macros.put("__ptr32", "");
			macros.put("__ptr64", "");
			macros.put("__interface", "struct");

			macros.put("__pragma(A)", "");
			macros.put("__identifier(A)", "A");
			macros.put("__declspec(A)", "");

			IScannerInfo si = new ScannerInfo(macros, null);

			InternalFileContentProvider ifcp = new InternalFileContentProvider() {
				@Override
				public InternalFileContent getContentForInclusion(String filePath, IMacroDictionary macroDictionary) {
					return (InternalFileContent) FileContent.createForExternalFileLocation(filePath);
				}

				@Override
				public InternalFileContent getContentForInclusion(IIndexFileLocation ifl, String astPath) {
					return (InternalFileContent) FileContent.create(ifl);
				}

			};

			IIncludeFileResolutionHeuristics ifrh = new IIncludeFileResolutionHeuristics() {
				List<String> path = new ArrayList<String>();
				{
					for (IValue include : includePath)
						path.add(locToPath((ISourceLocation) include));
				}

				private String locToPath(ISourceLocation loc) {
					if (!loc.getScheme().equals("file"))
						throw new IllegalArgumentException("Will not convert non-file loc");
					return loc.getAuthority() + loc.getPath();
				}

				@Override
				public String findInclusion(String include, String currentFile) {
					include = include.replace("\\", "/");
					String filePath = include.substring(0, include.lastIndexOf('/') + 1);
					String fileName = include.substring(include.lastIndexOf('/') + 1);
					for (String path : path) {
						File[] files = new File(path, filePath).listFiles();
						if (files == null)
							continue;
						for (File f : files)
							if (isRightFile(f.getName(), fileName)) {
								addDependency(currentFile, include);
								return f.getAbsolutePath();
							}

					}
					throw new RuntimeException("Include " + include + " for " + currentFile + " not found");
				}

				private boolean isRightFile(String include, String toMatch) {
					if (System.getProperty("os.name").contains("Win"))
						return include.equalsIgnoreCase(toMatch);
					return include.equals(toMatch);
				}
			};

			ifcp.setIncludeResolutionHeuristics(ifrh);
			IIndex idx = new CIndex(new IIndexFragment[] {});
			int options = ILanguage.OPTION_PARSE_INACTIVE_CODE;

			IParserLogService log = new IParserLogService() {

				@Override
				public void traceLog(String message) {
					// ctx.getStdErr().println(message);
				}

				@Override
				public boolean isTracing() {
					return true;
				}

			};

			IASTTranslationUnit tu = GPPLanguage.getDefault().getASTTranslationUnit(fc, si, ifcp, idx, options, log);
			IValue result = convertCdtToRascal(tu);
			if (result == null) {
				throw RuntimeExceptionFactory.parseError(file, null, null);
			}
			// out(dependencies.toString());

			scanner = new CPreprocessor(fc, si, ParserLanguage.CPP, log,
					GPPScannerExtensionConfiguration.getInstance(si), ifcp);

			return result;
		} catch (CoreException e) {
			throw RuntimeExceptionFactory.io(vf.string(e.getMessage()), null, null);
		}
	}

	IScanner scanner;

	public IValue parseExpression(IString expression, IEvaluatorContext ctx) throws CoreException, IOException {
		setIEvaluatorContext(ctx);
		this.sourceLoc = vf.sourceLocation(URIUtil.assumeCorrect("unknown://", "", ""));
		if (!expression.getValue().endsWith(";"))
			expression.concat(vf.string(";"));
		String expr = "void main() {\n\t" + expression.getValue() + "\n}";
		FileContent fc = FileContent.create("", expr.toCharArray());
		Map<String, String> macroDefinitions = new HashMap<String, String>();
		String[] includeSearchPaths = new String[0];
		IScannerInfo si = new ScannerInfo(macroDefinitions, includeSearchPaths);
		IncludeFileContentProvider ifcp = IncludeFileContentProvider.getEmptyFilesProvider();
		IIndex idx = null;
		int options = ILanguage.OPTION_PARSE_INACTIVE_CODE;
		IParserLogService log = new DefaultLogService();
		IASTTranslationUnit tu = GPPLanguage.getDefault().getASTTranslationUnit(fc, si, ifcp, idx, options, log);

		IASTFunctionDefinition main = (IASTFunctionDefinition) tu.getDeclarations()[0];
		IASTCompoundStatement body = (IASTCompoundStatement) main.getBody();
		IASTExpression ex = ((IASTExpressionStatement) body.getStatements()[0]).getExpression();
		ex.accept(this);
		return stack.pop();
	}

	public synchronized IValue convertCdtToRascal(IASTTranslationUnit translationUnit) throws CoreException {
		translationUnit.accept(this);
		if (stack.size() == 1)
			return stack.pop();
		if (stack.size() == 0)
			throw new RuntimeException("Stack empty after converting, error");
		IConstructor ast = stack.pop();
		err("Superfluous nodes on the stack after converting:");
		stack.iterator().forEachRemaining(it -> err(it.toString()));
		return ast;
	}

	public void setIEvaluatorContext(IEvaluatorContext ctx) {
		this.ctx = ctx;
		br.setIEvaluatorContext(ctx);
		tr.setIEvaluatorContext(ctx);
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

	public ISourceLocation getSourceLocation(IASTNode node) {
		IASTFileLocation astFileLocation = node.getFileLocation();
		if (astFileLocation != null) {
			return vf.sourceLocation(vf.sourceLocation(astFileLocation.getFileName()), astFileLocation.getNodeOffset(),
					astFileLocation.getNodeLength());
		} else
			return vf.sourceLocation(URIUtil.assumeCorrect("unknown:///", "", ""));
	}

	IList getAttributes(IASTAttributeOwner node) {
		IListWriter attributeSpecifiers = vf.listWriter();
		Stream.of(node.getAttributeSpecifiers()).forEach(it -> {
			it.accept(this);
			attributeSpecifiers.append(stack.pop());
		});
		return attributeSpecifiers.done();
	}

	IList getModifiers(IASTNode node) {
		ISourceLocation loc = getSourceLocation(node);
		IListWriter modifiers = vf.listWriter();

		if (node instanceof ICPPASTDeclSpecifier) {
			if (((ICPPASTDeclSpecifier) node).isFriend())
				modifiers.append(builder.Modifier_friend(loc));
			if (((ICPPASTDeclSpecifier) node).isVirtual())
				modifiers.append(builder.Modifier_virtual(loc));
			if (((ICPPASTDeclSpecifier) node).isExplicit())
				modifiers.append(builder.Modifier_explicit(loc));
			if (((ICPPASTDeclSpecifier) node).isConstexpr())
				modifiers.append(builder.Modifier_constexpr(loc));
			if (((ICPPASTDeclSpecifier) node).isThreadLocal())
				modifiers.append(builder.Modifier_threadLocal(loc));
		}

		if (node instanceof ICPPASTFunctionDeclarator) {
			if (((ICPPASTFunctionDeclarator) node).isMutable())
				modifiers.append(builder.Modifier_mutable(loc));
			if (((ICPPASTFunctionDeclarator) node).isPureVirtual())
				modifiers.append(builder.Modifier_pureVirtual(loc));
		}

		if (node instanceof IASTDeclSpecifier) {
			switch (((IASTDeclSpecifier) node).getStorageClass()) {
			case IASTDeclSpecifier.sc_typedef:
				modifiers.append(builder.Modifier_typedef(loc));
				break;
			case IASTDeclSpecifier.sc_extern:
				modifiers.append(builder.Modifier_extern(loc));
				break;
			case IASTDeclSpecifier.sc_static:
				modifiers.append(builder.Modifier_static(loc));
				break;
			case IASTDeclSpecifier.sc_auto:
				modifiers.append(builder.Modifier_auto(loc));
				break;
			case IASTDeclSpecifier.sc_register:
				modifiers.append(builder.Modifier_register(loc));
				break;
			case IASTDeclSpecifier.sc_mutable:
				modifiers.append(builder.Modifier_mutable(loc));
				break;
			}
		}

		if (node instanceof IASTSimpleDeclSpecifier) {
			if (((IASTSimpleDeclSpecifier) node).isSigned())
				modifiers.append(builder.Modifier_signed(loc));
			if (((IASTSimpleDeclSpecifier) node).isUnsigned())
				modifiers.append(builder.Modifier_unsigned(loc));
			if (((IASTSimpleDeclSpecifier) node).isShort())
				modifiers.append(builder.Modifier_short(loc));
			if (((IASTSimpleDeclSpecifier) node).isLong())
				modifiers.append(builder.Modifier_long(loc));
			if (((IASTSimpleDeclSpecifier) node).isLongLong())
				modifiers.append(builder.Modifier_longlong(loc));
			if (((IASTSimpleDeclSpecifier) node).isComplex())
				modifiers.append(builder.Modifier_complex(loc));
			if (((IASTSimpleDeclSpecifier) node).isImaginary())
				modifiers.append(builder.Modifier_imaginary(loc));
		}

		if (node instanceof ICASTArrayModifier) {
			if (((ICASTArrayModifier) node).isConst())
				modifiers.append(builder.Modifier_const(loc));
			if (((ICASTArrayModifier) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(loc));
			if (((ICASTArrayModifier) node).isRestrict())
				modifiers.append(builder.Modifier_restrict(loc));
		} else if (node instanceof ICPPASTFunctionDeclarator) {
			if (((ICPPASTFunctionDeclarator) node).isConst())
				modifiers.append(builder.Modifier_const(loc));
			if (((ICPPASTFunctionDeclarator) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(loc));
		} else if (node instanceof IASTDeclSpecifier) {
			if (((IASTDeclSpecifier) node).isConst())
				modifiers.append(builder.Modifier_const(loc));
			if (((IASTDeclSpecifier) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(loc));
			if (((IASTDeclSpecifier) node).isRestrict())
				modifiers.append(builder.Modifier_restrict(loc));
			if (((IASTDeclSpecifier) node).isInline())
				modifiers.append(builder.Modifier_inline(loc));
		} else if (node instanceof IASTPointer) {
			if (((IASTPointer) node).isConst())
				modifiers.append(builder.Modifier_const(loc));
			if (((IASTPointer) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(loc));
			if (((IASTPointer) node).isRestrict())
				modifiers.append(builder.Modifier_restrict(loc));
		} else if (node instanceof ICPPASTNamespaceDefinition) {
			if (((ICPPASTNamespaceDefinition) node).isInline())
				modifiers.append(builder.Modifier_inline(loc));
		}

		if (node instanceof ICPPASTNamedTypeSpecifier) {
			if (((ICPPASTNamedTypeSpecifier) node).isTypename())
				modifiers.append(builder.Modifier_typename(loc));
		} else if (node instanceof ICPPASTUsingDeclaration)
			if (((ICPPASTUsingDeclaration) node).isTypename())
				modifiers.append(builder.Modifier_typename(loc));

		return modifiers.done();
	}

	@Override
	public int visit(IASTTranslationUnit tu) {
		ISourceLocation loc = getSourceLocation(tu);
		IListWriter declarations = vf.listWriter();
		Stream.of(tu.getDeclarations()).forEach(it -> {
			it.accept(this);
			declarations.append(stack.pop());
		});

		IConstructor translationUnit = builder.Declaration_translationUnit(declarations.done(), loc);
		stack.push(translationUnit);

		return PROCESS_ABORT;
	}

	// Names

	@Override
	public int visit(IASTName name) {
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
		boolean alternate = name.isAlternate();
		boolean operator = name.isOperator();
		IASTName _lastName = name.getLastName();
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTName name) {
		ISourceLocation loc = getSourceLocation(name);
		if (name instanceof ICPPASTConversionName)
			visit((ICPPASTConversionName) name);
		else if (name instanceof ICPPASTOperatorName)
			visit((ICPPASTOperatorName) name);
		else if (name instanceof ICPPASTQualifiedName)
			visit((ICPPASTQualifiedName) name);
		else if (name instanceof ICPPASTTemplateId)
			visit((ICPPASTTemplateId) name);
		else {
			stack.push(builder.Expression_name(new String(name.toCharArray()), loc));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTConversionName name) {
		ISourceLocation loc = getSourceLocation(name);
		name.getTypeId().accept(this);
		stack.push(builder.Expression_conversionName(name.toString(), stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTOperatorName name) {
		ISourceLocation loc = getSourceLocation(name);
		stack.push(builder.Expression_operatorName(name.toString(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTQualifiedName name) {
		ISourceLocation loc = getSourceLocation(name);
		ISourceLocation decl = br.resolveBinding(name);
		ICPPASTNameSpecifier[] _qualifier = name.getQualifier();
		IASTName _lastName = name.getLastName();
		boolean fullyQualified = name.isFullyQualified();

		IListWriter qualifier = vf.listWriter();
		Stream.of(_qualifier).forEach(it -> {
			it.accept(this);
			qualifier.append(stack.pop());
		});
		_lastName.accept(this);
		IConstructor lastName = stack.pop();
		// TODO: check fullyQualified
		if (fullyQualified)
			;
		// err("WARNING: ICPPASTQualifiedName has fullyQualified=true");
		stack.push(builder.Expression_qualifiedName(qualifier.done(), lastName, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTemplateId name) {
		ISourceLocation loc = getSourceLocation(name);
		ISourceLocation decl = br.resolveBinding(name);
		IASTName _templateName = name.getTemplateName();
		IASTNode[] _templateArguments = name.getTemplateArguments();

		_templateName.accept(this);
		IConstructor templateName = stack.pop();
		IListWriter templateArguments = vf.listWriter();
		Stream.of(_templateArguments).forEach(it -> {
			it.accept(this);
			templateArguments.append(stack.pop());
		});
		stack.push(builder.Expression_templateId(templateName, templateArguments.done(), loc, decl));
		return PROCESS_ABORT;
	}

	// Declarations

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
		else if (declaration instanceof ICPPASTTemplateSpecialization)
			visit((ICPPASTTemplateSpecialization) declaration);
		else if (declaration instanceof ICPPASTTemplateDeclaration)
			visit((ICPPASTTemplateDeclaration) declaration);
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

	public int visit(ICPPASTAliasDeclaration declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		ISourceLocation decl = br.resolveBinding(declaration);
		IList attributes = getAttributes(declaration);
		IASTName _alias = declaration.getAlias();
		ICPPASTTypeId _mappingTypeId = declaration.getMappingTypeId();
		_alias.accept(this);
		IConstructor alias = stack.pop();
		_mappingTypeId.accept(this);
		IConstructor mappingTypeId = stack.pop();
		stack.push(builder.Declaration_alias(attributes, alias, mappingTypeId, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTExplicitTemplateInstantiation declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		IASTDeclaration _declaration = declaration.getDeclaration();
		_declaration.accept(this);
		switch (declaration.getModifier()) {
		case 0:
			stack.push(builder.Declaration_explicitTemplateInstantiation(stack.pop(), loc));
			break;
		case ICPPASTExplicitTemplateInstantiation.STATIC:
			stack.push(
					builder.Declaration_explicitTemplateInstantiation(builder.Modifier_static(loc), stack.pop(), loc));
			break;
		case ICPPASTExplicitTemplateInstantiation.INLINE:
			stack.push(
					builder.Declaration_explicitTemplateInstantiation(builder.Modifier_inline(loc), stack.pop(), loc));
			break;
		case ICPPASTExplicitTemplateInstantiation.EXTERN:
			stack.push(
					builder.Declaration_explicitTemplateInstantiation(builder.Modifier_extern(loc), stack.pop(), loc));
			break;
		default:
			throw new RuntimeException(
					"ICPPASTExplicitTemplateInstantiation encountered unknown modifier " + declaration.getModifier());
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTLinkageSpecification declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		String literal = declaration.getLiteral();
		IASTDeclaration[] _declarations = declaration.getDeclarations();

		IListWriter declarations = vf.listWriter();
		Stream.of(_declarations).forEach(it -> {
			it.accept(this);
			declarations.append(stack.pop());
		});

		stack.push(builder.Declaration_linkageSpecification(literal, declarations.done(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNamespaceAlias declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		ISourceLocation decl = br.resolveBinding(declaration);
		declaration.getAlias().accept(this);
		IConstructor alias = stack.pop();
		declaration.getMappingName().accept(this);
		IConstructor mappingName = stack.pop();
		stack.push(builder.Declaration_namespaceAlias(alias, mappingName, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTStaticAssertDeclaration declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		declaration.getCondition().accept(this);
		IConstructor condition = stack.pop();
		declaration.getMessage().accept(this);
		stack.push(builder.Declaration_staticAssert(condition, stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTemplateDeclaration declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		// The "export" keyword has been removed from the C++ standard
		IASTDeclaration _declaration = declaration.getDeclaration();
		ICPPASTTemplateParameter[] _templateParameters = declaration.getTemplateParameters();
		IListWriter templateParameters = vf.listWriter();
		Stream.of(_templateParameters).forEach(it -> {
			it.accept(this);
			templateParameters.append(stack.pop());
		});
		_declaration.accept(this);
		stack.push(builder.Declaration_template(templateParameters.done(), stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTemplateSpecialization declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		declaration.getDeclaration().accept(this);
		stack.push(builder.Declaration_explicitTemplateSpecialization(stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTUsingDeclaration declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		ISourceLocation decl = br.resolveBinding(declaration);
		IList attributes = getAttributes(declaration);
		IList modifiers = getModifiers(declaration);
		declaration.getName().accept(this);
		stack.push(builder.Declaration_usingDeclaration(attributes, modifiers, stack.pop(), loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTUsingDirective declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		ISourceLocation decl = br.resolveBinding(declaration);
		IList attributes = getAttributes(declaration);
		IASTName qualifiedName = declaration.getQualifiedName();
		qualifiedName.accept(this);
		stack.push(builder.Declaration_usingDirective(attributes, stack.pop(), loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTVisibilityLabel declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		int visibility = declaration.getVisibility();
		switch (visibility) {
		case ICPPASTVisibilityLabel.v_public:
			stack.push(builder.Declaration_visibilityLabel(builder.Modifier_public(loc), loc));
			break;
		case ICPPASTVisibilityLabel.v_protected:
			stack.push(builder.Declaration_visibilityLabel(builder.Modifier_protected(loc), loc));
			break;
		case ICPPASTVisibilityLabel.v_private:
			stack.push(builder.Declaration_visibilityLabel(builder.Modifier_private(loc), loc));
			break;
		default:
			throw new RuntimeException("Unknown CPPVisibilityLabel code " + visibility + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTASMDeclaration declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		stack.push(builder.Declaration_asmDeclaration(declaration.getAssembly(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTFunctionDefinition definition) {
		ISourceLocation loc = getSourceLocation(definition);
		if (definition instanceof ICPPASTFunctionDefinition) {
			IList attributes = getAttributes((ICPPASTFunctionDefinition) definition);
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
			if ((isDefaulted || isDeleted) && definition instanceof ICPPASTFunctionWithTryBlock)
				throw new RuntimeException("IASTFunctionDefinition defaulted/deleted and with try?");
			if (isDefaulted)
				stack.push(builder.Declaration_defaultedFunctionDefinition(attributes, declSpecifier,
						memberInitializers.done(), declarator, loc));
			else if (isDeleted)
				stack.push(builder.Declaration_deletedFunctionDefinition(attributes, declSpecifier,
						memberInitializers.done(), declarator, loc));
			else if (definition instanceof ICPPASTFunctionWithTryBlock) {
				ICPPASTCatchHandler[] _catchHandlers = ((ICPPASTFunctionWithTryBlock) definition).getCatchHandlers();
				IListWriter catchHandlers = vf.listWriter();
				Stream.of(_catchHandlers).forEach(it -> {
					it.accept(this);
					catchHandlers.append(stack.pop());
				});
				_body.accept(this);
				stack.push(builder.Declaration_functionWithTryBlockDefinition(attributes, declSpecifier, declarator,
						memberInitializers.done(), stack.pop(), catchHandlers.done(), loc));
			} else {
				_body.accept(this);
				stack.push(builder.Declaration_functionDefinition(attributes, declSpecifier, declarator,
						memberInitializers.done(), stack.pop(), loc));
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
			stack.push(builder.Declaration_functionDefinition(declSpecifier, declarator, body, loc));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTParameterDeclaration parameterDeclaration) {
		ISourceLocation loc = getSourceLocation(parameterDeclaration);
		// TODO: remove duplicate code
		if (parameterDeclaration instanceof ICPPASTParameterDeclaration) {
			ICPPASTParameterDeclaration declaration = (ICPPASTParameterDeclaration) parameterDeclaration;
			IASTDeclSpecifier _declSpecifier = declaration.getDeclSpecifier();
			ICPPASTDeclarator _declarator = declaration.getDeclarator();

			_declSpecifier.accept(this);
			IConstructor declSpecifier = stack.pop();
			if (_declarator == null)
				stack.push(builder.Declaration_parameter(declSpecifier, loc));
			else {
				_declarator.accept(this);
				stack.push(builder.Declaration_parameter(declSpecifier, stack.pop(), loc));
			}
		} else {
			IASTDeclSpecifier _declSpecifier = parameterDeclaration.getDeclSpecifier();
			IASTDeclarator _declarator = parameterDeclaration.getDeclarator();

			_declSpecifier.accept(this);
			IConstructor declSpecifier = stack.pop();
			if (_declarator == null)
				stack.push(builder.Declaration_parameter(declSpecifier, loc));
			else {
				_declarator.accept(this);
				stack.push(builder.Declaration_parameter(declSpecifier, stack.pop(), loc));
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTProblemDeclaration declaration) {
		err("ProblemDeclaration: ");
		prefix += 4;
		err(declaration.getProblem().getMessageWithLocation());
		err(declaration.getRawSignature());
		prefix -= 4;
		String problem = declaration.getProblem().getMessageWithLocation();
		ISourceLocation loc = getSourceLocation(declaration);
		String raw = declaration.getRawSignature();
		stack.push(builder.Declaration_problemDeclaration(getSourceLocation(declaration)));
		return PROCESS_ABORT;
	}

	public int visit(IASTSimpleDeclaration declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		IASTDeclSpecifier _declSpecifier = declaration.getDeclSpecifier();
		IASTDeclarator[] _declarators = declaration.getDeclarators();
		IList attributes = getAttributes(declaration);

		_declSpecifier.accept(this);
		IConstructor declSpecifier = stack.pop();
		IListWriter declarators = vf.listWriter();
		Stream.of(_declarators).forEach(it -> {
			it.accept(this);
			declarators.append(stack.pop());
		});
		stack.push(builder.Declaration_simpleDeclaration(attributes, declSpecifier, declarators.done(), loc));
		return PROCESS_ABORT;
	}

	// Initializers

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
		ISourceLocation loc = getSourceLocation(initializer);
		IASTInitializerClause initializerClause = initializer.getInitializerClause();
		initializerClause.accept(this);
		stack.push(builder.Expression_equalsInitializer(stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTInitializerList initializer) {
		// TODO: cpp: check isPackExpansion, maybe getSize
		ISourceLocation loc = getSourceLocation(initializer);
		IASTInitializerClause[] _clauses = initializer.getClauses();
		IListWriter clauses = vf.listWriter();
		Stream.of(_clauses).forEach(it -> {
			it.accept(this);
			clauses.append(stack.pop());
		});
		stack.push(builder.Expression_initializerList(clauses.done(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICASTDesignatedInitializer initializer) {
		err("ICASTDesignatedInitializer: " + initializer.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTConstructorChainInitializer initializer) {
		// TODO: check isPackExpansion
		ISourceLocation loc = getSourceLocation(initializer);
		ISourceLocation decl = br.resolveBinding(initializer);
		IASTName _memberInitializerId = initializer.getMemberInitializerId();
		IASTInitializer _memberInitializer = initializer.getInitializer();
		_memberInitializerId.accept(this);
		IConstructor memberInitializerId = stack.pop();
		_memberInitializer.accept(this);
		IConstructor memberInitializer = stack.pop();
		stack.push(builder.Expression_constructorChainInitializer(memberInitializerId, memberInitializer, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTConstructorInitializer initializer) {
		ISourceLocation loc = getSourceLocation(initializer);
		IASTInitializerClause[] _arguments = initializer.getArguments();
		IListWriter arguments = vf.listWriter();
		Stream.of(_arguments).forEach(it -> {
			it.accept(this);
			arguments.append(stack.pop());
		});
		stack.push(builder.Expression_constructorInitializer(arguments.done(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTDesignatedInitializer initializer) {
		ISourceLocation loc = getSourceLocation(initializer);
		ICPPASTDesignator[] _designators = initializer.getDesignators();
		ICPPASTInitializerClause _operand = initializer.getOperand();

		IListWriter designators = vf.listWriter();
		Stream.of(_designators).forEach(it -> {
			it.accept(this);
			designators.append(stack.pop());
		});
		_operand.accept(this);

		stack.push(builder.Expression_designatedInitializer(designators.done(), stack.pop(), loc));
		return PROCESS_ABORT;
	}

	// InitializerClauses

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

	// Declarators

	@Override
	public int visit(IASTDeclarator declarator) {
		if (declarator instanceof IASTArrayDeclarator)
			visit((IASTArrayDeclarator) declarator);
		// else if (declarator instanceof IASTFieldDeclarator)
		// visit((IASTFieldDeclarator) declarator);
		else if (declarator instanceof IASTFunctionDeclarator)
			visit((IASTFunctionDeclarator) declarator);
		else if (declarator instanceof ICPPASTDeclarator)
			visit((ICPPASTDeclarator) declarator);
		else {
			// TODO: add attributes
			IASTPointerOperator[] _pointerOperators = declarator.getPointerOperators();
			IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
			IASTName _name = declarator.getName();
			IASTInitializer _initializer = declarator.getInitializer();

			List<IConstructor> pointerOperators = new ArrayList<IConstructor>();
			Stream.of(_pointerOperators).forEach(it -> {
				it.accept(this);
				pointerOperators.add(stack.pop());
			});
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
		ISourceLocation loc = getSourceLocation(declarator);
		ISourceLocation decl = br.resolveBinding(declarator);
		IList attributes = getAttributes(declarator);

		IListWriter arrayModifiers = vf.listWriter();
		Stream.of(declarator.getArrayModifiers()).forEach(it -> {
			it.accept(this);
			arrayModifiers.append(stack.pop());
		});

		IListWriter pointerOperators = vf.listWriter();
		Stream.of(declarator.getPointerOperators()).forEach(it -> {
			it.accept(this);
			pointerOperators.append(stack.pop());
		});

		declarator.getName().accept(this);
		IConstructor name = stack.pop();

		// TODO: check declaresParameterPack
		if (declarator instanceof ICPPASTArrayDeclarator
				&& ((ICPPASTArrayDeclarator) declarator).declaresParameterPack())
			out("WARNING: IASTArrayDeclarator has declaresParameterPack=true");

		IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
		IASTInitializer _initializer = declarator.getInitializer();
		if (_nestedDeclarator == null) {
			if (_initializer == null)
				stack.push(builder.Declarator_arrayDeclarator(attributes, pointerOperators.done(), name,
						arrayModifiers.done(), loc, decl));
			else {
				_initializer.accept(this);
				stack.push(builder.Declarator_arrayDeclarator(attributes, pointerOperators.done(), name,
						arrayModifiers.done(), stack.pop(), loc, decl));
			}
		} else {
			_nestedDeclarator.accept(this);
			IConstructor nestedDeclarator = stack.pop();
			if (_initializer == null)
				stack.push(builder.Declarator_arrayDeclaratorNested(attributes, pointerOperators.done(),
						nestedDeclarator, arrayModifiers.done(), loc, decl));
			else {
				_initializer.accept(this);
				stack.push(builder.Declarator_arrayDeclaratorNested(attributes, pointerOperators.done(),
						nestedDeclarator, arrayModifiers.done(), stack.pop(), loc, decl));
			}
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTFieldDeclarator declarator) {
		// TODO: implement
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
		if (declarator instanceof ICPPASTFunctionDeclarator)
			visit((ICPPASTFunctionDeclarator) declarator);
		else {
			// TODO: not reached, remove?
			// TODO: check getNestedDeclarator and getInitializer
			ISourceLocation loc = getSourceLocation(declarator);
			ISourceLocation decl = br.resolveBinding(declarator);
			IList attributes = getAttributes(declarator);

			IASTName _name = declarator.getName();
			IASTParameterDeclaration[] _parameters = declarator.getParameters();
			if (declarator.takesVarArgs())
				err("WARNING: IASTStandardFunctionDeclarator has takesVarArgs=true");

			IASTPointerOperator[] _pointerOperators = declarator.getPointerOperators();
			IListWriter pointerOperators = vf.listWriter();
			Stream.of(_pointerOperators).forEach(it -> {
				it.accept(this);
				pointerOperators.append(stack.pop());
			});

			_name.accept(this);
			IConstructor name = stack.pop();
			IListWriter parameters = vf.listWriter();
			Stream.of(_parameters).forEach(it -> {
				it.accept(this);
				parameters.append(stack.pop());
			});
			stack.push(builder.Declarator_functionDeclarator(attributes, pointerOperators.done(), name,
					parameters.done(), loc, decl));
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
		// TODO: implement
		// check add getDeclarator, getParameterNames, getParameterDeclarations

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
			ISourceLocation loc = getSourceLocation(declarator);
			ISourceLocation decl = br.resolveBinding(declarator);
			IList attributes = getAttributes(declarator);
			IASTPointerOperator[] _pointerOperators = declarator.getPointerOperators();
			IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
			IASTName _name = declarator.getName();
			IASTInitializer _initializer = declarator.getInitializer();

			IListWriter pointerOperators = vf.listWriter();
			Stream.of(_pointerOperators).forEach(it -> {
				it.accept(this);
				pointerOperators.append(stack.pop());
			});
			if (_nestedDeclarator != null)
				err("WARNING: ICPPASTDeclarator has nestedDeclarator " + _nestedDeclarator.getRawSignature());
			_name.accept(this);
			IConstructor name = stack.pop();
			IConstructor initializer = null;
			if (_initializer == null) {
				stack.push(builder.Declarator_declarator(attributes, pointerOperators.done(), name, loc, decl));
			} else {
				_initializer.accept(this);
				initializer = stack.pop();
				stack.push(builder.Declarator_declarator(attributes, pointerOperators.done(), name, initializer, loc,
						decl));
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTArrayDeclarator declarator) {
		visit((IASTArrayDeclarator) declarator);
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTFieldDeclarator declarator) {
		ISourceLocation loc = getSourceLocation(declarator);
		ISourceLocation decl = br.resolveBinding(declarator);
		IList attributes = getAttributes(declarator);

		IListWriter pointerOperators = vf.listWriter();
		Stream.of(declarator.getPointerOperators()).forEach(it -> {
			it.accept(this);
			pointerOperators.append(stack.pop());
		});

		declarator.getBitFieldSize().accept(this);
		IConstructor bitFieldSize = stack.pop();

		IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
		if (_nestedDeclarator != null)
			err("WARNING: ICPPASTDeclarator has nestedDeclarator " + _nestedDeclarator.getRawSignature());

		declarator.getName().accept(this);
		IConstructor name = stack.pop();

		IASTInitializer _initializer = declarator.getInitializer();
		if (_initializer == null) {
			stack.push(builder.Declarator_fieldDeclarator(attributes, pointerOperators.done(), name, bitFieldSize, loc,
					decl));
		} else {
			_initializer.accept(this);
			stack.push(builder.Declarator_fieldDeclarator(attributes, pointerOperators.done(), name, bitFieldSize,
					stack.pop(), loc, decl));
		}

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTFunctionDeclarator declarator) {
		// TODO: check refQualifier and declaresParameterPack
		ISourceLocation loc = getSourceLocation(declarator);
		ISourceLocation decl = br.resolveBinding(declarator);
		IList attributes = getAttributes(declarator);
		IList modifiers = getModifiers(declarator);

		IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
		IASTInitializer _initializer = declarator.getInitializer();
		IASTTypeId _trailingReturnType = declarator.getTrailingReturnType();
		IASTTypeId[] _exceptionSpecification = declarator.getExceptionSpecification();
		ICPPASTExpression _noexceptExpression = declarator.getNoexceptExpression();

		// TODO: fix when name == null
		IConstructor name = builder.Expression_name("", loc);
		IASTName _name = declarator.getName();
		if (_name != null) {
			_name.accept(this);
			name = stack.pop();
		}

		IListWriter parameters = vf.listWriter();
		Stream.of(declarator.getParameters()).forEach(it -> {
			it.accept(this);
			parameters.append(stack.pop());
		});
		if (declarator.takesVarArgs())
			parameters.append(builder.Declaration_varArgs(loc));

		IListWriter virtSpecifiers = vf.listWriter();
		Stream.of(declarator.getVirtSpecifiers()).forEach(it -> {
			it.accept(this);
			virtSpecifiers.append(stack.pop());
		});

		IListWriter pointerOperators = vf.listWriter();
		Stream.of(declarator.getPointerOperators()).forEach(it -> {
			it.accept(this);
			pointerOperators.append(stack.pop());
		});

		if (_nestedDeclarator != null) {
			if (_trailingReturnType != null)
				throw new RuntimeException("FunctionDeclarator: Trailing return type and nested declarator?");
			_nestedDeclarator.accept(this);
			IConstructor nestedDeclarator = stack.pop();
			if (_initializer == null)
				stack.push(builder.Declarator_functionDeclaratorNested(attributes, pointerOperators.done(), modifiers,
						nestedDeclarator, parameters.done(), virtSpecifiers.done(), loc, decl));
			else {
				_initializer.accept(this);
				stack.push(builder.Declarator_functionDeclaratorNested(attributes, pointerOperators.done(), modifiers,
						nestedDeclarator, parameters.done(), virtSpecifiers.done(), stack.pop(), loc, decl));
			}
			if (!(_exceptionSpecification.equals(ICPPASTFunctionDeclarator.NO_EXCEPTION_SPECIFICATION)))
				err("WARNING: ICPPASTFunctionDeclaration had nestedDeclarator and also exceptionSpecification");
		} else if (_exceptionSpecification.equals(ICPPASTFunctionDeclarator.NO_EXCEPTION_SPECIFICATION)) {
			if (_trailingReturnType == null)
				stack.push(builder.Declarator_functionDeclarator(attributes, pointerOperators.done(), modifiers, name,
						parameters.done(), virtSpecifiers.done(), loc, decl));
			else {
				_trailingReturnType.accept(this);
				stack.push(builder.Declarator_functionDeclarator(attributes, pointerOperators.done(), modifiers, name,
						parameters.done(), virtSpecifiers.done(), stack.pop(), loc, decl));
			}
		} else if (_exceptionSpecification.equals(IASTTypeId.EMPTY_TYPEID_ARRAY)) {
			if (_trailingReturnType != null)
				throw new RuntimeException("FunctionDeclarator: Trailing return type and exception specification?");
			stack.push(builder.Declarator_functionDeclaratorWithES(attributes, pointerOperators.done(), modifiers, name,
					parameters.done(), virtSpecifiers.done(), loc, decl));
		} else if (_noexceptExpression != null) {
			if (_trailingReturnType != null)
				throw new RuntimeException("FunctionDeclarator: Trailing return type and noexceptExpression?");
			if (_initializer != null)
				throw new RuntimeException("FunctionDeclarator: Initializer and noexceptExpression?");
			_noexceptExpression.accept(this);
			stack.push(builder.Declarator_functionDeclaratorNoexcept(attributes, pointerOperators.done(), modifiers,
					name, parameters.done(), virtSpecifiers.done(), stack.pop(), loc, decl));
		} else {
			if (_trailingReturnType != null)
				throw new RuntimeException("FunctionDeclarator: Trailing return type and exception specification?");
			IListWriter exceptionSpecification = vf.listWriter();
			Stream.of(_exceptionSpecification).forEach(it -> {
				it.accept(this);
				exceptionSpecification.append(stack.pop());
			});
			stack.push(builder.Declarator_functionDeclaratorWithES(attributes, pointerOperators.done(), modifiers, name,
					parameters.done(), virtSpecifiers.done(), exceptionSpecification.done(), loc, decl));
		}

		return PROCESS_ABORT;
	}

	// DeclSpecifiers

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
		ISourceLocation loc = getSourceLocation(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec);
		IList modifiers = getModifiers(declSpec);

		int key = declSpec.getKey();
		IASTName _name = declSpec.getName();
		IASTDeclaration[] _members = declSpec.getMembers();
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
			stack.push(builder.DeclSpecifier_struct(modifiers, name, members.done(), loc, decl));
			break;
		case IASTCompositeTypeSpecifier.k_union:
			stack.push(builder.DeclSpecifier_union(modifiers, name, members.done(), loc, decl));
			break;
		default:
			throw new RuntimeException("Unknown IASTCompositeTypeSpecifier code " + key + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTCompositeTypeSpecifier declSpec) {
		ISourceLocation loc = getSourceLocation(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec);
		IList attributes = getAttributes(declSpec);
		IList modifiers = getModifiers(declSpec);

		ICPPASTClassVirtSpecifier virtSpecifier = declSpec.getVirtSpecifier();
		if (virtSpecifier != null && !(virtSpecifier.getKind().equals(ICPPASTClassVirtSpecifier.SpecifierKind.Final)))
			throw new RuntimeException("ICPPASTCompositeTypeSpecifier encountered unknown classVirtSpecifier type");

		declSpec.getName().accept(this);
		IConstructor name = stack.pop();

		IListWriter members = vf.listWriter();
		Stream.of(declSpec.getMembers()).forEach(it -> {
			it.accept(this);
			members.append(stack.pop());
		});

		IListWriter baseSpecifiers = vf.listWriter();
		Stream.of(declSpec.getBaseSpecifiers()).forEach(it -> {
			it.accept(this);
			baseSpecifiers.append(stack.pop());
		});

		switch (declSpec.getKey()) {
		case ICPPASTCompositeTypeSpecifier.k_struct:
			if (virtSpecifier == null)
				stack.push(builder.DeclSpecifier_struct(attributes, modifiers, name, baseSpecifiers.done(),
						members.done(), loc, decl));
			else
				stack.push(builder.DeclSpecifier_structFinal(attributes, modifiers, name, baseSpecifiers.done(),
						members.done(), loc, decl));
			break;
		case ICPPASTCompositeTypeSpecifier.k_union:
			if (virtSpecifier == null)
				stack.push(builder.DeclSpecifier_union(attributes, modifiers, name, baseSpecifiers.done(),
						members.done(), loc, decl));
			else
				stack.push(builder.DeclSpecifier_unionFinal(attributes, modifiers, name, baseSpecifiers.done(),
						members.done(), loc, decl));
			break;
		case ICPPASTCompositeTypeSpecifier.k_class:
			if (virtSpecifier == null)
				stack.push(builder.DeclSpecifier_class(attributes, modifiers, name, baseSpecifiers.done(),
						members.done(), loc, decl));
			else
				stack.push(builder.DeclSpecifier_classFinal(attributes, modifiers, name, baseSpecifiers.done(),
						members.done(), loc, decl));
			break;
		default:
			throw new RuntimeException("Unknown IASTCompositeTypeSpecifier code " + declSpec.getKey() + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTElaboratedTypeSpecifier declSpec) {
		ISourceLocation loc = getSourceLocation(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec);
		if (declSpec instanceof ICASTElaboratedTypeSpecifier) {
			out("ElaboratedTypeSpecifier: " + declSpec.getRawSignature());
			throw new RuntimeException("NYI");
		} else if (declSpec instanceof ICPPASTElaboratedTypeSpecifier) {
			int kind = declSpec.getKind();
			IASTName _name = declSpec.getName();
			IList modifiers = getModifiers(declSpec);
			_name.accept(this);
			switch (kind) {
			case ICPPASTElaboratedTypeSpecifier.k_enum:
				stack.push(builder.DeclSpecifier_etsEnum(modifiers, stack.pop(), loc, decl));
				break;
			case ICPPASTElaboratedTypeSpecifier.k_struct:
				stack.push(builder.DeclSpecifier_etsStruct(modifiers, stack.pop(), loc, decl));
				break;
			case ICPPASTElaboratedTypeSpecifier.k_union:
				stack.push(builder.DeclSpecifier_etsUnion(modifiers, stack.pop(), loc, decl));
				break;
			case ICPPASTElaboratedTypeSpecifier.k_class:
				stack.push(builder.DeclSpecifier_etsClass(modifiers, stack.pop(), loc, decl));
				break;
			default:
				throw new RuntimeException("IASTElaboratedTypeSpecifier encountered unknown kind " + kind);
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTEnumerationSpecifier declSpec) {
		if (declSpec instanceof ICPPASTEnumerationSpecifier)
			visit((ICPPASTEnumerationSpecifier) declSpec);
		else
			throw new RuntimeException("NYI");
		return PROCESS_ABORT;
	}

	public int visit(IASTNamedTypeSpecifier declSpec) {
		ISourceLocation loc = getSourceLocation(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec);
		IList modifiers = getModifiers(declSpec);
		declSpec.getName().accept(this);
		stack.push(builder.DeclSpecifier_namedTypeSpecifier(modifiers, stack.pop(), loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(IASTSimpleDeclSpecifier declSpec) {
		if (declSpec instanceof ICPPASTSimpleDeclSpecifier) {
			visit((ICPPASTSimpleDeclSpecifier) declSpec);
			return PROCESS_ABORT;
		}
		throw new RuntimeException("NYI: C SimpleDeclSpecifier");
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
		ISourceLocation loc = getSourceLocation(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec);
		IList attributes = getAttributes(declSpec);
		IList modifiers = getModifiers(declSpec);

		IASTName _name = declSpec.getName();
		IASTEnumerator[] _enumerators = declSpec.getEnumerators();
		IASTDeclSpecifier _baseType = declSpec.getBaseType();

		// TODO: check isOpaque
		if (declSpec.isOpaque())
			err("WARNING: ICPPASTEnumerationSpecifier has isOpaque=true");

		_name.accept(this);
		IConstructor name = stack.pop();
		IListWriter enumerators = vf.listWriter();
		Stream.of(_enumerators).forEach(it -> {
			it.accept(this);
			enumerators.append(stack.pop());
		});

		if (_baseType == null) {
			if (declSpec.isScoped())
				stack.push(
						builder.DeclSpecifier_enumScoped(attributes, modifiers, name, enumerators.done(), loc, decl));
			else
				stack.push(builder.DeclSpecifier_enum(attributes, modifiers, name, enumerators.done(), loc, decl));
		} else {
			_baseType.accept(this);
			IConstructor baseType = stack.pop();
			if (declSpec.isScoped())
				stack.push(builder.DeclSpecifier_enumScoped(attributes, modifiers, baseType, name, enumerators.done(),
						loc, decl));
			else
				stack.push(builder.DeclSpecifier_enum(attributes, modifiers, baseType, name, enumerators.done(), loc,
						decl));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNamedTypeSpecifier declSpec) {
		out("CPPNamedTypeSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTSimpleDeclSpecifier declSpec) {
		ISourceLocation loc = getSourceLocation(declSpec);
		IList attributes = getAttributes((ICPPASTSimpleDeclSpecifier) declSpec);
		IList modifiers = getModifiers(declSpec);

		switch (declSpec.getType()) {
		case IASTSimpleDeclSpecifier.t_unspecified:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_unspecified(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_void:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_void(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_char:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_char(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_int:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_integer(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_float:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_float(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_double:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_double(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_bool:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_bool(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_wchar_t:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_wchar_t(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_typeof:
			declSpec.getDeclTypeExpression().accept(this);
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_typeof(loc), stack.pop(),
					loc));
			break;
		case IASTSimpleDeclSpecifier.t_decltype:
			declSpec.getDeclTypeExpression().accept(this);
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_decltype(loc),
					stack.pop(), loc));
			break;
		case IASTSimpleDeclSpecifier.t_auto:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_auto(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_char16_t:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_char16_t(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_char32_t:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_char32_t(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_int128:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_int128(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_float128:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_float128(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_decimal32:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_decimal128(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_decimal64:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_decimal64(loc), loc));
			break;
		case IASTSimpleDeclSpecifier.t_decimal128:
			stack.push(builder.DeclSpecifier_declSpecifier(attributes, modifiers, builder.Type_decimal128(loc), loc));
			break;
		default:
			throw new RuntimeException("Unknown IASTSimpleDeclSpecifier kind " + declSpec.getType() + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTypeTransformationSpecifier declSpec) {
		// TODO: implement, check operator and operand
		err("ICPPASTTypeTransformationSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(IASTArrayModifier arrayModifier) {
		if (arrayModifier instanceof ICASTArrayModifier)
			throw new RuntimeException("NYI");
		ISourceLocation loc = getSourceLocation(arrayModifier);
		IList attributes = getAttributes(arrayModifier);
		IASTExpression _constantExpression = arrayModifier.getConstantExpression();
		if (_constantExpression == null)
			stack.push(builder.Expression_arrayModifier(attributes, loc));
		else {
			_constantExpression.accept(this);
			stack.push(builder.Expression_arrayModifier(attributes, stack.pop(), loc));
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
		ISourceLocation loc = getSourceLocation(pointer);
		IList attributes = getAttributes(pointer);
		IList modifiers = getModifiers(pointer);
		if (pointer instanceof ICPPASTPointerToMember) {
			((ICPPASTPointerToMember) pointer).getName().accept(this);
			stack.push(builder.Declaration_pointerToMember(attributes, modifiers, stack.pop(), loc));
		} else
			stack.push(builder.Declaration_pointer(attributes, modifiers, loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTReferenceOperator referenceOperator) {
		ISourceLocation loc = getSourceLocation(referenceOperator);
		IList attributes = getAttributes(referenceOperator);
		if (referenceOperator.isRValueReference())
			stack.push(builder.Declaration_rvalueReference(attributes, loc));
		else
			stack.push(builder.Declaration_reference(attributes, loc));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTAttribute attribute) {
		ISourceLocation src = getSourceLocation(attribute);
		if (attribute.getArgumentClause() == null)
			stack.push(builder.Attribute_attribute(new String(attribute.getName()), src));
		else
			stack.push(builder.Attribute_attribute(new String(attribute.getName()),
					new String(attribute.getArgumentClause().getTokenCharImage()), src));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTAttributeSpecifier specifier) {
		ISourceLocation src = getSourceLocation(specifier);
		IASTAttributeList list = (IASTAttributeList) specifier;
		IListWriter attributes = vf.listWriter();
		Stream.of(list.getAttributes()).forEach(it -> {
			it.accept(this);
			attributes.append(stack.pop());
		});
		stack.push(builder.Attribute_attributeSpecifier(attributes.done(), src));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTToken token) {
		err("Token: " + new String(token.getTokenCharImage()));
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(IASTExpression expression) {
		if (expression instanceof IASTArraySubscriptExpression)
			visit((IASTArraySubscriptExpression) expression);
		else if (expression instanceof IASTBinaryExpression)
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

	public int visit(ICPPASTArraySubscriptExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		ICPPASTExpression _arrayExpression = expression.getArrayExpression();
		ICPPASTInitializerClause _argument = expression.getArgument();

		_arrayExpression.accept(this);
		IConstructor arrayExpression = stack.pop();
		_argument.accept(this);
		IConstructor argument = stack.pop();

		stack.push(builder.Expression_arraySubscriptExpression(arrayExpression, argument, loc, typ));

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTBinaryExpression expression) {
		out("CPPBinaryExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTCastExpression expression) {
		out("CPPCastExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTDeleteExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		expression.getOperand().accept(this);
		if (expression.isGlobal()) {
			if (expression.isVectored())
				stack.push(builder.Expression_globalVectoredDelete(stack.pop(), loc));
			else
				stack.push(builder.Expression_globalDelete(stack.pop(), loc));
		} else {
			if (expression.isVectored())
				stack.push(builder.Expression_vectoredDelete(stack.pop(), loc));
			else
				stack.push(builder.Expression_delete(stack.pop(), loc));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTExpressionList expression) {
		out("CPPExpressionList: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTFieldReference expression) {
		// TODO: Implement
		out("CPPFieldReference: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTFunctionCallExpression expression) {
		out("CPPFunctionCallExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTLambdaExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		ISourceLocation decl = br.UNKNOWN;
		CaptureDefault captureDefault = expression.getCaptureDefault();
		ICPPASTCapture[] _captures = expression.getCaptures();
		IASTImplicitName _closureTypeName = expression.getClosureTypeName();
		ICPPASTFunctionDeclarator _declarator = expression.getDeclarator();
		IASTImplicitName _functionCallOperatorName = expression.getFunctionCallOperatorName();
		IASTCompoundStatement _body = expression.getBody();

		IListWriter captures = vf.listWriter();
		Stream.of(_captures).forEach(it -> {
			it.accept(this);
			captures.append(stack.pop());
		});

		// TODO: check to remove closureType and functionCallOperatorName
		if (!_closureTypeName.getRawSignature().equals("["))
			err("ICPPASTLambdaExpression has closureTypeName " + _closureTypeName.getRawSignature()
					+ ", not implemented");
		if (!_functionCallOperatorName.getRawSignature().equals("{"))
			err("ICPPASTLambdaExpression has functionCallOperatorName " + _functionCallOperatorName.getRawSignature()
					+ ", not implemented");

		IConstructor declarator;
		if (_declarator == null)
			declarator = builder.Declarator_missingDeclarator(loc, decl);
		else {
			_declarator.accept(this);
			declarator = stack.pop();
		}
		_body.accept(this);
		IConstructor body = stack.pop();

		switch (captureDefault) {
		case BY_COPY:
			stack.push(builder.Expression_lambda(builder.Modifier_captDefByCopy(loc), captures.done(), declarator, body,
					loc));
			break;
		case BY_REFERENCE:
			stack.push(builder.Expression_lambda(builder.Modifier_captDefByReference(loc), captures.done(), declarator,
					body, loc));
			break;
		case UNSPECIFIED:
			stack.push(builder.Expression_lambda(builder.Modifier_captDefUnspecified(loc), captures.done(), declarator,
					body, loc));
			break;
		default:
			throw new RuntimeException("Unknown default capture type " + captureDefault + " encountered, exiting");
		}

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTLiteralExpression expression) {
		// This may never be reached
		visit((IASTLiteralExpression) expression);
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNaryTypeIdExpression expression) {
		out("CPPNaryTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTNewExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		if (expression.isNewTypeId())
			err("WARNING: ICPPASTNewExpression has isNewTypeId=true");

		IASTTypeId _typeId = expression.getTypeId();
		IASTInitializer _initializer = expression.getInitializer();

		_typeId.accept(this);
		IConstructor typeId = stack.pop();

		IASTInitializerClause[] _placementArguments = expression.getPlacementArguments();
		if (_placementArguments != null) {
			IListWriter placementArguments = vf.listWriter();
			Stream.of(_placementArguments).forEach(it -> {
				it.accept(this);
				placementArguments.append(stack.pop());
			});
			if (_initializer == null) {
				if (expression.isGlobal())
					stack.push(builder.Expression_globalNewWithArgs(placementArguments.done(), typeId, loc));
				else
					stack.push(builder.Expression_newWithArgs(placementArguments.done(), typeId, loc));
			} else {
				_initializer.accept(this);
				IConstructor initializer = stack.pop();
				if (expression.isGlobal())
					stack.push(
							builder.Expression_globalNewWithArgs(placementArguments.done(), typeId, initializer, loc));
				else
					stack.push(builder.Expression_newWithArgs(placementArguments.done(), typeId, initializer, loc));
			}
		} else if (_initializer == null) {
			if (expression.isGlobal())
				stack.push(builder.Expression_globalNew(typeId, loc));
			else
				stack.push(builder.Expression_new(typeId, loc));
		} else {
			_initializer.accept(this);
			IConstructor initializer = stack.pop();
			if (expression.isGlobal())
				stack.push(builder.Expression_globalNew(typeId, initializer, loc));
			else
				stack.push(builder.Expression_new(typeId, initializer, loc));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTPackExpansionExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		expression.getPattern().accept(this);
		stack.push(builder.Expression_packExpansion(stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTSimpleTypeConstructorExpression expression) {
		// decl keyword parameter?
		ISourceLocation loc = getSourceLocation(expression);
		ICPPASTDeclSpecifier _declSpecifier = expression.getDeclSpecifier();
		IASTInitializer _initializer = expression.getInitializer();
		_declSpecifier.accept(this);
		IConstructor declSpecifier = stack.pop();
		_initializer.accept(this);
		IConstructor initializer = stack.pop();
		stack.push(builder.Expression_simpleTypeConstructor(declSpecifier, initializer, loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTypeIdExpression expression) {
		out("CPPTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTUnaryExpression expression) {
		out("CPPUnaryExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTArraySubscriptExpression expression) {
		if (expression instanceof ICPPASTArraySubscriptExpression)
			visit((ICPPASTArraySubscriptExpression) expression);
		else
			throw new RuntimeException("NYI");
		return PROCESS_ABORT;
	}

	public int visit(IASTBinaryExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		IASTExpression _lhs = expression.getOperand1();
		_lhs.accept(this);
		IConstructor lhs = stack.pop();
		int op = expression.getOperator();
		IASTExpression _rhs = expression.getOperand2();
		_rhs.accept(this);
		IConstructor rhs = stack.pop();

		switch (op) {
		case IASTBinaryExpression.op_multiply:
			stack.push(builder.Expression_multiply(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_divide:
			stack.push(builder.Expression_divide(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_modulo:
			stack.push(builder.Expression_modulo(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_plus:
			stack.push(builder.Expression_plus(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_minus:
			stack.push(builder.Expression_minus(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_shiftLeft:
			stack.push(builder.Expression_shiftLeft(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_shiftRight:
			stack.push(builder.Expression_shiftRight(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_lessThan:
			stack.push(builder.Expression_lessThan(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_greaterThan:
			stack.push(builder.Expression_greaterThan(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_lessEqual:
			stack.push(builder.Expression_lessEqual(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_greaterEqual:
			stack.push(builder.Expression_greaterEqual(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_binaryAnd:
			stack.push(builder.Expression_binaryAnd(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_binaryXor:
			stack.push(builder.Expression_binaryXor(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_binaryOr:
			stack.push(builder.Expression_binaryOr(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_logicalAnd:
			stack.push(builder.Expression_logicalAnd(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_logicalOr:
			stack.push(builder.Expression_logicalOr(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_assign:
			stack.push(builder.Expression_assign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_multiplyAssign:
			stack.push(builder.Expression_multiplyAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_divideAssign:
			stack.push(builder.Expression_divideAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_moduloAssign:
			stack.push(builder.Expression_moduloAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_plusAssign:
			stack.push(builder.Expression_plusAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_minusAssign:
			stack.push(builder.Expression_minusAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_shiftLeftAssign:
			stack.push(builder.Expression_shiftLeftAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_shiftRightAssign:
			stack.push(builder.Expression_shiftRightAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_binaryAndAssign:
			stack.push(builder.Expression_binaryAndAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_binaryXorAssign:
			stack.push(builder.Expression_binaryXorAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_binaryOrAssign:
			stack.push(builder.Expression_binaryOrAssign(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_equals:
			stack.push(builder.Expression_equals(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_notequals:
			stack.push(builder.Expression_notEquals(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_pmdot:
			stack.push(builder.Expression_pmDot(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_pmarrow:
			stack.push(builder.Expression_pmArrow(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_max:
			stack.push(builder.Expression_max(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_min:
			stack.push(builder.Expression_min(lhs, rhs, loc, typ));
			break;
		case IASTBinaryExpression.op_ellipses:
			stack.push(builder.Expression_ellipses(lhs, rhs, loc, typ));
			break;
		default:
			throw new RuntimeException("Operator " + op + " unknown, exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTBinaryTypeIdExpression expression) {
		// has typ
		out("BinaryTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTCastExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		int operator = expression.getOperator();
		IASTExpression _operand = expression.getOperand();
		IASTTypeId typeId = expression.getTypeId();
		_operand.accept(this);
		IConstructor operand = stack.pop();
		typeId.accept(this);
		IConstructor type = stack.pop();

		switch (operator) {
		case ICPPASTCastExpression.op_cast:
			stack.push(builder.Expression_cast(type, operand, loc, typ));
			break;
		case ICPPASTCastExpression.op_dynamic_cast:
			stack.push(builder.Expression_dynamicCast(type, operand, loc, typ));
			break;
		case ICPPASTCastExpression.op_static_cast:
			stack.push(builder.Expression_staticCast(type, operand, loc, typ));
			break;
		case ICPPASTCastExpression.op_reinterpret_cast:
			stack.push(builder.Expression_reinterpretCast(type, operand, loc, typ));
			break;
		case ICPPASTCastExpression.op_const_cast:
			stack.push(builder.Expression_constCast(type, operand, loc, typ));
			break;
		default:
			throw new RuntimeException("Unknown cast type " + operator);
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTConditionalExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		IASTExpression _condition = expression.getLogicalConditionExpression();
		IASTExpression _positive = expression.getPositiveResultExpression();
		IASTExpression _negative = expression.getNegativeResultExpression();

		_condition.accept(this);
		IConstructor condition = stack.pop();
		_positive.accept(this);
		IConstructor positive = stack.pop();
		_negative.accept(this);
		IConstructor negative = stack.pop();

		stack.push(builder.Expression_conditional(condition, positive, negative, loc, typ));

		return PROCESS_ABORT;
	}

	public int visit(IASTExpressionList expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		IListWriter expressions = vf.listWriter();
		Stream.of(expression.getExpressions()).forEach(it -> {
			it.accept(this);
			expressions.append(stack.pop());
		});
		stack.push(builder.Expression_expressionList(expressions.done(), loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(IASTFieldReference expression) {
		ISourceLocation loc = getSourceLocation(expression);
		ISourceLocation decl = br.resolveBinding(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		if (expression instanceof ICPPASTFieldReference) {
			// TODO: check isTemplate
			ICPPASTFieldReference reference = (ICPPASTFieldReference) expression;
			IASTExpression _fieldOwner = reference.getFieldOwner();
			IASTName _fieldName = reference.getFieldName();

			_fieldOwner.accept(this);
			IConstructor fieldOwner = stack.pop();
			_fieldName.accept(this);
			IConstructor fieldName = stack.pop();
			if (expression.isPointerDereference())
				stack.push(builder.Expression_fieldReferencePointerDeref(fieldOwner, fieldName, loc, decl, typ));
			else
				stack.push(builder.Expression_fieldReference(fieldOwner, fieldName, loc, decl, typ));
		} else
			throw new RuntimeException("IASTFieldReference: NYI");
		return PROCESS_ABORT;
	}

	public int visit(IASTFunctionCallExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		IASTExpression _functionName = expression.getFunctionNameExpression();
		IASTInitializerClause[] _arguments = expression.getArguments();

		_functionName.accept(this);
		IConstructor functionName = stack.pop();
		IListWriter arguments = vf.listWriter();
		Stream.of(_arguments).forEach(it -> {
			it.accept(this);
			arguments.append(stack.pop());
		});
		stack.push(builder.Expression_functionCall(functionName, arguments.done(), loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(IASTIdExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		ISourceLocation decl = br.resolveBinding(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		expression.getName().accept(this);
		stack.push(builder.Expression_idExpression(stack.pop(), loc, decl, typ));
		return PROCESS_ABORT;
	}

	public int visit(IASTLiteralExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		int kind = expression.getKind();
		String value = new String(expression.getValue());// expression.toString();
		switch (kind) {
		case IASTLiteralExpression.lk_integer_constant:
			stack.push(builder.Expression_integerConstant(value, loc));
			break;
		case IASTLiteralExpression.lk_float_constant:
			stack.push(builder.Expression_floatConstant(value, loc));
			break;
		case IASTLiteralExpression.lk_char_constant:
			stack.push(builder.Expression_charConstant(value, loc));
			break;
		case IASTLiteralExpression.lk_string_literal:
			stack.push(builder.Expression_stringLiteral(value, loc));
			break;
		case IASTLiteralExpression.lk_this:
			stack.push(builder.Expression_this(loc));
			break;
		case IASTLiteralExpression.lk_true:
			stack.push(builder.Expression_true(loc));
			break;
		case IASTLiteralExpression.lk_false:
			stack.push(builder.Expression_false(loc));
			break;
		case IASTLiteralExpression.lk_nullptr:
			stack.push(builder.Expression_nullptr(loc));
			break;
		default:
			throw new RuntimeException("Encountered unknown literal kind " + kind + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTProblemExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IASTProblem problem = expression.getProblem();
		err("ProblemExpression " + expression.getRawSignature() + ":" + problem.getMessageWithLocation());
		stack.push(builder.Expression_problemExpression(loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTTypeIdInitializerExpression expression) {
		out("TypeIdInitializerExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(IASTTypeIdExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		int operator = expression.getOperator();
		expression.getTypeId().accept(this);
		switch (operator) {
		case IASTTypeIdExpression.op_sizeof:
			stack.push(builder.Expression_sizeof(stack.pop(), loc, typ));
			break;
		case IASTTypeIdExpression.op_typeid:
			stack.push(builder.Expression_typeid(stack.pop(), loc, typ));
			break;
		case IASTTypeIdExpression.op_alignof: // gnu-only?
			stack.push(builder.Expression_alignOf(stack.pop(), loc, typ));
			break;
		case IASTTypeIdExpression.op_sizeofParameterPack:
			stack.push(builder.Expression_sizeofParameterPack(stack.pop(), loc, typ));
			break;
		default:
			throw new RuntimeException(
					"ERROR: IASTTypeIdExpression called with unimplemented/unknown operator " + operator);
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTUnaryExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression.getExpressionType(), loc);
		int operator = expression.getOperator();
		IASTExpression _operand = expression.getOperand();

		IConstructor operand = null;
		if (_operand != null) {
			expression.getOperand().accept(this);
			operand = stack.pop();
		}

		switch (operator) {
		case IASTUnaryExpression.op_prefixIncr:
			stack.push(builder.Expression_prefixIncr(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_prefixDecr:
			stack.push(builder.Expression_prefixDecr(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_plus:
			stack.push(builder.Expression_plus(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_minus:
			stack.push(builder.Expression_minus(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_star:
			stack.push(builder.Expression_star(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_amper:
			stack.push(builder.Expression_amper(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_tilde:
			stack.push(builder.Expression_tilde(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_not:
			stack.push(builder.Expression_not(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_sizeof:
			stack.push(builder.Expression_sizeof(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_postFixIncr:
			stack.push(builder.Expression_postfixIncr(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_postFixDecr:
			stack.push(builder.Expression_postfixDecr(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_bracketedPrimary:
			stack.push(builder.Expression_bracketed(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_throw:
			if (operand == null)
				stack.push(builder.Expression_throw(loc, typ));
			else
				stack.push(builder.Expression_throw(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_typeid:
			stack.push(builder.Expression_typeid(operand, loc, typ));
			break;
		// case IASTUnaryExpression.op_typeof: (14) typeOf is deprecated
		case IASTUnaryExpression.op_alignOf:
			stack.push(builder.Expression_alignOf(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_sizeofParameterPack:
			stack.push(builder.Expression_sizeofParameterPack(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_noexcept:
			stack.push(builder.Expression_noexcept(operand, loc, typ));
			break;
		case IASTUnaryExpression.op_labelReference:
			stack.push(builder.Expression_labelReference(operand, loc, typ));
			break;
		default:
			throw new RuntimeException("Unknown unary operator " + operator + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTStatement statement) {
		// err("Statement: " + statement.getRawSignature() +
		// ", " + statement.getClass().getName());
		if (statement instanceof IASTAmbiguousStatement)
			visit((IASTAmbiguousStatement) statement);
		else if (statement instanceof IASTBreakStatement)
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
		else if (statement instanceof IASTIfExistsStatement)
			visit((IASTIfExistsStatement) statement);
		else if (statement instanceof IASTIfNotExistsStatement)
			visit((IASTIfNotExistsStatement) statement);
		else if (statement instanceof IASTTryExceptStatement)
			visit((IASTTryExceptStatement) statement);
		else if (statement instanceof IASTTryFinallyStatement)
			visit((IASTTryFinallyStatement) statement);
		else {
			throw new RuntimeException(
					"Statement: encountered non-implemented subtype " + statement.getClass().getName());
		}
		return PROCESS_ABORT;
	}

	public int visit(IGNUASTGotoStatement statement) {
		// requires decl keyword parameter
		err("IGNUAstGotoStatement: " + statement.getRawSignature());
		throw new RuntimeException("NYI");
	}

	public int visit(ICPPASTCatchHandler statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTStatement _catchBody = statement.getCatchBody();
		IASTDeclaration _declaration = statement.getDeclaration();

		_catchBody.accept(this);
		IConstructor catchBody = stack.pop();

		if (statement.isCatchAll())
			stack.push(builder.Statement_catchAll(attributes, catchBody, loc));
		else {
			_declaration.accept(this);
			stack.push(builder.Statement_catch(attributes, stack.pop(), catchBody, loc));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTRangeBasedForStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTDeclaration _declaration = statement.getDeclaration();
		IASTInitializerClause _initializerClause = statement.getInitializerClause();
		IASTStatement _body = statement.getBody();

		_declaration.accept(this);
		IConstructor declaration = stack.pop();
		_initializerClause.accept(this);
		IConstructor initializerClause = stack.pop();
		_body.accept(this);
		IConstructor body = stack.pop();

		stack.push(builder.Statement_rangeBasedFor(attributes, declaration, initializerClause, body, loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTryBlockStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTStatement _tryBody = statement.getTryBody();
		ICPPASTCatchHandler[] _catchHandlers = statement.getCatchHandlers();
		_tryBody.accept(this);
		IConstructor tryBody = stack.pop();
		IListWriter catchHandlers = vf.listWriter();
		Stream.of(_catchHandlers).forEach(it -> {
			it.accept(this);
			catchHandlers.append(stack.pop());
		});
		stack.push(builder.Statement_tryBlock(attributes, tryBody, catchHandlers.done(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTAmbiguousStatement statement) {
		out("visit(IASTAmbiguousStatement)");
		out(statement.getRawSignature());
		ISourceLocation loc = getSourceLocation(statement);
		IASTStatement[] _statements = statement.getStatements();
		IListWriter statements = vf.listWriter();
		prefix += 4;
		Stream.of(_statements).forEach(it -> {
			out("Statement " + it.getClass().getSimpleName() + ": " + it.getRawSignature());
			it.accept(this);
			statements.append(stack.pop());
		});
		prefix -= 4;
		throw new RuntimeException("Encountered Ambiguous statement");
	}

	public int visit(IASTBreakStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		stack.push(builder.Statement_break(attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTCaseStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTExpression _expression = statement.getExpression();
		_expression.accept(this);
		IConstructor expression = stack.pop();
		stack.push(builder.Statement_case(attributes, expression, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTCompoundStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTStatement[] _statements = statement.getStatements();
		IListWriter statements = vf.listWriter();
		Stream.of(_statements).forEach(it -> {
			it.accept(this);
			statements.append(stack.pop());
		});
		stack.push(builder.Statement_compoundStatement(attributes, statements.done(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTContinueStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		stack.push(builder.Statement_continue(attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTDeclarationStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTDeclaration _declaration = statement.getDeclaration();
		_declaration.accept(this);
		stack.push(builder.Statement_declarationStatement(attributes, stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTDefaultStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		stack.push(builder.Statement_defaultCase(attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTDoStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTStatement _body = statement.getBody();
		IASTExpression _condition = statement.getCondition();

		_body.accept(this);
		IConstructor body = stack.pop();
		_condition.accept(this);
		IConstructor condition = stack.pop();
		stack.push(builder.Statement_do(attributes, body, condition, loc));

		return PROCESS_ABORT;
	}

	public int visit(IASTExpressionStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTExpression _expression = statement.getExpression();
		_expression.accept(this);
		stack.push(builder.Statement_expressionStatement(attributes, stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTForStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTStatement _initializer = statement.getInitializerStatement();
		IASTExpression _condition = statement.getConditionExpression();
		IASTExpression _iteration = statement.getIterationExpression();
		IASTStatement _body = statement.getBody();

		IConstructor initializer;
		if (_initializer == null)
			initializer = builder.Expression_empty(loc);
		else {
			_initializer.accept(this);
			initializer = stack.pop();
		}
		IConstructor condition;
		if (_condition == null)
			condition = builder.Expression_empty(loc);
		else {
			_condition.accept(this);
			condition = stack.pop();
		}
		IConstructor iteration;
		if (_iteration == null)
			iteration = builder.Expression_empty(loc);
		else {
			_iteration.accept(this);
			iteration = stack.pop();
		}
		_body.accept(this);
		IConstructor body = stack.pop();

		if (statement instanceof ICPPASTForStatement) {
			IASTDeclaration _conditionDeclaration = ((ICPPASTForStatement) statement).getConditionDeclaration();
			if (_conditionDeclaration != null) {
				_conditionDeclaration.accept(this);
				stack.push(builder.Statement_forWithDecl(attributes, initializer, stack.pop(), iteration, body, loc));
				return PROCESS_ABORT;
			}
		}
		stack.push(builder.Statement_for(attributes, initializer, condition, iteration, body, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTGotoStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		ISourceLocation decl = br.resolveBinding(statement);
		IList attributes = getAttributes(statement);
		IASTName _name = statement.getName();
		_name.accept(this);
		stack.push(builder.Statement_goto(attributes, stack.pop(), loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(IASTIfExistsStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		statement.getCondition().accept(this);
		IConstructor condition = stack.pop();
		statement.getThenClause().accept(this);
		stack.push(builder.Statement_ifExists(attributes, condition, stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTIfNotExistsStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		statement.getCondition().accept(this);
		IConstructor condition = stack.pop();
		statement.getThenClause().accept(this);
		stack.push(builder.Statement_ifNotExists(attributes, condition, stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTTryExceptStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		statement.getTryBody().accept(this);
		IConstructor tryBody = stack.pop();
		statement.getExceptFilter().accept(this);
		IConstructor exceptFilter = stack.pop();
		statement.getExceptBody().accept(this);
		stack.push(builder.Statement_tryExcept(attributes, tryBody, exceptFilter, stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTTryFinallyStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		statement.getTryBody().accept(this);
		IConstructor tryBody = stack.pop();
		statement.getFinallyBody().accept(this);
		stack.push(builder.Statement_tryFinally(attributes, tryBody, stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTIfStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTExpression _condition = statement.getConditionExpression();
		IASTStatement _thenClause = statement.getThenClause();
		IASTStatement _elseClause = statement.getElseClause();

		if (_condition == null && statement instanceof ICPPASTIfStatement) {
			((ICPPASTIfStatement) statement).getConditionDeclaration().accept(this);
			IConstructor condition = stack.pop();
			_thenClause.accept(this);
			IConstructor thenClause = stack.pop();
			if (_elseClause == null) {
				stack.push(builder.Statement_ifWithDecl(attributes, condition, thenClause, loc));
			} else {
				_elseClause.accept(this);
				IConstructor elseClause = stack.pop();
				stack.push(builder.Statement_ifWithDecl(attributes, condition, thenClause, elseClause, loc));
			}
		} else {
			_condition.accept(this);
			IConstructor condition = stack.pop();
			_thenClause.accept(this);
			IConstructor thenClause = stack.pop();
			if (_elseClause == null) {
				stack.push(builder.Statement_if(attributes, condition, thenClause, loc));
			} else {
				_elseClause.accept(this);
				IConstructor elseClause = stack.pop();
				stack.push(builder.Statement_if(attributes, condition, thenClause, elseClause, loc));
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTLabelStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		ISourceLocation decl = br.resolveBinding(statement);
		IList attributes = getAttributes(statement);
		IASTName _name = statement.getName();
		IASTStatement _nestedStatement = statement.getNestedStatement();

		_name.accept(this);
		IConstructor name = stack.pop();
		_nestedStatement.accept(this);
		IConstructor nestedStatement = stack.pop();

		stack.push(builder.Statement_label(attributes, name, nestedStatement, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(IASTNullStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		stack.push(builder.Statement_nullStatement(attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTProblemStatement statement) {
		err("IASTProblemStatement:");
		prefix += 4;
		err(statement.getProblem().getMessageWithLocation());
		err(statement.getRawSignature());
		prefix -= 4;
		stack.push(builder.Statement_problem(statement.getRawSignature(), getSourceLocation(statement)));
		return PROCESS_ABORT;
	}

	public int visit(IASTReturnStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IASTExpression returnValue = statement.getReturnValue();
		IASTInitializerClause returnArgument = statement.getReturnArgument();
		if (returnValue == null && returnArgument == null)
			stack.push(builder.Statement_return(attributes, loc));
		else if (returnValue != null) {
			returnValue.accept(this);
			stack.push(builder.Statement_return(attributes, stack.pop(), loc));
		} else {
			returnArgument.accept(this);
			// Note: InitializerClause is currently mapped on Expression
			stack.push(builder.Statement_return(attributes, stack.pop(), loc));
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTSwitchStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);

		statement.getBody().accept(this);
		IConstructor body = stack.pop();

		IASTExpression _controller = statement.getControllerExpression();
		if (_controller == null && statement instanceof ICPPASTSwitchStatement) {
			((ICPPASTSwitchStatement) statement).getControllerDeclaration().accept(this);
			stack.push(builder.Statement_switchWithDecl(attributes, stack.pop(), body, loc));
			return PROCESS_ABORT;
		}

		_controller.accept(this);
		stack.push(builder.Statement_switch(attributes, stack.pop(), body, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTWhileStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);

		statement.getBody().accept(this);
		IConstructor body = stack.pop();

		IASTExpression _condition = statement.getCondition();
		if (_condition == null && statement instanceof ICPPASTWhileStatement) {
			((ICPPASTWhileStatement) statement).getConditionDeclaration().accept(this);
			stack.push(builder.Statement_whileWithDecl(attributes, stack.pop(), body, loc));
			return PROCESS_ABORT;
		}
		_condition.accept(this);
		stack.push(builder.Statement_while(attributes, stack.pop(), body, loc));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTTypeId typeId) {
		ISourceLocation loc = getSourceLocation(typeId);
		if (typeId instanceof IASTProblemTypeId) {
			if (typeId.getRawSignature().equals("...") || typeId.getRawSignature().contains("_THROW1("))
				stack.push(builder.Expression_typeId(
						builder.DeclSpecifier_msThrowEllipsis(loc, vf.sourceLocation("unknown:///")), loc));
			else {
				out("ProblemTypeId " + typeId.getClass().getSimpleName() + ": " + typeId.getRawSignature());
				throw new RuntimeException("IASTProblemTypeId encountered! "
						+ ((IASTProblemTypeId) typeId).getProblem().getMessageWithLocation());
			}
		} else {
			IASTDeclSpecifier _declSpecifier = typeId.getDeclSpecifier();
			IASTDeclarator _abstractDeclarator = typeId.getAbstractDeclarator();

			_declSpecifier.accept(this);
			IConstructor declSpecifier = stack.pop();
			_abstractDeclarator.accept(this);
			IConstructor abstractDeclarator = stack.pop();
			if (abstractDeclarator.has("name")) {
				abstractDeclarator = abstractDeclarator.set("name", builder.Expression_abstractEmptyName(loc));
				abstractDeclarator = abstractDeclarator.asWithKeywordParameters().unsetParameter("decl");
			}
			stack.push(builder.Expression_typeId(declSpecifier, abstractDeclarator, loc));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTEnumerator enumerator) {
		ISourceLocation loc = getSourceLocation(enumerator);
		ISourceLocation decl = br.resolveBinding(enumerator);
		IASTName _name = enumerator.getName();
		IASTExpression _value = enumerator.getValue();

		_name.accept(this);
		IConstructor name = stack.pop();
		if (_value == null)
			stack.push(builder.Declaration_enumerator(builder.Expression_name(name.get("value").toString(), loc), loc,
					decl));
		else {
			_value.accept(this);
			stack.push(builder.Declaration_enumerator(builder.Expression_name(name.get("value").toString(), loc),
					stack.pop(), loc, decl));
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
		ISourceLocation loc = getSourceLocation(baseSpecifier);
		ISourceLocation decl = br.resolveBinding(baseSpecifier);

		IListWriter modifiers = vf.listWriter();
		switch (baseSpecifier.getVisibility()) {
		case ICPPASTBaseSpecifier.v_public:
			modifiers.append(builder.Modifier_public(loc));
			break;
		case ICPPASTBaseSpecifier.v_protected:
			modifiers.append(builder.Modifier_protected(loc));
			break;
		case ICPPASTBaseSpecifier.v_private:
			modifiers.append(builder.Modifier_private(loc));
			break;
		case 0:
			modifiers.append(builder.Modifier_unspecifiedInheritance(loc));
			break;
		default:
			throw new RuntimeException("Unknown BaseSpecifier visibility code " + baseSpecifier.getVisibility());
		}
		if (baseSpecifier.isVirtual())
			modifiers.append(builder.Modifier_virtual(loc));

		ICPPASTNameSpecifier _nameSpecifier = baseSpecifier.getNameSpecifier();
		if (_nameSpecifier == null)
			stack.push(builder.Declaration_baseSpecifier(modifiers.done(), loc, decl));
		else {
			_nameSpecifier.accept(this);
			stack.push(builder.Declaration_baseSpecifier(modifiers.done(), stack.pop(), loc, decl));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTNamespaceDefinition namespaceDefinition) {
		ISourceLocation loc = getSourceLocation(namespaceDefinition);
		ISourceLocation decl = br.resolveBinding(namespaceDefinition);
		IList attributes = getAttributes(namespaceDefinition);
		IASTName _name = namespaceDefinition.getName();
		IASTDeclaration[] _declarations = namespaceDefinition.getDeclarations();

		_name.accept(this);
		IConstructor name = stack.pop();
		IListWriter declarations = vf.listWriter();
		Stream.of(_declarations).forEach(it -> {
			it.accept(this);
			declarations.append(stack.pop());
		});
		if (namespaceDefinition.isInline())
			stack.push(builder.Declaration_namespaceDefinitionInline(attributes, name, declarations.done(), loc, decl));
		else
			stack.push(builder.Declaration_namespaceDefinition(attributes, name, declarations.done(), loc, decl));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTTemplateParameter templateParameter) {
		ISourceLocation loc = getSourceLocation(templateParameter);
		boolean isParameterPack = templateParameter.isParameterPack();
		if (isParameterPack)
			err("WARNING: ICPPASTTemplateParameter has isParameterPack=true, unimplemented");
		if (templateParameter instanceof ICPPASTParameterDeclaration) {
			// TODO: duplicate, never reached, remove
			IASTDeclSpecifier _declSpecifier = ((ICPPASTParameterDeclaration) templateParameter).getDeclSpecifier();
			ICPPASTDeclarator _declarator = ((ICPPASTParameterDeclaration) templateParameter).getDeclarator();
			_declSpecifier.accept(this);
			IConstructor declSpecifier = stack.pop();
			if (_declarator == null)
				stack.push(builder.Declaration_parameter(declSpecifier, loc));
			else {
				_declarator.accept(this);
				IConstructor declarator = stack.pop();
				stack.push(builder.Declaration_parameter(declSpecifier, declarator, loc));
			}
		} else if (templateParameter instanceof ICPPASTSimpleTypeTemplateParameter) {
			ISourceLocation decl = br.resolveBinding((ICPPASTSimpleTypeTemplateParameter) templateParameter);
			ICPPASTSimpleTypeTemplateParameter parameter = (ICPPASTSimpleTypeTemplateParameter) templateParameter;
			parameter.getName().accept(this);
			IConstructor name = stack.pop();

			if (parameter.getDefaultType() != null) {
				parameter.getDefaultType().accept(this);
				switch (parameter.getParameterType()) {
				case ICPPASTSimpleTypeTemplateParameter.st_class:
					stack.push(builder.Declaration_sttClass(name, stack.pop(), loc, decl));
					break;
				case ICPPASTSimpleTypeTemplateParameter.st_typename:
					stack.push(builder.Declaration_sttTypename(name, stack.pop(), loc, decl));
					break;
				default:
					throw new RuntimeException("ICPPASTTemplateParameter encountered non-implemented parameter type "
							+ parameter.getParameterType());
				}
			} else {

				switch (parameter.getParameterType()) {
				case ICPPASTSimpleTypeTemplateParameter.st_class:
					stack.push(builder.Declaration_sttClass(name, loc, decl));
					break;
				case ICPPASTSimpleTypeTemplateParameter.st_typename:
					stack.push(builder.Declaration_sttTypename(name, loc, decl));
					break;
				default:
					throw new RuntimeException("ICPPASTTemplateParameter encountered non-implemented parameter type "
							+ parameter.getParameterType());
				}
			}
		} else if (templateParameter instanceof ICPPASTTemplatedTypeTemplateParameter) {
			ISourceLocation decl = br.resolveBinding((ICPPASTTemplatedTypeTemplateParameter) templateParameter);
			ICPPASTTemplateParameter[] _templateParameters = ((ICPPASTTemplatedTypeTemplateParameter) templateParameter)
					.getTemplateParameters();
			IListWriter templateParameters = vf.listWriter();
			Stream.of(_templateParameters).forEach(it -> {
				it.accept(this);
				templateParameters.append(stack.pop());
			});
			((ICPPASTTemplatedTypeTemplateParameter) templateParameter).getName().accept(this);
			stack.push(builder.Declaration_tttParameter(templateParameters.done(), stack.pop(), loc, decl));
			if (((ICPPASTTemplatedTypeTemplateParameter) templateParameter).getDefaultValue() != null)
				err("ICPPASTTemplatedTypeTemplateParameter has defaultType, unimplemented");
		} else
			throw new RuntimeException("ICPPASTTemplateParameter encountered unknown subtype "
					+ templateParameter.getClass().getName() + ". Exiting");
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTCapture capture) {
		// TODO: check isPackExpansion and capturesThisPointer
		ISourceLocation loc = getSourceLocation(capture);
		ISourceLocation decl = br.resolveBinding(capture);
		if (capture.capturesThisPointer())
			stack.push(builder.Expression_captureThisPtr(loc));
		else {
			capture.getIdentifier().accept(this);
			if (capture.isByReference())
				stack.push(builder.Expression_captureByRef(stack.pop(), loc, decl));
			else
				stack.push(builder.Expression_capture(stack.pop(), loc, decl));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICASTDesignator designator) {
		err("Designator: " + designator.getRawSignature());
		throw new RuntimeException("NYI");
	}

	@Override
	public int visit(ICPPASTDesignator designator) {
		ISourceLocation loc = getSourceLocation(designator);
		if (designator instanceof ICPPASTArrayDesignator) {
			((ICPPASTArrayDesignator) designator).getSubscriptExpression().accept(this);
			stack.push(builder.Expression_arrayDesignator(stack.pop(), loc));
		} else if (designator instanceof ICPPASTFieldDesignator) {
			((ICPPASTFieldDesignator) designator).getName().accept(this);
			stack.push(builder.Expression_fieldDesignator(stack.pop(), loc));
		} else if (designator instanceof IGPPASTArrayRangeDesignator) {
			ICPPASTExpression _rangeFloor = ((IGPPASTArrayRangeDesignator) designator).getRangeFloor();
			ICPPASTExpression _rangeCeiling = ((IGPPASTArrayRangeDesignator) designator).getRangeCeiling();
			_rangeFloor.accept(this);
			IConstructor rangeFloor = stack.pop();
			_rangeCeiling.accept(this);
			IConstructor rangeCeiling = stack.pop();
			stack.push(builder.Expression_arrayRangeDesignator(rangeFloor, rangeCeiling, loc));
		} else
			throw new RuntimeException("ICPPASTDesignator encountered unknown subclass, exiting");
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTVirtSpecifier virtSpecifier) {
		ISourceLocation loc = getSourceLocation(virtSpecifier);
		SpecifierKind kind = virtSpecifier.getKind();
		switch (kind) {
		case Final:
			stack.push(builder.Declaration_virtSpecifier(builder.Modifier_final(loc), loc));
			break;
		case Override:
			stack.push(builder.Declaration_virtSpecifier(builder.Modifier_override(loc), loc));
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
