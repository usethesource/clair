/** 
 * Copyright (c) 2016-2017, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
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

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.net.URISyntaxException;
import java.time.Duration;
import java.time.Instant;
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
import org.eclipse.cdt.core.dom.ast.ExpansionOverlapsBoundaryException;
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
import org.eclipse.cdt.core.dom.ast.IASTTypeId;
import org.eclipse.cdt.core.dom.ast.IASTTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTTypeIdInitializerExpression;
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression;
import org.eclipse.cdt.core.dom.ast.IASTWhileStatement;
import org.eclipse.cdt.core.dom.ast.IBinding;
import org.eclipse.cdt.core.dom.ast.c.ICASTArrayModifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignatedInitializer;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignator;
import org.eclipse.cdt.core.dom.ast.c.ICASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTAliasDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTAlignmentSpecifier;
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
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTypeIdExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTTypeTransformationSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUnaryExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDeclaration;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTUsingDirective;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVirtSpecifier;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTVisibilityLabel;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTWhileStatement;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPMethod;
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.IGPPASTArrayRangeDesignator;
import org.eclipse.cdt.core.index.IIndex;
import org.eclipse.cdt.core.index.IIndexFileLocation;
import org.eclipse.cdt.core.model.ILanguage;
import org.eclipse.cdt.core.parser.DefaultLogService;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.IParserLogService;
import org.eclipse.cdt.core.parser.IScannerInfo;
import org.eclipse.cdt.core.parser.IToken;
import org.eclipse.cdt.core.parser.IncludeFileContentProvider;
import org.eclipse.cdt.core.parser.ScannerInfo;
import org.eclipse.cdt.internal.core.dom.IIncludeFileResolutionHeuristics;
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode;
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode.NameCollector;
import org.eclipse.cdt.internal.core.dom.parser.IASTAmbiguousStatement;
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTCompoundStatementExpression;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ClassTypeHelper;
import org.eclipse.cdt.internal.core.index.CIndex;
import org.eclipse.cdt.internal.core.index.IIndexFragment;
import org.eclipse.cdt.internal.core.parser.IMacroDictionary;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContent;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContentProvider;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.utils.RuntimeExceptionFactory;
import org.rascalmpl.library.Prelude;
import org.rascalmpl.uri.URIUtil;

import io.usethesource.vallang.IBool;
import io.usethesource.vallang.IConstructor;
import io.usethesource.vallang.IList;
import io.usethesource.vallang.IListWriter;
import io.usethesource.vallang.IMap;
import io.usethesource.vallang.ISet;
import io.usethesource.vallang.ISetWriter;
import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IString;
import io.usethesource.vallang.ITuple;
import io.usethesource.vallang.IValue;
import io.usethesource.vallang.IValueFactory;
import io.usethesource.vallang.exceptions.FactParseError;
import io.usethesource.vallang.exceptions.FactTypeUseException;
import io.usethesource.vallang.io.StandardTextReader;

@SuppressWarnings("restriction")
public class Parser extends ASTVisitor {
	private IValueFactory vf;
	private AST builder;
	private IEvaluatorContext ctx;
	private Stack<IConstructor> stack = new Stack<IConstructor>();
	private BindingsResolver br = new BindingsResolver();
	private TypeResolver tr;

	boolean doProblemLogging = false;
	private boolean includeStdLib = false;
	private IList stdLib;

	private ISetWriter declaredType;

	public Parser(IValueFactory vf) {
		super(true);
		this.shouldVisitAmbiguousNodes = true;
		this.shouldVisitImplicitNames = true;
		this.includeInactiveNodes = true;
		this.shouldVisitTokens = true;

		this.vf = vf;
		this.builder = new AST(vf);
		this.tr = new TypeResolver(builder, vf);
		this.declaredType = vf.setWriter();
	}

	Map<String, Set<String>> dependencies = new HashMap<String, Set<String>>();

	private void addDependency(String from, String to) {
		if (!dependencies.containsKey(from))
			dependencies.put(from, new HashSet<String>());
		dependencies.get(from).add(to);
	}

	private IASTTranslationUnit getCdtAst(ISourceLocation file, IList includePath, IMap additionalMacros) {
		try {
			FileContent fc = FileContent.create(file.toString(),
					((IString) new Prelude(vf).readFile(file)).getValue().toCharArray());

			Map<String, String> macros = new HashMap<String, String>();
			Iterator<Entry<IValue, IValue>> it = additionalMacros.entryIterator();
			while (it.hasNext()) {
				Entry<IValue, IValue> entry = it.next();
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
			macros.put("_stdcall", "");

			macros.put("_USE_DECLSPECS_FOR_SAL", "0");
			macros.put("_DLL", "1");

			macros.put("NDEBUG", "");
			macros.put("WIN32", "");
			macros.put("_WINDOWS", "");
			macros.put("_WIN32_DCOM", "");
			macros.put("_USRDLL", "");
			macros.put("SSCF1_INCLUDED", "");
			macros.put("LOGGINGTRACING_INCLUDED", "");
			macros.put("_WINDLL", "");
			macros.put("_UNICODE", "");
			macros.put("UNICODE", "");
			macros.put("_AFXDLL", "");

			// macros.put("__INTELLISENSE__", "1");

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
					try {
						path.add(locToPath(URIUtil.getParentLocation(file)));
					} catch (IllegalArgumentException e) {
						// Do nothing, will not lookup parent of non-file loc
					}
					try {
						path.add(ResourcesPlugin.getWorkspace().getRoot().getProject("clair").getLocation().toString()
								+ "/includes");
					} catch (Throwable t) {
						ctx.getStdErr().println(
								"WARNING: ResourcesPlugin was null, can't get workspace; not overriding include files");
					}

					for (IValue include : includePath)
						path.add(locToPath((ISourceLocation) include));
				}

				private String locToPath(ISourceLocation loc) {
					if (!loc.getScheme().equals("file"))
						throw new IllegalArgumentException("Will not convert non-file loc");
					return loc.getAuthority() + loc.getPath();
				}

				String checkDirectory(File dir, String fileName) {
					if (!dir.isDirectory()) {
						return null;
					}
					for (File f : dir.listFiles()) {
						if (isRightFile(f.getName(), fileName)) {
							return f.getAbsolutePath();
						}
					}
					return null;
				}

				@Override
				public String findInclusion(String include, String currentFile) {
					include = include.trim().replace("\\", "/");
					String filePath = include.substring(0, include.lastIndexOf('/') + 1);
					String fileName = include.substring(include.lastIndexOf('/') + 1);
					String found = checkDirectory(new File(new File(currentFile).getParentFile(), filePath), fileName);
					if (found != null) {
						addDependency(currentFile, include);
						return found;
					}
					for (String path : path) {
						found = checkDirectory(new File(path, filePath), fileName);
						if (found != null) {
							addDependency(currentFile, include);
							return found;
						}
					}
					err("Include " + include + " for " + currentFile + " not found");
					return null;// TODO: restore exception here
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

			return GPPLanguage.getDefault().getASTTranslationUnit(fc, si, ifcp, idx, options, log);
		} catch (CoreException e) {
			throw RuntimeExceptionFactory.io(vf.string(e.getMessage()), null, null);
		} catch (StackOverflowError e) {
			throw RuntimeExceptionFactory.stackOverflow(null, null);
		} catch (Throwable e) {
			// TODO: make more specific
			throw RuntimeExceptionFactory.io(vf.string(e.getMessage()), null, null);
		}
	}

	public IValue parseCpp(ISourceLocation file, IList stdLib, IList includeDirs, IMap additionalMacros,
			IBool includeStdLib, IEvaluatorContext ctx) {
		setIEvaluatorContext(ctx);
		this.includeStdLib = includeStdLib.getValue() || stdLib.isEmpty();
		this.stdLib = stdLib;
		Instant begin = Instant.now();
		out("Beginning at " + begin.toString());
		IListWriter allIncludes = vf.listWriter();
		allIncludes.appendAll(includeDirs);
		allIncludes.appendAll(stdLib);
		IASTTranslationUnit tu = getCdtAst(file, allIncludes.done(), additionalMacros);
		Instant between = Instant.now();
		out("CDT took " + new Double(Duration.between(begin, between).toMillis()).doubleValue() / 1000 + "seconds");
		IValue result = convertCdtToRascal(tu);
		Instant done = Instant.now();
		out("Converting took " + new Double(Duration.between(between, done).toMillis()).doubleValue() / 1000
				+ "seconds");
		if (result == null) {
			throw RuntimeExceptionFactory.parseError(file, null, null);
		}
		return result;
	}

	public ITuple parseCppToM3AndAst(ISourceLocation file, IList stdLib, IList includeDirs, IMap additionalMacros,
			IBool includeStdLib, IEvaluatorContext ctx) {
		setIEvaluatorContext(ctx);
		this.includeStdLib = includeStdLib.getValue() || stdLib.isEmpty();
		this.stdLib = stdLib;
		IValue m3 = builder.M3_m3(file);
		IListWriter allIncludes = vf.listWriter();
		allIncludes.appendAll(includeDirs);
		allIncludes.appendAll(stdLib);
		IASTTranslationUnit tu = getCdtAst(file, allIncludes.done(), additionalMacros);
		IList comments = getCommentsFromTranslationUnit(tu);
		ISet macroExpansions = getMacroExpansionsFromTranslationUnit(tu);
		ISet macroDefinitions = getMacroDefinitionsFromTranslationUnit(tu);
		ISet methodOverrides = getMethodOverrides(tu);

		m3 = m3.asWithKeywordParameters().setParameter("comments", comments);
		m3 = m3.asWithKeywordParameters().setParameter("macroExpansions", macroExpansions);
		m3 = m3.asWithKeywordParameters().setParameter("macroDefinitions", macroDefinitions);
		m3 = m3.asWithKeywordParameters().setParameter("methodOverrides", methodOverrides);
		m3 = setM3IncludeInformationFromTranslationUnit(tu, m3);

		declaredType = vf.setWriter();
		IValue result = convertCdtToRascal(tu);
		m3 = m3.asWithKeywordParameters().setParameter("declaredType", declaredType.done());

		return vf.tuple(m3, result);
	}

	public ISet getMethodOverrides(IASTTranslationUnit tu) {
		NameCollector anc = new NameCollector();
		tu.accept(anc);
		Set<IBinding> bindings = new HashSet<>();
		Stream.of(anc.getNames()).forEach(it -> bindings.add(it.resolveBinding()));
		ISetWriter methodOverrides = vf.setWriter();
		bindings.stream().filter(ICPPMethod.class::isInstance).forEach(override -> {
			Stream.of(ClassTypeHelper.findOverridden((ICPPMethod) override)).forEach(base -> {
				try {
					methodOverrides.insert(vf.tuple(br.resolveBinding(base), br.resolveBinding(override)));
				} catch (FactTypeUseException | URISyntaxException e) {
					err("Got FactTypeUseException\n" + e.getMessage());
				}
			});
		});
		return methodOverrides.done();
	}

	public ISet getMacroDefinitionsFromTranslationUnit(IASTTranslationUnit tu) {
		ISetWriter macros = vf.setWriter();
		Stream.of(tu.getMacroDefinitions()).forEach(it -> {
			ISourceLocation decl;
			try {
				decl = br.resolveBinding(it.getName().resolveBinding());
			} catch (URISyntaxException e) {
				decl = vf.sourceLocation(URIUtil.rootScheme("null"));
			}
			macros.insert(vf.tuple(decl, getSourceLocation(it)));
		});
		return macros.done();
	}

	public IList getCommentsFromTranslationUnit(IASTTranslationUnit tu) {
		IListWriter comments = vf.listWriter();
		Stream.of(tu.getComments()).forEach(it -> comments.append(getSourceLocation(it)));
		return comments.done();
	}

	public IList parseForComments(ISourceLocation file, IList includePath, IMap additionalMacros,
			IEvaluatorContext ctx) {
		setIEvaluatorContext(ctx);
		IASTTranslationUnit tu = getCdtAst(file, includePath, additionalMacros);
		return getCommentsFromTranslationUnit(tu);
	}

	public IValue setM3IncludeInformationFromTranslationUnit(IASTTranslationUnit tu, IValue m3) {
		ISetWriter includeDirectives = vf.setWriter();
		ISetWriter inactiveIncludes = vf.setWriter();
		ISetWriter includeResolution = vf.setWriter();
		Stream.of(tu.getIncludeDirectives()).forEach(it -> {
			ISourceLocation include = vf.sourceLocation(URIUtil.rootScheme("unknown"));
			try {
				include = vf.sourceLocation(it.isSystemInclude() ? "cpp+systemInclude" : "cpp+include", null,
						it.getName().toString());
			} catch (URISyntaxException e) {
				// Shouldn't happen
			}
			if (it.isActive())
				includeDirectives.insert(vf.tuple(include, getSourceLocation(it)));
			else
				inactiveIncludes.insert(vf.tuple(include, getSourceLocation(it)));
			ISourceLocation path = "" == it.getPath() ? vf.sourceLocation(URIUtil.rootScheme("unresolved"))
					: vf.sourceLocation(it.getPath());
			includeResolution.insert(vf.tuple(include, path));
		});

		m3 = m3.asWithKeywordParameters().setParameter("includeDirectives", includeDirectives.done());
		m3 = m3.asWithKeywordParameters().setParameter("inactiveIncludes", inactiveIncludes.done());
		m3 = m3.asWithKeywordParameters().setParameter("includeResolution", includeResolution.done());
		return m3;
	}

	public ISet getMacroExpansionsFromTranslationUnit(IASTTranslationUnit tu) {
		ISetWriter macros = vf.setWriter();
		Stream.of(tu.getMacroExpansions()).forEach(it -> {
			ISourceLocation decl;
			try {
				decl = br.resolveBinding(it.getMacroReference().resolveBinding());
			} catch (URISyntaxException e) {
				decl = vf.sourceLocation(URIUtil.rootScheme("unknown"));
			}
			macros.insert(vf.tuple(getSourceLocation(it), decl));
		});
		return macros.done();
	}

	private void addDeclaredType(ISourceLocation decl, IConstructor typ) {
		declaredType.insert(vf.tuple(decl, typ));
	}

	public ISet parseForMacros(ISourceLocation file, IList includePath, IMap additionalMacros, IEvaluatorContext ctx) {
		setIEvaluatorContext(ctx);
		IASTTranslationUnit tu = getCdtAst(file, includePath, additionalMacros);
		return getMacroExpansionsFromTranslationUnit(tu);
	}

	public IValue parseString(IString code, IEvaluatorContext ctx) throws CoreException {
		return parseString(code, null, ctx);
	}

	public IValue parseString(IString code, ISourceLocation loc, IEvaluatorContext ctx) throws CoreException {
		setIEvaluatorContext(ctx);
		stdLib = vf.listWriter().done();
		FileContent fc = FileContent.create(loc == null ? "" : loc.toString(), code.getValue().toCharArray());
		IScannerInfo si = new ScannerInfo();
		IncludeFileContentProvider ifcp = IncludeFileContentProvider.getEmptyFilesProvider();
		int options = ILanguage.OPTION_PARSE_INACTIVE_CODE;
		IParserLogService log = new DefaultLogService();
		IASTTranslationUnit tu = GPPLanguage.getDefault().getASTTranslationUnit(fc, si, ifcp, null, options, log);
		tu.accept(this);
		return stack.pop();
	}

	public IValue convertCdtToRascal(IASTTranslationUnit translationUnit) {
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
			String fileName = astFileLocation.getFileName();
			fileName = fileName.replace('\\', '/');
			try {
				return vf.sourceLocation(
						(ISourceLocation) new StandardTextReader().read(vf, new StringReader(fileName)),
						astFileLocation.getNodeOffset(), astFileLocation.getNodeLength());
			} catch (FactParseError | FactTypeUseException | IOException e) {
			}
			if (!fileName.startsWith("/")) {
				fileName = "/" + fileName;
			}
			try {
				return vf.sourceLocation(
						(ISourceLocation) new StandardTextReader().read(vf, new StringReader(fileName)),
						astFileLocation.getNodeOffset(), astFileLocation.getNodeLength());
			} catch (FactParseError | FactTypeUseException | IOException e) {
			}
			return vf.sourceLocation(vf.sourceLocation(fileName), astFileLocation.getNodeOffset(),
					astFileLocation.getNodeLength());
		}
		return vf.sourceLocation(URIUtil.assumeCorrect("unknown:///", "", ""));
	}

	public ISourceLocation getTokenSourceLocation(IASTNode node, String literal) {
		ISourceLocation loc = getSourceLocation(node);
		try {
			IToken tokens = node.getSyntax();
			while (tokens != null) {
				if (literal.equals(tokens.getImage())) {
					return vf.sourceLocation(loc, loc.getOffset() + tokens.getOffset(), literal.length());
				}
				tokens = tokens.getNext();
			}
		} catch (ExpansionOverlapsBoundaryException e) {
			// Fall back to node's source location. Possibly find string in node's image
		}
		return loc;
	}

	IList getAttributes(IASTAttributeOwner node) {
		IListWriter attributeSpecifiers = vf.listWriter();
		Stream.of(node.getAttributeSpecifiers()).forEach(it -> {
			visit((IASTAttributeSpecifier) it);
			attributeSpecifiers.append(stack.pop());
		});
		return attributeSpecifiers.done();
	}

	IList getModifiers(IASTNode node) {
		IListWriter modifiers = vf.listWriter();

		if (node instanceof ICPPASTDeclSpecifier) {
			if (((ICPPASTDeclSpecifier) node).isFriend())
				modifiers.append(builder.Modifier_friend(getTokenSourceLocation(node, "friend")));
			if (((ICPPASTDeclSpecifier) node).isVirtual())
				modifiers.append(builder.Modifier_virtual(getTokenSourceLocation(node, "virtual")));
			if (((ICPPASTDeclSpecifier) node).isExplicit())
				modifiers.append(builder.Modifier_explicit(getTokenSourceLocation(node, "explicit")));
			if (((ICPPASTDeclSpecifier) node).isConstexpr())
				modifiers.append(builder.Modifier_constexpr(getTokenSourceLocation(node, "constexpr")));
			if (((ICPPASTDeclSpecifier) node).isThreadLocal())
				modifiers.append(builder.Modifier_threadLocal(getTokenSourceLocation(node, "thread_local")));
		}

		if (node instanceof ICPPASTFunctionDeclarator) {
			if (((ICPPASTFunctionDeclarator) node).isMutable())
				modifiers.append(builder.Modifier_mutable(getTokenSourceLocation(node, "mutable")));
			if (((ICPPASTFunctionDeclarator) node).isPureVirtual())
				modifiers.append(builder.Modifier_pureVirtual(getTokenSourceLocation(node, "virtual")));// check
		}

		if (node instanceof IASTDeclSpecifier) {
			switch (((IASTDeclSpecifier) node).getStorageClass()) {
			case IASTDeclSpecifier.sc_typedef:
				modifiers.append(builder.Modifier_typedef(getTokenSourceLocation(node, "typedef")));
				break;
			case IASTDeclSpecifier.sc_extern:
				modifiers.append(builder.Modifier_extern(getTokenSourceLocation(node, "extern")));
				break;
			case IASTDeclSpecifier.sc_static:
				modifiers.append(builder.Modifier_static(getTokenSourceLocation(node, "static")));
				break;
			case IASTDeclSpecifier.sc_auto:
				modifiers.append(builder.Modifier_modAuto(getTokenSourceLocation(node, "auto")));
				break;
			case IASTDeclSpecifier.sc_register:
				modifiers.append(builder.Modifier_register(getTokenSourceLocation(node, "register")));
				break;
			case IASTDeclSpecifier.sc_mutable:
				modifiers.append(builder.Modifier_mutable(getTokenSourceLocation(node, "mutable")));
				break;
			}
		}

		if (node instanceof IASTSimpleDeclSpecifier) {
			if (((IASTSimpleDeclSpecifier) node).isSigned())
				modifiers.append(builder.Modifier_signed(getTokenSourceLocation(node, "signed")));
			if (((IASTSimpleDeclSpecifier) node).isUnsigned())
				modifiers.append(builder.Modifier_unsigned(getTokenSourceLocation(node, "unsigned")));
			if (((IASTSimpleDeclSpecifier) node).isShort())
				modifiers.append(builder.Modifier_short(getTokenSourceLocation(node, "short")));
			if (((IASTSimpleDeclSpecifier) node).isLong())
				modifiers.append(builder.Modifier_long(getTokenSourceLocation(node, "long")));
			if (((IASTSimpleDeclSpecifier) node).isLongLong())
				modifiers.append(builder.Modifier_longlong(getTokenSourceLocation(node, "long long")));
			if (((IASTSimpleDeclSpecifier) node).isComplex())
				modifiers.append(builder.Modifier_complex(getTokenSourceLocation(node, "_Complex")));
			if (((IASTSimpleDeclSpecifier) node).isImaginary())
				modifiers.append(builder.Modifier_imaginary(getTokenSourceLocation(node, "_Imaginary")));
		}

		if (node instanceof ICASTArrayModifier) {
			if (((ICASTArrayModifier) node).isConst())
				modifiers.append(builder.Modifier_const(getTokenSourceLocation(node, "const")));
			if (((ICASTArrayModifier) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(getTokenSourceLocation(node, "volatile")));
			if (((ICASTArrayModifier) node).isRestrict())
				modifiers.append(builder.Modifier_restrict(getTokenSourceLocation(node, "restrict")));
		} else if (node instanceof ICPPASTFunctionDeclarator) {
			if (((ICPPASTFunctionDeclarator) node).isConst())
				modifiers.append(builder.Modifier_const(getTokenSourceLocation(node, "const")));
			if (((ICPPASTFunctionDeclarator) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(getTokenSourceLocation(node, "volatile")));
		} else if (node instanceof IASTDeclSpecifier) {
			if (((IASTDeclSpecifier) node).isConst())
				modifiers.append(builder.Modifier_const(getTokenSourceLocation(node, "const")));
			if (((IASTDeclSpecifier) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(getTokenSourceLocation(node, "volatile")));
			if (((IASTDeclSpecifier) node).isRestrict())
				modifiers.append(builder.Modifier_restrict(getTokenSourceLocation(node, "restrict")));
			if (((IASTDeclSpecifier) node).isInline())
				modifiers.append(builder.Modifier_inline(getTokenSourceLocation(node, "inline")));
		} else if (node instanceof IASTPointer) {
			if (((IASTPointer) node).isConst())
				modifiers.append(builder.Modifier_const(getTokenSourceLocation(node, "const")));
			if (((IASTPointer) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(getTokenSourceLocation(node, "volatile")));
			if (((IASTPointer) node).isRestrict())
				modifiers.append(builder.Modifier_restrict(getTokenSourceLocation(node, "restrict")));
		} else if (node instanceof ICPPASTNamespaceDefinition) {
			if (((ICPPASTNamespaceDefinition) node).isInline())
				modifiers.append(builder.Modifier_inline(getTokenSourceLocation(node, "inline")));
		}

		if (node instanceof ICPPASTNamedTypeSpecifier) {
			if (((ICPPASTNamedTypeSpecifier) node).isTypename())
				modifiers.append(builder.Modifier_typename(getTokenSourceLocation(node, "typename")));
		} else if (node instanceof ICPPASTUsingDeclaration)
			if (((ICPPASTUsingDeclaration) node).isTypename())
				modifiers.append(builder.Modifier_typename(getTokenSourceLocation(node, "typename")));

		return modifiers.done().stream()
				.sorted((v1, v2) -> ((ISourceLocation) v1.asWithKeywordParameters().getParameter("src")).getOffset()
						- ((ISourceLocation) v2.asWithKeywordParameters().getParameter("src")).getOffset())
				.collect(vf.listWriter());
	}

	@Override
	public int visit(IASTTranslationUnit tu) {
		ISourceLocation loc = getSourceLocation(tu);
		IListWriter declarations = vf.listWriter();
		declLoop: for (IASTDeclaration declaration : tu.getDeclarations()) {
			if (ctx.isInterrupted()) {
				declarations.append(builder
						.Declaration_problemDeclaration(vf.sourceLocation(URIUtil.assumeCorrect("interrupted:///"))));
				break;
			}
			ISourceLocation declLoc = getSourceLocation(declaration);
			if (!includeStdLib) {
				for (IValue it : stdLib) {
					ISourceLocation l = (ISourceLocation) it;
					if (l.getScheme().equals(declLoc.getScheme()) && declLoc.getPath().startsWith(l.getPath())) {
						continue declLoop;
					}
				}
			}
			declaration.accept(this);
			declarations.append(stack.pop());
		}

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
			throw new RuntimeException("NYI at " + getSourceLocation(name));
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTImplicitName name) {
		err("IASTImplicitName " + name.getRawSignature());
		boolean alternate = name.isAlternate();
		boolean operator = name.isOperator();
		IASTName _lastName = name.getLastName();
		throw new RuntimeException("NYI at " + getSourceLocation(name));
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
			stack.push(builder.Name_name(new String(name.toCharArray()), loc));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTConversionName name) {
		ISourceLocation loc = getSourceLocation(name);
		IConstructor typ = tr.resolveType(name);
		name.getTypeId().accept(this);
		stack.push(builder.Name_conversionName(name.toString(), stack.pop(), loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTOperatorName name) {
		ISourceLocation loc = getSourceLocation(name);
		stack.push(builder.Name_operatorName(name.toString(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTQualifiedName name) {
		ISourceLocation loc = getSourceLocation(name);
		ISourceLocation decl = br.resolveBinding(name);

		IListWriter qualifier = vf.listWriter();
		Stream.of(name.getQualifier()).forEach(it -> {
			it.accept(this);
			qualifier.append(stack.pop());
		});

		name.getLastName().accept(this);
		IConstructor lastName = stack.pop();
		// TODO: check fullyQualified
		if (name.isFullyQualified())
			;
		// err("WARNING: ICPPASTQualifiedName has fullyQualified=true");
		stack.push(builder.Name_qualifiedName(qualifier.done(), lastName, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTemplateId name) {
		ISourceLocation loc = getSourceLocation(name);
		ISourceLocation decl = br.resolveBinding(name);

		name.getTemplateName().accept(this);
		IConstructor templateName = stack.pop();
		IListWriter templateArguments = vf.listWriter();
		Stream.of(name.getTemplateArguments()).forEach(it -> {
			it.accept(this);
			templateArguments.append(stack.pop());
		});
		stack.push(builder.Name_templateId(templateName, templateArguments.done(), loc, decl));
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
			throw new RuntimeException("Declaration: encountered non-implemented subtype "
					+ declaration.getClass().getName() + " at " + getSourceLocation(declaration));
		}

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTAliasDeclaration declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		ISourceLocation decl = br.resolveBinding(declaration);
		IConstructor typ = tr.resolveType(declaration);
		IList attributes = getAttributes(declaration);

		declaration.getAlias().accept(this);
		IConstructor alias = stack.pop();
		declaration.getMappingTypeId().accept(this);
		IConstructor mappingTypeId = stack.pop();
		stack.push(builder.Declaration_alias(alias, mappingTypeId, attributes, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTExplicitTemplateInstantiation declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		declaration.getDeclaration().accept(this);
		switch (declaration.getModifier()) {
		case 0:
			stack.push(builder.Declaration_explicitTemplateInstantiation(stack.pop(), loc));
			break;
		case ICPPASTExplicitTemplateInstantiation.STATIC:
			stack.push(builder.Declaration_explicitTemplateInstantiation(
					builder.Modifier_static(getTokenSourceLocation(declaration, "static")), stack.pop(), loc));
			break;
		case ICPPASTExplicitTemplateInstantiation.INLINE:
			stack.push(builder.Declaration_explicitTemplateInstantiation(
					builder.Modifier_inline(getTokenSourceLocation(declaration, "inline")), stack.pop(), loc));
			break;
		case ICPPASTExplicitTemplateInstantiation.EXTERN:
			stack.push(builder.Declaration_explicitTemplateInstantiation(
					builder.Modifier_extern(getTokenSourceLocation(declaration, "extern")), stack.pop(), loc));
			break;
		default:
			throw new RuntimeException("ICPPASTExplicitTemplateInstantiation encountered unknown modifier "
					+ declaration.getModifier() + " at " + getSourceLocation(declaration));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTLinkageSpecification declaration) {
		ISourceLocation loc = getSourceLocation(declaration);

		IListWriter declarations = vf.listWriter();
		Stream.of(declaration.getDeclarations()).forEach(it -> {
			it.accept(this);
			declarations.append(stack.pop());
		});
		stack.push(builder.Declaration_linkageSpecification(declaration.getLiteral(), declarations.done(), loc));
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
		IConstructor typ = tr.resolveType(declaration);
		// The "export" keyword has been removed from the C++ standard
		IListWriter templateParameters = vf.listWriter();
		Stream.of(declaration.getTemplateParameters()).forEach(it -> {
			it.accept(this);
			templateParameters.append(stack.pop());
		});
		declaration.getDeclaration().accept(this);
		stack.push(builder.Declaration_template(templateParameters.done(), stack.pop(), typ, loc));
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
		stack.push(builder.Declaration_usingDeclaration(modifiers, stack.pop(), attributes, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTUsingDirective declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		ISourceLocation decl = br.resolveBinding(declaration);
		IList attributes = getAttributes(declaration);
		IASTName qualifiedName = declaration.getQualifiedName();
		qualifiedName.accept(this);
		stack.push(builder.Declaration_usingDirective(stack.pop(), attributes, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTVisibilityLabel declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		switch (declaration.getVisibility()) {
		case ICPPASTVisibilityLabel.v_public:
			stack.push(builder.Declaration_visibilityLabel(
					builder.Modifier_public(getTokenSourceLocation(declaration, "public")), loc));
			break;
		case ICPPASTVisibilityLabel.v_protected:
			stack.push(builder.Declaration_visibilityLabel(
					builder.Modifier_protected(getTokenSourceLocation(declaration, "protected")), loc));
			break;
		case ICPPASTVisibilityLabel.v_private:
			stack.push(builder.Declaration_visibilityLabel(
					builder.Modifier_private(getTokenSourceLocation(declaration, "private")), loc));
			break;
		default:
			throw new RuntimeException("Unknown CPPVisibilityLabel code " + declaration.getVisibility() + " at "
					+ getSourceLocation(declaration) + ". Exiting");
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
			boolean isDefaulted = ((ICPPASTFunctionDefinition) definition).isDefaulted();
			boolean isDeleted = ((ICPPASTFunctionDefinition) definition).isDeleted();

			definition.getDeclSpecifier().accept(this);
			IConstructor declSpecifier = stack.pop();
			definition.getDeclarator().accept(this);
			IConstructor declarator = stack.pop();

			IListWriter memberInitializers = vf.listWriter();
			Stream.of(((ICPPASTFunctionDefinition) definition).getMemberInitializers()).forEach(it -> {
				it.accept(this);
				memberInitializers.append(stack.pop());
			});

			if (isDefaulted && isDeleted)
				err("WARNING: IASTFunctionDefinition both deleted and defaulted");
			if ((isDefaulted || isDeleted) && definition instanceof ICPPASTFunctionWithTryBlock)
				throw new RuntimeException("IASTFunctionDefinition defaulted/deleted and with try? at " + loc);
			if (isDefaulted)
				stack.push(builder.Declaration_defaultedFunctionDefinition(declSpecifier, memberInitializers.done(),
						declarator, attributes, loc));
			else if (isDeleted)
				stack.push(builder.Declaration_deletedFunctionDefinition(declSpecifier, memberInitializers.done(),
						declarator, attributes, loc));
			else if (definition instanceof ICPPASTFunctionWithTryBlock) {
				IListWriter catchHandlers = vf.listWriter();
				Stream.of(((ICPPASTFunctionWithTryBlock) definition).getCatchHandlers()).forEach(it -> {
					it.accept(this);
					catchHandlers.append(stack.pop());
				});
				definition.getBody().accept(this);
				stack.push(builder.Declaration_functionWithTryBlockDefinition(declSpecifier, declarator,
						memberInitializers.done(), stack.pop(), catchHandlers.done(), attributes, loc));
			} else {
				definition.getBody().accept(this);
				stack.push(builder.Declaration_functionDefinition(declSpecifier, declarator, memberInitializers.done(),
						stack.pop(), attributes, loc));
			}
			addDeclaredType(br.resolveBinding(definition.getDeclarator()), tr.resolveType(definition.getDeclarator()));
		} else { // C Function definition
			if (true)
				throw new RuntimeException("Encountered C function definition at " + loc + ", NYI");
//			definition.getDeclSpecifier().accept(this);
//			IConstructor declSpecifier = stack.pop();
//			definition.getDeclarator().accept(this);
//			IConstructor declarator = stack.pop();
//			definition.getBody().accept(this);
//			IConstructor body = stack.pop();
//			stack.push(builder.Declaration_functionDefinition(declSpecifier, declarator, body, loc));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTParameterDeclaration parameterDeclaration) {
		ISourceLocation loc = getSourceLocation(parameterDeclaration);
		// TODO: remove duplicate code
		if (parameterDeclaration instanceof ICPPASTParameterDeclaration) {
			// TODO: add isParameterPack()
			ICPPASTParameterDeclaration declaration = (ICPPASTParameterDeclaration) parameterDeclaration;

			declaration.getDeclSpecifier().accept(this);
			IConstructor declSpecifier = stack.pop();
			if (declaration.getDeclarator() == null)
				stack.push(builder.Declaration_parameter(declSpecifier, loc));
			else {
				declaration.getDeclarator().accept(this);
				stack.push(builder.Declaration_parameter(declSpecifier, stack.pop(), loc));
			}
		} else {
			parameterDeclaration.getDeclSpecifier().accept(this);
			IConstructor declSpecifier = stack.pop();
			if (parameterDeclaration.getDeclarator() == null)
				stack.push(builder.Declaration_parameter(declSpecifier, loc));
			else {
				parameterDeclaration.getDeclarator().accept(this);
				stack.push(builder.Declaration_parameter(declSpecifier, stack.pop(), loc));
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTProblemDeclaration declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		IASTProblem problem = declaration.getProblem();
		String raw = declaration.getRawSignature();
		if (doProblemLogging) {
			if (!(raw.contains("$fail$") || raw.contains("�") || raw.contains("__int64(24)")
					|| raw.contains("CString default"))) {
				err("ProblemDeclaration: ");
				prefix += 4;
				err(Integer.toHexString(problem.getID()) + ": " + problem.getMessageWithLocation() + ", " + loc);
				err(raw);
				prefix -= 4;
			}
		}
		stack.push(builder.Declaration_problemDeclaration(loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTSimpleDeclaration declaration) {
		ISourceLocation loc = getSourceLocation(declaration);
		IList attributes = getAttributes(declaration);

		declaration.getDeclSpecifier().accept(this);
		IConstructor declSpecifier = stack.pop();

		IListWriter declarators = vf.listWriter();
		Stream.of(declaration.getDeclarators()).forEach(it -> {
			it.accept(this);
			declarators.append(stack.pop());
			addDeclaredType(br.resolveBinding(it), tr.resolveType(it));
		});
		stack.push(builder.Declaration_simpleDeclaration(declSpecifier, declarators.done(), attributes, loc));
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
			throw new RuntimeException("Initializer: encountered unknown subtype "
					+ initializer.getClass().getSimpleName() + " at " + getSourceLocation(initializer));
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTEqualsInitializer initializer) {
		ISourceLocation loc = getSourceLocation(initializer);
		initializer.getInitializerClause().accept(this);
		stack.push(builder.Expression_equalsInitializer(stack.pop(), loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTInitializerList initializer) {
		// TODO: cpp: check isPackExpansion, maybe getSize
		ISourceLocation loc = getSourceLocation(initializer);
		IListWriter clauses = vf.listWriter();
		Stream.of(initializer.getClauses()).forEach(it -> {
			it.accept(this);
			clauses.append(stack.pop());
		});
		stack.push(builder.Expression_initializerList(clauses.done(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICASTDesignatedInitializer initializer) {
		err("ICASTDesignatedInitializer: " + initializer.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(initializer));
	}

	public int visit(ICPPASTConstructorChainInitializer initializer) {
		// TODO: check isPackExpansion
		ISourceLocation loc = getSourceLocation(initializer);
		ISourceLocation decl = br.resolveBinding(initializer);

		initializer.getMemberInitializerId().accept(this);
		IConstructor memberInitializerId = stack.pop();
		initializer.getInitializer().accept(this);
		IConstructor memberInitializer = stack.pop();

		stack.push(builder.Expression_constructorChainInitializer(memberInitializerId, memberInitializer, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTConstructorInitializer initializer) {
		ISourceLocation loc = getSourceLocation(initializer);

		IListWriter arguments = vf.listWriter();
		Stream.of(initializer.getArguments()).forEach(it -> {
			it.accept(this);
			arguments.append(stack.pop());
		});

		stack.push(builder.Expression_constructorInitializer(arguments.done(), loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTDesignatedInitializer initializer) {
		ISourceLocation loc = getSourceLocation(initializer);

		IListWriter designators = vf.listWriter();
		Stream.of(initializer.getDesignators()).forEach(it -> {
			it.accept(this);
			designators.append(stack.pop());
		});

		initializer.getOperand().accept(this);
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
					"Unknown IASTInitializerClause subclass " + initializerClause.getClass().getName() + " at "
							+ getSourceLocation(initializerClause) + ". Exiting");
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTInitializerClause initializer) {
		err("ICPPASTInitializerClause: " + initializer.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(initializer));
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
			List<IConstructor> pointerOperators = new ArrayList<IConstructor>();
			Stream.of(declarator.getPointerOperators()).forEach(it -> {
				it.accept(this);
				pointerOperators.add(stack.pop());
			});
			IConstructor nestedDeclarator = null;
			if (declarator.getNestedDeclarator() != null) {
				declarator.getNestedDeclarator().accept(this);
				nestedDeclarator = stack.pop();
			}
			declarator.getName().accept(this);
			IConstructor name = stack.pop();
			IConstructor initializer = null;
			if (declarator.getInitializer() == null) {

			} else {
				declarator.getInitializer().accept(this);
				initializer = stack.pop();
			}

			throw new RuntimeException("NYI at " + getSourceLocation(declarator));
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

		if (declarator.getNestedDeclarator() == null) {
			if (declarator.getInitializer() == null)
				stack.push(builder.Declarator_arrayDeclarator(pointerOperators.done(), name, arrayModifiers.done(),
						attributes, loc, decl));
			else {
				declarator.getInitializer().accept(this);
				stack.push(builder.Declarator_arrayDeclarator(pointerOperators.done(), name, arrayModifiers.done(),
						stack.pop(), attributes, loc, decl));
			}
		} else {
			declarator.getNestedDeclarator().accept(this);
			IConstructor nestedDeclarator = stack.pop();
			if (declarator.getInitializer() == null)
				stack.push(builder.Declarator_arrayDeclaratorNested(pointerOperators.done(), nestedDeclarator,
						arrayModifiers.done(), attributes, loc, decl));
			else {
				declarator.getInitializer().accept(this);
				stack.push(builder.Declarator_arrayDeclaratorNested(pointerOperators.done(), nestedDeclarator,
						arrayModifiers.done(), stack.pop(), attributes, loc, decl));
			}
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTFieldDeclarator declarator) {
		// TODO: implement
		err("FieldDeclarator: " + declarator.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(declarator));
	}

	public int visit(IASTFunctionDeclarator declarator) {
		if (declarator instanceof IASTStandardFunctionDeclarator)
			visit((IASTStandardFunctionDeclarator) declarator);
		else if (declarator instanceof ICASTKnRFunctionDeclarator)
			visit((ICASTKnRFunctionDeclarator) declarator);
		else
			throw new RuntimeException("Unknown FunctionDeclarator subtype " + declarator.getClass().getName() + " at "
					+ getSourceLocation(declarator) + ". Exiting");
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

			if (declarator.takesVarArgs())
				err("WARNING: IASTStandardFunctionDeclarator has takesVarArgs=true");

			declarator.getName().accept(this);
			IConstructor name = stack.pop();

			IListWriter pointerOperators = vf.listWriter();
			Stream.of(declarator.getPointerOperators()).forEach(it -> {
				it.accept(this);
				pointerOperators.append(stack.pop());
			});

			IListWriter parameters = vf.listWriter();
			Stream.of(declarator.getParameters()).forEach(it -> {
				it.accept(this);
				parameters.append(stack.pop());
			});

			stack.push(builder.Declarator_functionDeclarator(pointerOperators.done(), name, parameters.done(),
					attributes, loc, decl));
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

		throw new RuntimeException("NYI at " + getSourceLocation(declarator));
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

			// if (declarator.getNestedDeclarator() != null)
			// err("WARNING: ICPPASTDeclarator has nestedDeclarator " +
			// _nestedDeclarator.getRawSignature());

			declarator.getName().accept(this);
			IConstructor name = stack.pop();

			IListWriter pointerOperators = vf.listWriter();
			Stream.of(declarator.getPointerOperators()).forEach(it -> {
				it.accept(this);
				pointerOperators.append(stack.pop());
			});

			IASTInitializer initializer = declarator.getInitializer();
			if (initializer == null) {
				stack.push(builder.Declarator_declarator(pointerOperators.done(), name, attributes, loc, decl));
			} else {
				initializer.accept(this);
				stack.push(builder.Declarator_declarator(pointerOperators.done(), name, stack.pop(), attributes, loc,
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
		declarator.getName().accept(this);
		IConstructor name = stack.pop();

		IASTDeclarator nestedDeclarator = declarator.getNestedDeclarator();
		if (nestedDeclarator != null)
			err("WARNING: ICPPASTDeclarator has nestedDeclarator " + nestedDeclarator.getRawSignature());

		IASTInitializer initializer = declarator.getInitializer();
		if (initializer == null) {
			stack.push(builder.Declarator_fieldDeclarator(pointerOperators.done(), name, bitFieldSize, attributes, loc,
					decl));
		} else {
			initializer.accept(this);
			stack.push(builder.Declarator_fieldDeclarator(pointerOperators.done(), name, bitFieldSize, stack.pop(),
					attributes, loc, decl));
		}

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTFunctionDeclarator declarator) {
		// TODO: check refQualifier and declaresParameterPack
		ISourceLocation loc = getSourceLocation(declarator);
		ISourceLocation decl = br.resolveBinding(declarator);
//		IConstructor typ = tr.resolveType(declarator);
		IList attributes = getAttributes(declarator);
		IList modifiers = getModifiers(declarator);

		IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
		IASTInitializer _initializer = declarator.getInitializer();
		IASTTypeId _trailingReturnType = declarator.getTrailingReturnType();
		IASTTypeId[] _exceptionSpecification = declarator.getExceptionSpecification();
		ICPPASTExpression _noexceptExpression = declarator.getNoexceptExpression();

		// TODO: fix when name == null
		IConstructor name = builder.Name_name("", vf.sourceLocation(loc, loc.getOffset(), 0));
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
			parameters.append(builder.Declaration_varArgs(getTokenSourceLocation(declarator, "...")));

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
				throw new RuntimeException("FunctionDeclarator: Trailing return type and nested declarator? at " + loc);
			_nestedDeclarator.accept(this);
			IConstructor nestedDeclarator = stack.pop();
			if (_initializer == null)
				stack.push(builder.Declarator_functionDeclaratorNested(pointerOperators.done(), modifiers,
						nestedDeclarator, parameters.done(), virtSpecifiers.done(), attributes, loc, decl));
			else {
				_initializer.accept(this);
				stack.push(builder.Declarator_functionDeclaratorNested(pointerOperators.done(), modifiers,
						nestedDeclarator, parameters.done(), virtSpecifiers.done(), stack.pop(), attributes, loc,
						decl));
			}
			// if
			// (!(_exceptionSpecification.equals(ICPPASTFunctionDeclarator.NO_EXCEPTION_SPECIFICATION)))
			// err("WARNING: ICPPASTFunctionDeclaration had nestedDeclarator and
			// also exceptionSpecification");
		} else if (_exceptionSpecification.equals(ICPPASTFunctionDeclarator.NO_EXCEPTION_SPECIFICATION)) {
			if (_trailingReturnType == null)
				stack.push(builder.Declarator_functionDeclarator(pointerOperators.done(), modifiers, name,
						parameters.done(), virtSpecifiers.done(), attributes, loc, decl));
			else {
				_trailingReturnType.accept(this);
				stack.push(builder.Declarator_functionDeclarator(pointerOperators.done(), modifiers, name,
						parameters.done(), virtSpecifiers.done(), stack.pop(), attributes, loc, decl));
			}
		} else if (_exceptionSpecification.equals(IASTTypeId.EMPTY_TYPEID_ARRAY)) {
			if (_trailingReturnType != null)
				throw new RuntimeException(
						"FunctionDeclarator: Trailing return type and exception specification? at " + loc);
			stack.push(builder.Declarator_functionDeclaratorWithES(pointerOperators.done(), modifiers, name,
					parameters.done(), virtSpecifiers.done(), attributes, loc, decl));
		} else if (_noexceptExpression != null) {
			if (_trailingReturnType != null)
				throw new RuntimeException(
						"FunctionDeclarator: Trailing return type and noexceptExpression? at " + loc);
			if (_initializer != null)
				throw new RuntimeException("FunctionDeclarator: Initializer and noexceptExpression? at " + loc);
			_noexceptExpression.accept(this);
			stack.push(builder.Declarator_functionDeclaratorNoexcept(pointerOperators.done(), modifiers, name,
					parameters.done(), virtSpecifiers.done(), stack.pop(), attributes, loc, decl));
		} else {
			if (_trailingReturnType != null)
				throw new RuntimeException(
						"FunctionDeclarator: Trailing return type and exception specification? at " + loc);
			IListWriter exceptionSpecification = vf.listWriter();
			Stream.of(_exceptionSpecification).forEach(it -> {
				it.accept(this);
				exceptionSpecification.append(stack.pop());
			});
			stack.push(builder.Declarator_functionDeclaratorWithES(pointerOperators.done(), modifiers, name,
					parameters.done(), virtSpecifiers.done(), exceptionSpecification.done(), attributes, loc, decl));
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
			throw new RuntimeException("Unknown sub-class encountered: " + declSpec.getClass().getName() + " at "
					+ getSourceLocation(declSpec) + ". Exiting");
		return PROCESS_ABORT;
	}

	public int visit(IASTCompositeTypeSpecifier declSpec) {
		if (declSpec instanceof ICASTCompositeTypeSpecifier)
			visit((ICASTCompositeTypeSpecifier) declSpec);
		else if (declSpec instanceof ICPPASTCompositeTypeSpecifier)
			visit((ICPPASTCompositeTypeSpecifier) declSpec);
		else
			throw new RuntimeException("Unknown IASTCompositeTypeSpecifier subinterface "
					+ declSpec.getClass().getName() + " at " + getSourceLocation(declSpec));
		return PROCESS_ABORT;
	}

	public int visit(ICASTCompositeTypeSpecifier declSpec) {
		throw new RuntimeException("C-style CompositeTypeSpecifier encountered at " + getSourceLocation(declSpec));
//		ISourceLocation loc = getSourceLocation(declSpec);
//		ISourceLocation decl = br.resolveBinding(declSpec);
//		IList modifiers = getModifiers(declSpec);
//
//		IASTName _name = declSpec.getName();
//		_name.accept(this);
//		IConstructor name = stack.pop();
//
//		IListWriter members = vf.listWriter();
//		Stream.of(declSpec.getMembers()).forEach(it -> {
//			it.accept(this);
//			members.append(stack.pop());
//		});
//
//		if (true)
//			throw new RuntimeException("Unfinished");
//
//		switch (declSpec.getKey()) {
//		case IASTCompositeTypeSpecifier.k_struct:
//			stack.push(builder.DeclSpecifier_struct(modifiers, name, members.done(), loc, decl));
//			break;
//		case IASTCompositeTypeSpecifier.k_union:
//			stack.push(builder.DeclSpecifier_union(modifiers, name, members.done(), loc, decl));
//			break;
//		default:
//			throw new RuntimeException("Unknown IASTCompositeTypeSpecifier code " + declSpec.getKey() + ". Exiting");
//		}
//
//		return PROCESS_ABORT;
	}

	public int visit(ICPPASTCompositeTypeSpecifier declSpec) {
		ISourceLocation loc = getSourceLocation(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec);
		IConstructor typ = tr.resolveType(declSpec);
		IList attributes = getAttributes(declSpec);
		IList modifiers = getModifiers(declSpec);

		ICPPASTClassVirtSpecifier virtSpecifier = declSpec.getVirtSpecifier();
		if (virtSpecifier != null && !(virtSpecifier.getKind().equals(ICPPASTClassVirtSpecifier.SpecifierKind.Final)))
			throw new RuntimeException(
					"ICPPASTCompositeTypeSpecifier encountered unknown classVirtSpecifier type at " + loc);

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
				stack.push(builder.DeclSpecifier_struct(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl));
			else
				stack.push(builder.DeclSpecifier_structFinal(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl));
			break;
		case ICPPASTCompositeTypeSpecifier.k_union:
			if (virtSpecifier == null)
				stack.push(builder.DeclSpecifier_union(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl));
			else
				stack.push(builder.DeclSpecifier_unionFinal(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl));
			break;
		case ICPPASTCompositeTypeSpecifier.k_class:
			if (virtSpecifier == null)
				stack.push(builder.DeclSpecifier_class(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl));
			else
				stack.push(builder.DeclSpecifier_classFinal(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl));
			break;
		default:
			throw new RuntimeException(
					"Unknown IASTCompositeTypeSpecifier code " + declSpec.getKey() + "at" + loc + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	public int visit(IASTElaboratedTypeSpecifier declSpec) {
		if (declSpec instanceof ICASTElaboratedTypeSpecifier) {
			out("ElaboratedTypeSpecifier: " + declSpec.getRawSignature());
			throw new RuntimeException("NYI at " + getSourceLocation(declSpec));
		} else if (declSpec instanceof ICPPASTElaboratedTypeSpecifier) {
			ISourceLocation loc = getSourceLocation(declSpec);
			ISourceLocation decl = br.resolveBinding(declSpec);
			IList modifiers = getModifiers(declSpec);

			declSpec.getName().accept(this);
			switch (declSpec.getKind()) {
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
				throw new RuntimeException(
						"IASTElaboratedTypeSpecifier encountered unknown kind " + declSpec.getKind() + " at " + loc);
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTEnumerationSpecifier declSpec) {
		if (declSpec instanceof ICPPASTEnumerationSpecifier)
			visit((ICPPASTEnumerationSpecifier) declSpec);
		else
			throw new RuntimeException("NYI at " + getSourceLocation(declSpec));
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
		throw new RuntimeException("NYI at " + getSourceLocation(declSpec));
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
			throw new RuntimeException("NYI at " + getSourceLocation(declSpec));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTElaboratedTypeSpecifier declSpec) {
		out("CPPElaboratedTypeSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(declSpec));
	}

	public int visit(ICPPASTEnumerationSpecifier declSpec) {
		ISourceLocation loc = getSourceLocation(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec);
		IConstructor typ = tr.resolveType(declSpec);
		IList attributes = getAttributes(declSpec);
		IList modifiers = getModifiers(declSpec);

		declSpec.getName().accept(this);
		IConstructor name = stack.pop();

		IListWriter enumerators = vf.listWriter();
		Stream.of(declSpec.getEnumerators()).forEach(it -> {
			it.accept(this);
			enumerators.append(stack.pop());
		});

		IASTDeclSpecifier baseType = declSpec.getBaseType();
		if (baseType == null) {
			if (declSpec.isScoped()) {
				if (declSpec.isOpaque())
					stack.push(builder.DeclSpecifier_enumScopedOpaque(modifiers, name, attributes, loc, decl));
				else
					stack.push(builder.DeclSpecifier_enumScoped(modifiers, name, enumerators.done(), attributes, loc,
							decl));
			} else
				stack.push(builder.DeclSpecifier_enum(modifiers, name, enumerators.done(), attributes, loc, decl));
		} else {
			baseType.accept(this);
			if (declSpec.isScoped()) {
				if (declSpec.isOpaque())
					stack.push(builder.DeclSpecifier_enumScopedOpaque(modifiers, stack.pop(), name, attributes, loc,
							decl));
				else
					stack.push(builder.DeclSpecifier_enumScoped(modifiers, stack.pop(), name, enumerators.done(),
							attributes, loc, decl));
			} else {
				if (declSpec.isOpaque())
					stack.push(builder.DeclSpecifier_enumOpaque(modifiers, stack.pop(), name, attributes, loc, decl));
				else
					stack.push(builder.DeclSpecifier_enum(modifiers, stack.pop(), name, enumerators.done(), attributes,
							loc, decl));
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNamedTypeSpecifier declSpec) {
		out("CPPNamedTypeSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(declSpec));
	}

	public int visit(ICPPASTSimpleDeclSpecifier declSpec) {
		ISourceLocation loc = getSourceLocation(declSpec);
		IConstructor typ = tr.resolveType(declSpec);
		IList attributes = getAttributes(declSpec);
		IList modifiers = getModifiers(declSpec);

		switch (declSpec.getType()) {
		case IASTSimpleDeclSpecifier.t_unspecified: {
			ISourceLocation location = null;
			if (modifiers.isEmpty()) {
				location = vf.sourceLocation(loc, loc.getOffset(), 0);
			} else {
				ISourceLocation before = (ISourceLocation) modifiers.get(modifiers.size() - 1).asWithKeywordParameters()
						.getParameter("src");
				location = vf.sourceLocation(before, before.getOffset() + before.getLength(), 0);
			}
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers, builder.Type_unspecified(location), attributes,
					loc));
			break;
		}
		case IASTSimpleDeclSpecifier.t_void:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_void(getTokenSourceLocation(declSpec, "void")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_char:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_char(getTokenSourceLocation(declSpec, "char")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_int:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_integer(getTokenSourceLocation(declSpec, "int")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_float:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_float(getTokenSourceLocation(declSpec, "float")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_double:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_double(getTokenSourceLocation(declSpec, "double")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_bool:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_bool(getTokenSourceLocation(declSpec, "bool")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_wchar_t:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_wchar_t(getTokenSourceLocation(declSpec, "wchar_t")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_typeof:
			declSpec.getDeclTypeExpression().accept(this);
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_typeof(getTokenSourceLocation(declSpec, "typeof")), stack.pop(), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_decltype:
			declSpec.getDeclTypeExpression().accept(this);
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decltype(getTokenSourceLocation(declSpec, "decltype")), stack.pop(), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_auto:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_auto(getTokenSourceLocation(declSpec, "auto")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_char16_t:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_char16_t(getTokenSourceLocation(declSpec, "char16_t")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_char32_t:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_char32_t(getTokenSourceLocation(declSpec, "char32_t")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_int128:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_int128(getTokenSourceLocation(declSpec, "__int128")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_float128:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_float128(getTokenSourceLocation(declSpec, "__float128")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_decimal32:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decimal128(getTokenSourceLocation(declSpec, "_Decimal32")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_decimal64:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decimal64(getTokenSourceLocation(declSpec, "_Decimal64")), attributes, loc));
			break;
		case IASTSimpleDeclSpecifier.t_decimal128:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decimal128(getTokenSourceLocation(declSpec, "_Decimal128")), attributes, loc));
			break;
		default:
			throw new RuntimeException(
					"Unknown IASTSimpleDeclSpecifier kind " + declSpec.getType() + " at " + loc + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTypeTransformationSpecifier declSpec) {
		// TODO: implement, check operator and operand
		err("ICPPASTTypeTransformationSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(declSpec));
	}

	@Override
	public int visit(IASTArrayModifier arrayModifier) {
		if (arrayModifier instanceof ICASTArrayModifier)
			throw new RuntimeException("NYI at " + getSourceLocation(arrayModifier));
		ISourceLocation loc = getSourceLocation(arrayModifier);
		IList attributes = getAttributes(arrayModifier);

		IASTExpression constantExpression = arrayModifier.getConstantExpression();
		if (constantExpression == null)
			stack.push(builder.Expression_arrayModifier(attributes, loc));
		else {
			constantExpression.accept(this);
			stack.push(builder.Expression_arrayModifier(stack.pop(), attributes, loc));
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
			throw new RuntimeException("Unknown IASTPointerOperator subtype +" + ptrOperator.getClass().getName()
					+ " at " + getSourceLocation(ptrOperator) + ". Exiting");
		return PROCESS_ABORT;
	}

	public int visit(IASTPointer pointer) {
		ISourceLocation loc = getSourceLocation(pointer);
		IList attributes = getAttributes(pointer);
		IList modifiers = getModifiers(pointer);
		if (pointer instanceof ICPPASTPointerToMember) {
			((ICPPASTPointerToMember) pointer).getName().accept(this);
			stack.push(builder.Declaration_pointerToMember(modifiers, stack.pop(), attributes, loc));
		} else
			stack.push(builder.Declaration_pointer(modifiers, attributes, loc));
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
		if (attribute.getArgumentClause() == null || attribute.getArgumentClause().getTokenCharImage() == null)
			stack.push(builder.Attribute_attribute(new String(attribute.getName()), src));
		else
			stack.push(builder.Attribute_attribute(new String(attribute.getName()),
					new String(attribute.getArgumentClause().getTokenCharImage()), src));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTAttributeSpecifier specifier) {
		ISourceLocation src = getSourceLocation(specifier);
		if (specifier instanceof ICPPASTAlignmentSpecifier) {
			IASTExpression expression = ((ICPPASTAlignmentSpecifier) specifier).getExpression();
			if (expression != null)
				expression.accept(this);
			else
				((ICPPASTAlignmentSpecifier) specifier).getTypeId().accept(this);
			stack.push(builder.Attribute_alignmentSpecifier(stack.pop(), src));
		} else {
			IASTAttributeList list = (IASTAttributeList) specifier;
			IListWriter attributes = vf.listWriter();
			Stream.of(list.getAttributes()).forEach(it -> {
				it.accept(this);
				attributes.append(stack.pop());
			});
			stack.push(builder.Attribute_attributeSpecifier(attributes.done(), src));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTToken token) {
		err("Token: " + new String(token.getTokenCharImage()));
		throw new RuntimeException("NYI at " + getSourceLocation(token));
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
		else if (expression instanceof CPPASTCompoundStatementExpression)
			visit((CPPASTCompoundStatementExpression) expression);
		else {
			throw new RuntimeException("Expression: encountered non-implemented subtype "
					+ expression.getClass().getName() + " at " + getSourceLocation(expression));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTArraySubscriptExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getArrayExpression().accept(this);
		IConstructor arrayExpression = stack.pop();
		expression.getArgument().accept(this);
		IConstructor argument = stack.pop();

		stack.push(builder.Expression_arraySubscriptExpression(arrayExpression, argument, loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTBinaryExpression expression) {
		out("CPPBinaryExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(expression));
	}

	public int visit(ICPPASTCastExpression expression) {
		out("CPPCastExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(expression));
	}

	public int visit(ICPPASTDeleteExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);
		expression.getOperand().accept(this);
		if (expression.isGlobal()) {
			if (expression.isVectored())
				stack.push(builder.Expression_globalVectoredDelete(stack.pop(), loc, typ));
			else
				stack.push(builder.Expression_globalDelete(stack.pop(), loc, typ));
		} else {
			if (expression.isVectored())
				stack.push(builder.Expression_vectoredDelete(stack.pop(), loc, typ));
			else
				stack.push(builder.Expression_delete(stack.pop(), loc, typ));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTExpressionList expression) {
		// has typ
		out("CPPExpressionList: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(expression));
	}

	public int visit(ICPPASTFieldReference expression) {
		// TODO: Implement
		// has typ
		out("CPPFieldReference: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(expression));
	}

	public int visit(ICPPASTFunctionCallExpression expression) {
		// has typ
		out("CPPFunctionCallExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(expression));
	}

	public int visit(ICPPASTLambdaExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		ISourceLocation decl = br.UNKNOWN;
		IConstructor typ = tr.resolveType(expression);
		CaptureDefault captureDefault = expression.getCaptureDefault();

		IListWriter captures = vf.listWriter();
		Stream.of(expression.getCaptures()).forEach(it -> {
			it.accept(this);
			captures.append(stack.pop());
		});

		IConstructor declarator;
		if (expression.getDeclarator() == null) {
			ISourceLocation endOfCapture = getTokenSourceLocation(expression, "]");
			declarator = builder.Declarator_missingDeclarator(
					vf.sourceLocation(endOfCapture, endOfCapture.getOffset() + 1, 0), decl);
		} else {
			expression.getDeclarator().accept(this);
			declarator = stack.pop();
		}

		expression.getBody().accept(this);
		IConstructor body = stack.pop();

		switch (captureDefault) {
		case BY_COPY:
			stack.push(
					builder.Expression_lambda(builder.Modifier_captDefByCopy(getTokenSourceLocation(expression, "=")),
							captures.done(), declarator, body, loc, typ));
			break;
		case BY_REFERENCE:
			stack.push(builder.Expression_lambda(
					builder.Modifier_captDefByReference(getTokenSourceLocation(expression, "&")), captures.done(),
					declarator, body, loc, typ));
			break;
		case UNSPECIFIED:
			stack.push(builder.Expression_lambda(
					builder.Modifier_captDefUnspecified(vf.sourceLocation(loc, loc.getOffset(), 0)), captures.done(),
					declarator, body, loc, typ));
			break;
		default:
			throw new RuntimeException("Unknown default capture type " + captureDefault + " encountered at "
					+ getSourceLocation(expression) + ", exiting");
		}

		return PROCESS_ABORT;
	}

	public int visit(ICPPASTLiteralExpression expression) {
		// This may never be reached
		visit((IASTLiteralExpression) expression);
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTNaryTypeIdExpression expression) {
		// has typ
		out("CPPNaryTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(expression));
	}

	public int visit(ICPPASTNewExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);
		// if (expression.isNewTypeId())
		// err("WARNING: ICPPASTNewExpression \"" + expression.getRawSignature()
		// + "\" has isNewTypeId=true");
		// else
		// err("WARNING: ICPPASTNewExpression \"" + expression.getRawSignature()
		// + "\" has isNewTypeId=false");

		expression.getTypeId().accept(this);
		IConstructor typeId = stack.pop();

		IASTInitializerClause[] _placementArguments = expression.getPlacementArguments();
		IASTInitializer _initializer = expression.getInitializer();
		if (_placementArguments != null) {
			IListWriter placementArguments = vf.listWriter();
			Stream.of(_placementArguments).forEach(it -> {
				it.accept(this);
				placementArguments.append(stack.pop());
			});
			if (_initializer == null) {
				if (expression.isGlobal())
					stack.push(builder.Expression_globalNewWithArgs(placementArguments.done(), typeId, loc, typ));
				else
					stack.push(builder.Expression_newWithArgs(placementArguments.done(), typeId, loc, typ));
			} else {
				_initializer.accept(this);
				if (expression.isGlobal())
					stack.push(builder.Expression_globalNewWithArgs(placementArguments.done(), typeId, stack.pop(), loc,
							typ));
				else
					stack.push(
							builder.Expression_newWithArgs(placementArguments.done(), typeId, stack.pop(), loc, typ));
			}
		} else if (_initializer == null) {
			if (expression.isGlobal())
				stack.push(builder.Expression_globalNew(typeId, loc, typ));
			else
				stack.push(builder.Expression_new(typeId, loc, typ));
		} else {
			_initializer.accept(this);
			if (expression.isGlobal())
				stack.push(builder.Expression_globalNew(typeId, stack.pop(), loc, typ));
			else
				stack.push(builder.Expression_new(typeId, stack.pop(), loc, typ));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTPackExpansionExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);
		expression.getPattern().accept(this);
		stack.push(builder.Expression_packExpansion(stack.pop(), loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTSimpleTypeConstructorExpression expression) {
		// decl keyword parameter?
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getDeclSpecifier().accept(this);
		IConstructor declSpecifier = stack.pop();
		expression.getInitializer().accept(this);
		IConstructor initializer = stack.pop();

		stack.push(builder.Expression_simpleTypeConstructor(declSpecifier, initializer, loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTypeIdExpression expression) {
		// has typ
		out("CPPTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(expression));
	}

	public int visit(ICPPASTUnaryExpression expression) {
		out("CPPUnaryExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(expression));
	}

	public int visit(CPPASTCompoundStatementExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ;
		try {
			typ = tr.resolveType(expression);
		} catch (Throwable t) {
			err("CPPASTCompoundStatement couldn't get type at " + loc);
			t.printStackTrace(ctx.getStdErr());
			typ = builder.TypeSymbol_any();
		}
		expression.getCompoundStatement().accept(this);
		stack.push(builder.Expression_compoundStatementExpression(stack.pop(), loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(IASTArraySubscriptExpression expression) {
		if (expression instanceof ICPPASTArraySubscriptExpression)
			visit((ICPPASTArraySubscriptExpression) expression);
		else
			throw new RuntimeException("NYI at " + getSourceLocation(expression));
		return PROCESS_ABORT;
	}

	public int visit(IASTBinaryExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getOperand1().accept(this);
		IConstructor lhs = stack.pop();
		expression.getInitOperand2().accept(this);
		IConstructor rhs = stack.pop();

		switch (expression.getOperator()) {
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
			throw new RuntimeException("Operator " + expression.getOperator() + " unknown at " + loc + ", exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTBinaryTypeIdExpression expression) {
		// has typ
		out("BinaryTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(expression));
	}

	public int visit(IASTCastExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getOperand().accept(this);
		IConstructor operand = stack.pop();
		expression.getTypeId().accept(this);
		IConstructor type = stack.pop();

		switch (expression.getOperator()) {
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
			throw new RuntimeException("Unknown cast type " + expression.getOperator() + " at " + loc);
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTConditionalExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getLogicalConditionExpression().accept(this);
		IConstructor condition = stack.pop();
		expression.getPositiveResultExpression().accept(this);
		IConstructor positive = stack.pop();
		expression.getNegativeResultExpression().accept(this);
		IConstructor negative = stack.pop();

		stack.push(builder.Expression_conditional(condition, positive, negative, loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(IASTExpressionList expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);
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
		if (expression instanceof ICPPASTFieldReference) {
			// TODO: implement isTemplate
			ISourceLocation decl = br.resolveBinding(expression);
			IConstructor typ = tr.resolveType(expression);

			expression.getFieldOwner().accept(this);
			IConstructor fieldOwner = stack.pop();
			expression.getFieldName().accept(this);
			IConstructor fieldName = stack.pop();

			if (expression.isPointerDereference())
				stack.push(builder.Expression_fieldReferencePointerDeref(fieldOwner, fieldName, loc, decl, typ));
			else
				stack.push(builder.Expression_fieldReference(fieldOwner, fieldName, loc, decl, typ));
		} else
			throw new RuntimeException("IASTFieldReference: NYI at " + loc);
		return PROCESS_ABORT;
	}

	public int visit(IASTFunctionCallExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getFunctionNameExpression().accept(this);
		IConstructor functionName = stack.pop();

		IListWriter arguments = vf.listWriter();
		Stream.of(expression.getArguments()).forEach(it -> {
			it.accept(this);
			arguments.append(stack.pop());
		});
		stack.push(builder.Expression_functionCall(functionName, arguments.done(), loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(IASTIdExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		ISourceLocation decl = br.resolveBinding(expression);
		IConstructor typ = tr.resolveType(expression);
		expression.getName().accept(this);
		stack.push(builder.Expression_idExpression(stack.pop(), loc, decl, typ));
		return PROCESS_ABORT;
	}

	public int visit(IASTLiteralExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);

		String value = new String(expression.getValue());
		switch (expression.getKind()) {
		case IASTLiteralExpression.lk_integer_constant:
			stack.push(builder.Expression_integerConstant(value, loc, typ));
			break;
		case IASTLiteralExpression.lk_float_constant:
			stack.push(builder.Expression_floatConstant(value, loc, typ));
			break;
		case IASTLiteralExpression.lk_char_constant:
			stack.push(builder.Expression_charConstant(value, loc, typ));
			break;
		case IASTLiteralExpression.lk_string_literal:
			stack.push(builder.Expression_stringLiteral(value, loc, typ));
			break;
		case IASTLiteralExpression.lk_this:
			stack.push(builder.Expression_this(loc, typ));
			break;
		case IASTLiteralExpression.lk_true:
			stack.push(builder.Expression_true(loc, typ));
			break;
		case IASTLiteralExpression.lk_false:
			stack.push(builder.Expression_false(loc, typ));
			break;
		case IASTLiteralExpression.lk_nullptr:
			stack.push(builder.Expression_nullptr(loc, typ));
			break;
		default:
			throw new RuntimeException(
					"Encountered unknown literal kind " + expression.getKind() + " at " + loc + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTProblemExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IASTProblem problem = expression.getProblem();
		if (doProblemLogging)
			err("ProblemExpression " + expression.getRawSignature() + ":" + problem.getMessageWithLocation());
		stack.push(builder.Expression_problemExpression(loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTTypeIdInitializerExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);
		expression.getTypeId().accept(this);
		IConstructor typeId = stack.pop();
		expression.getInitializer().accept(this);
		stack.push(builder.Expression_typeIdInitializerExpression(typeId, stack.pop(), loc, typ));
		return PROCESS_ABORT;
	}

	public int visit(IASTTypeIdExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getTypeId().accept(this);
		switch (expression.getOperator()) {
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
			throw new RuntimeException("ERROR: IASTTypeIdExpression called with unimplemented/unknown operator "
					+ expression.getOperator() + " at " + loc);
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTUnaryExpression expression) {
		ISourceLocation loc = getSourceLocation(expression);
		IConstructor typ = tr.resolveType(expression);

		IConstructor operand = null;
		if (expression.getOperand() != null) {
			expression.getOperand().accept(this);
			operand = stack.pop();
		}

		switch (expression.getOperator()) {
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
			throw new RuntimeException(
					"Unknown unary operator " + expression.getOperator() + " at " + loc + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTStatement statement) {
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
		else if (statement instanceof IASTGotoStatement)
			visit((IASTGotoStatement) statement);
		else if (statement instanceof IASTIfStatement)
			visit((IASTIfStatement) statement);
		else if (statement instanceof IASTLabelStatement)
			visit((IASTLabelStatement) statement);
		else if (statement instanceof IASTNullStatement)
			visit((IASTNullStatement) statement);
		else if (statement instanceof IASTReturnStatement)
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
		else if (statement instanceof IGNUASTGotoStatement)
			visit((IGNUASTGotoStatement) statement);
		else if (statement instanceof IASTProblemStatement)
			visit((IASTProblemStatement) statement);
		else {
			throw new RuntimeException("Statement: encountered non-implemented subtype "
					+ statement.getClass().getName() + " at " + getSourceLocation(statement));
		}
		return PROCESS_ABORT;
	}

	public int visit(IGNUASTGotoStatement statement) {
		// requires decl keyword parameter
		err("IGNUAstGotoStatement: " + statement.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(statement));
	}

	public int visit(ICPPASTCatchHandler statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);

		statement.getCatchBody().accept(this);
		IConstructor catchBody = stack.pop();

		if (statement.isCatchAll())
			stack.push(builder.Statement_catchAll(catchBody, attributes, loc));
		else {
			statement.getDeclaration().accept(this);
			stack.push(builder.Statement_catch(stack.pop(), catchBody, attributes, loc));
		}
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTRangeBasedForStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);

		statement.getDeclaration().accept(this);
		IConstructor declaration = stack.pop();
		statement.getInitializerClause().accept(this);
		IConstructor initializerClause = stack.pop();
		statement.getBody().accept(this);
		IConstructor body = stack.pop();

		stack.push(builder.Statement_rangeBasedFor(declaration, initializerClause, body, attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(ICPPASTTryBlockStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);

		statement.getTryBody().accept(this);
		IConstructor tryBody = stack.pop();

		IListWriter catchHandlers = vf.listWriter();
		Stream.of(statement.getCatchHandlers()).forEach(it -> {
			it.accept(this);
			catchHandlers.append(stack.pop());
		});

		stack.push(builder.Statement_tryBlock(tryBody, catchHandlers.done(), attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTAmbiguousStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		out("visit(IASTAmbiguousStatement) " + loc);
		out(statement.getRawSignature());
		IListWriter statements = vf.listWriter();
		prefix += 4;
		Stream.of(statement.getStatements()).forEach(it -> {
			out("Statement " + it.getClass().getSimpleName() + ": " + it.getRawSignature());
			it.accept(this);
			statements.append(stack.pop());
		});
		prefix -= 4;
		throw new RuntimeException("Encountered Ambiguous statement at " + loc);
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
		statement.getExpression().accept(this);
		IConstructor expression = stack.pop();
		stack.push(builder.Statement_case(expression, attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTCompoundStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		IListWriter statements = vf.listWriter();
		Stream.of(statement.getStatements()).forEach(it -> {
			it.accept(this);
			statements.append(stack.pop());
		});
		stack.push(builder.Statement_compoundStatement(statements.done(), attributes, loc));
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
		statement.getDeclaration().accept(this);
		stack.push(builder.Statement_declarationStatement(stack.pop(), attributes, loc));
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

		statement.getBody().accept(this);
		IConstructor body = stack.pop();
		statement.getCondition().accept(this);
		IConstructor condition = stack.pop();
		stack.push(builder.Statement_do(body, condition, attributes, loc));

		return PROCESS_ABORT;
	}

	public int visit(IASTExpressionStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		statement.getExpression().accept(this);
		stack.push(builder.Statement_expressionStatement(stack.pop(), attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTForStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);

		IASTStatement _initializer = statement.getInitializerStatement();
		IConstructor initializer = null;
		if (_initializer != null) {
			_initializer.accept(this);
			initializer = stack.pop();
		}

		IASTExpression _condition = statement.getConditionExpression();
		IConstructor condition;
		if (_condition == null) {
			ISourceLocation initializerLoc = (ISourceLocation) initializer.asWithKeywordParameters()
					.getParameter("src");
			condition = builder.Expression_empty(
					vf.sourceLocation(initializerLoc, initializerLoc.getOffset() + initializerLoc.getLength(), 0));
		} else {
			_condition.accept(this);
			condition = stack.pop();
		}

		IASTExpression _iteration = statement.getIterationExpression();
		IConstructor iteration;
		if (_iteration == null) {
			ISourceLocation conditionLoc = (ISourceLocation) condition.asWithKeywordParameters().getParameter("src");
			iteration = builder.Expression_empty(
					vf.sourceLocation(conditionLoc, conditionLoc.getOffset() + conditionLoc.getLength(), 0));
		} else {
			_iteration.accept(this);
			iteration = stack.pop();
		}

		statement.getBody().accept(this);
		IConstructor body = stack.pop();

		if (statement instanceof ICPPASTForStatement) {
			IASTDeclaration _conditionDeclaration = ((ICPPASTForStatement) statement).getConditionDeclaration();
			if (_conditionDeclaration != null) {
				_conditionDeclaration.accept(this);
				stack.push(builder.Statement_forWithDecl(initializer, stack.pop(), iteration, body, attributes, loc));
				return PROCESS_ABORT;
			}
		}
		stack.push(builder.Statement_for(initializer, condition, iteration, body, attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTGotoStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		ISourceLocation decl = br.resolveBinding(statement);
		IList attributes = getAttributes(statement);
		statement.getName().accept(this);
		stack.push(builder.Statement_goto(stack.pop(), attributes, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(IASTIfStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);

		statement.getThenClause().accept(this);
		IConstructor thenClause = stack.pop();

		IConstructor elseClause = null;
		if (statement.getElseClause() != null) {
			statement.getElseClause().accept(this);
			elseClause = stack.pop();
		}

		if (statement.getConditionExpression() == null && statement instanceof ICPPASTIfStatement) {
			((ICPPASTIfStatement) statement).getConditionDeclaration().accept(this);
			if (elseClause == null) {
				stack.push(builder.Statement_ifWithDecl(stack.pop(), thenClause, attributes, loc));
			} else {
				stack.push(builder.Statement_ifWithDecl(stack.pop(), thenClause, elseClause, attributes, loc));
			}
		} else {
			statement.getConditionExpression().accept(this);
			if (elseClause == null) {
				stack.push(builder.Statement_if(stack.pop(), thenClause, attributes, loc));
			} else {
				stack.push(builder.Statement_if(stack.pop(), thenClause, elseClause, attributes, loc));
			}
		}
		return PROCESS_ABORT;
	}

	public int visit(IASTLabelStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		ISourceLocation decl = br.resolveBinding(statement);
		IList attributes = getAttributes(statement);

		statement.getName().accept(this);
		IConstructor name = stack.pop();
		statement.getNestedStatement().accept(this);
		IConstructor nestedStatement = stack.pop();

		stack.push(builder.Statement_label(name, nestedStatement, attributes, loc, decl));
		return PROCESS_ABORT;
	}

	public int visit(IASTNullStatement statement) {
		ISourceLocation loc = getSourceLocation(statement);
		IList attributes = getAttributes(statement);
		stack.push(builder.Statement_nullStatement(attributes, loc));
		return PROCESS_ABORT;
	}

	public int visit(IASTProblemStatement statement) {
		if (doProblemLogging) {
			err("IASTProblemStatement:");
			prefix += 4;
			err(statement.getProblem().getMessageWithLocation());
			err(statement.getRawSignature());
			prefix -= 4;
		}
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
			stack.push(builder.Statement_return(stack.pop(), attributes, loc));
		} else {
			returnArgument.accept(this);
			// Note: InitializerClause is currently mapped on Expression
			stack.push(builder.Statement_return(stack.pop(), attributes, loc));
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
			stack.push(builder.Statement_switchWithDecl(stack.pop(), body, attributes, loc));
			return PROCESS_ABORT;
		}

		_controller.accept(this);
		stack.push(builder.Statement_switch(stack.pop(), body, attributes, loc));
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
			stack.push(builder.Statement_whileWithDecl(stack.pop(), body, attributes, loc));
			return PROCESS_ABORT;
		}
		_condition.accept(this);
		stack.push(builder.Statement_while(stack.pop(), body, attributes, loc));
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
				throw new RuntimeException("IASTProblemTypeId encountered at " + loc + "! "
						+ ((IASTProblemTypeId) typeId).getProblem().getMessageWithLocation());
			}
		} else {
			typeId.getDeclSpecifier().accept(this);
			IConstructor declSpecifier = stack.pop();
			typeId.getAbstractDeclarator().accept(this);
			IConstructor abstractDeclarator = stack.pop();
			if (abstractDeclarator.has("name")) {// TODO: properly fix
				ISourceLocation declaratorLoc = getSourceLocation(typeId.getAbstractDeclarator());
				abstractDeclarator = abstractDeclarator.set("name", builder.Name_abstractEmptyName(declaratorLoc));
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

		enumerator.getName().accept(this);
		IConstructor name = stack.pop();

		IASTExpression value = enumerator.getValue();
		if (value == null)
			stack.push(builder.Declaration_enumerator(name, loc, decl));
		else {
			value.accept(this);
			stack.push(builder.Declaration_enumerator(name, stack.pop(), loc, decl));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTProblem problem) {
		err("Problem: " + problem.getMessage());
		throw new RuntimeException("NYI at " + getSourceLocation(problem));
	}

	@Override
	public int visit(ICPPASTBaseSpecifier baseSpecifier) {
		ISourceLocation loc = getSourceLocation(baseSpecifier);
		ISourceLocation decl = br.resolveBinding(baseSpecifier);
		IConstructor typ = tr.resolveType(baseSpecifier);

		IListWriter modifiers = vf.listWriter();
		switch (baseSpecifier.getVisibility()) {
		case ICPPASTBaseSpecifier.v_public:
			modifiers.append(builder.Modifier_public(getTokenSourceLocation(baseSpecifier, "public")));
			break;
		case ICPPASTBaseSpecifier.v_protected:
			modifiers.append(builder.Modifier_protected(getTokenSourceLocation(baseSpecifier, "protected")));
			break;
		case ICPPASTBaseSpecifier.v_private:
			modifiers.append(builder.Modifier_private(getTokenSourceLocation(baseSpecifier, "private")));
			break;
		case 0:
			modifiers.append(builder.Modifier_unspecifiedInheritance(vf.sourceLocation(loc, loc.getOffset(), 0)));
			break;
		default:
			throw new RuntimeException(
					"Unknown BaseSpecifier visibility code " + baseSpecifier.getVisibility() + " at " + loc);
		}
		if (baseSpecifier.isVirtual())
			modifiers.append(builder.Modifier_virtual(getTokenSourceLocation(baseSpecifier, "virtual")));

		ICPPASTNameSpecifier nameSpecifier = baseSpecifier.getNameSpecifier();
		if (nameSpecifier == null)
			stack.push(builder.Declaration_baseSpecifier(modifiers.done(), loc, decl));
		else {
			nameSpecifier.accept(this);
			stack.push(builder.Declaration_baseSpecifier(modifiers.done(), stack.pop(), loc, decl));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTNamespaceDefinition namespaceDefinition) {
		ISourceLocation loc = getSourceLocation(namespaceDefinition);
		ISourceLocation decl = br.resolveBinding(namespaceDefinition);
		IList attributes = getAttributes(namespaceDefinition);

		namespaceDefinition.getName().accept(this);
		IConstructor name = stack.pop();

		IListWriter declarations = vf.listWriter();
		Stream.of(namespaceDefinition.getDeclarations()).forEach(it -> {
			it.accept(this);
			declarations.append(stack.pop());
		});

		if (namespaceDefinition.isInline())
			stack.push(builder.Declaration_namespaceDefinitionInline(name, declarations.done(), attributes, loc, decl));
		else
			stack.push(builder.Declaration_namespaceDefinition(name, declarations.done(), attributes, loc, decl));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTTemplateParameter templateParameter) {
		ISourceLocation loc = getSourceLocation(templateParameter);
		boolean isParameterPack = templateParameter.isParameterPack();
		// if (isParameterPack)
		// err("WARNING: ICPPASTTemplateParameter has isParameterPack=true,
		// unimplemented");
		if (templateParameter instanceof ICPPASTParameterDeclaration) {
			// TODO: duplicate, never reached, remove
			((ICPPASTParameterDeclaration) templateParameter).getDeclSpecifier().accept(this);
			IConstructor declSpecifier = stack.pop();
			ICPPASTDeclarator declarator = ((ICPPASTParameterDeclaration) templateParameter).getDeclarator();
			if (declarator == null)
				stack.push(builder.Declaration_parameter(declSpecifier, loc));
			else {
				declarator.accept(this);
				stack.push(builder.Declaration_parameter(declSpecifier, stack.pop(), loc));
			}
		} else if (templateParameter instanceof ICPPASTSimpleTypeTemplateParameter) {
			ISourceLocation decl = br.resolveBinding((ICPPASTSimpleTypeTemplateParameter) templateParameter);
			IConstructor typ = tr.resolveType(templateParameter);

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
							+ parameter.getParameterType() + " at " + loc);
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
							+ parameter.getParameterType() + " at " + loc);
				}
			}
		} else if (templateParameter instanceof ICPPASTTemplatedTypeTemplateParameter) {
			ISourceLocation decl = br.resolveBinding((ICPPASTTemplatedTypeTemplateParameter) templateParameter);
			IListWriter templateParameters = vf.listWriter();
			Stream.of(((ICPPASTTemplatedTypeTemplateParameter) templateParameter).getTemplateParameters())
					.forEach(it -> {
						it.accept(this);
						templateParameters.append(stack.pop());
					});
			((ICPPASTTemplatedTypeTemplateParameter) templateParameter).getName().accept(this);
			stack.push(builder.Declaration_tttParameter(templateParameters.done(), stack.pop(), loc, decl));
			if (((ICPPASTTemplatedTypeTemplateParameter) templateParameter).getDefaultValue() != null)
				err("ICPPASTTemplatedTypeTemplateParameter has defaultType at " + loc + ", unimplemented");
		} else
			throw new RuntimeException("ICPPASTTemplateParameter encountered unknown subtype "
					+ templateParameter.getClass().getName() + " at " + loc + ". Exiting");
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
		throw new RuntimeException("NYI at " + getSourceLocation(designator));
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
			((IGPPASTArrayRangeDesignator) designator).getRangeFloor().accept(this);
			IConstructor rangeFloor = stack.pop();
			((IGPPASTArrayRangeDesignator) designator).getRangeCeiling().accept(this);
			IConstructor rangeCeiling = stack.pop();
			stack.push(builder.Expression_arrayRangeDesignator(rangeFloor, rangeCeiling, loc));
		} else
			throw new RuntimeException("ICPPASTDesignator encountered unknown subclass at " + loc + ", exiting");
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTVirtSpecifier virtSpecifier) {
		ISourceLocation loc = getSourceLocation(virtSpecifier);
		switch (virtSpecifier.getKind()) {
		case Final:
			stack.push(builder.Declaration_virtSpecifier(
					builder.Modifier_final(getTokenSourceLocation(virtSpecifier, "final")), loc));
			break;
		case Override:
			stack.push(builder.Declaration_virtSpecifier(
					builder.Modifier_override(getTokenSourceLocation(virtSpecifier, "override")), loc));
			break;
		default:
			throw new RuntimeException("ICPPASTVirtSpecifier encountered unknown SpecifierKind "
					+ virtSpecifier.getKind().name() + " at " + loc);
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTClassVirtSpecifier classVirtSpecifier) {
		err("ClassVirtSpecifier: " + classVirtSpecifier.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(classVirtSpecifier));
	}

	@Override
	public int visit(ICPPASTDecltypeSpecifier decltypeSpecifier) {
		// has typ
		err("DecltypeSpecifier: " + decltypeSpecifier.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(decltypeSpecifier));
	}

	@Override
	public int visit(ASTAmbiguousNode astAmbiguousNode) {
		err("AstAmbiguousNode: " + astAmbiguousNode.getRawSignature());
		throw new RuntimeException("NYI at " + getSourceLocation(astAmbiguousNode));
	}

}
