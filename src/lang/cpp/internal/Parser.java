/** 
 * Copyright (c) 2016-2020, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
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

import java.io.PrintWriter;
import java.net.URISyntaxException;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
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
import org.eclipse.cdt.core.dom.ast.IASTMacroExpansionLocation;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNamedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.eclipse.cdt.core.dom.ast.IASTNodeLocation;
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
import org.eclipse.cdt.core.dom.ast.c.ICASTArrayDesignator;
import org.eclipse.cdt.core.dom.ast.c.ICASTArrayModifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignatedInitializer;
import org.eclipse.cdt.core.dom.ast.c.ICASTDesignator;
import org.eclipse.cdt.core.dom.ast.c.ICASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTEnumerationSpecifier;
import org.eclipse.cdt.core.dom.ast.c.ICASTFieldDesignator;
import org.eclipse.cdt.core.dom.ast.c.ICASTSimpleDeclSpecifier;
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
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunction;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPMethod;
import org.eclipse.cdt.core.dom.ast.gnu.IGCCASTAttributeList;
import org.eclipse.cdt.core.dom.ast.gnu.IGNUASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.gnu.c.IGCCASTArrayRangeDesignator;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.IGPPASTArrayRangeDesignator;
import org.eclipse.cdt.core.dom.ast.ms.IMSASTDeclspecList;
import org.eclipse.cdt.core.model.ILanguage;
import org.eclipse.cdt.core.parser.DefaultLogService;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.IParserLogService;
import org.eclipse.cdt.core.parser.IScannerInfo;
import org.eclipse.cdt.core.parser.IToken;
import org.eclipse.cdt.core.parser.IncludeFileContentProvider;
import org.eclipse.cdt.core.parser.ScannerInfo;
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode;
import org.eclipse.cdt.internal.core.dom.parser.ASTAmbiguousNode.NameCollector;
import org.eclipse.cdt.internal.core.dom.parser.IASTAmbiguousStatement;
import org.eclipse.cdt.internal.core.dom.parser.c.CASTArraySubscriptExpression;
import org.eclipse.cdt.internal.core.dom.parser.c.CASTCompoundStatementExpression;
import org.eclipse.cdt.internal.core.dom.parser.c.CASTDeclarator;
import org.eclipse.cdt.internal.core.dom.parser.c.CASTFieldReference;
import org.eclipse.cdt.internal.core.dom.parser.c.CASTFunctionDeclarator;
import org.eclipse.cdt.internal.core.dom.parser.c.CASTName;
import org.eclipse.cdt.internal.core.dom.parser.c.CASTParameterDeclaration;
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTCompoundStatementExpression;
import org.eclipse.cdt.internal.core.dom.parser.cpp.ClassTypeHelper;
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPFunctionSet;
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPSemantics;
import org.eclipse.core.runtime.CoreException;
import org.rascalmpl.debug.IRascalMonitor;
import org.rascalmpl.exceptions.RuntimeExceptionFactory;
import org.rascalmpl.interpreter.Evaluator;
import org.rascalmpl.uri.URIUtil;
import org.rascalmpl.values.IRascalValueFactory;

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
import io.usethesource.vallang.exceptions.FactTypeUseException;

public class Parser extends ASTVisitor {
	private final IValueFactory vf;
	private final PrintWriter stdOut;
	private final PrintWriter stdErr;
	private final IRascalMonitor monitor;
	private final AST builder;
	private final Stack<IConstructor> stack = new Stack<>();
	private final BindingsResolver br;
	private final TypeResolver tr;
	private final Locations locs;

	private boolean doProblemLogging = false;
	private boolean toM3 = false;
	private boolean includeStdLib = false;
	private IList stdLib;

	private ISetWriter declaredType;
	private ISetWriter functionDefinitions;
	private ISetWriter newResolutions;
	private ISetWriter implicitDeclarations;
	
	private IASTNode currentNode;

	private void at(IASTNode it) {
		this.currentNode = it;
	}

	public Parser(IValueFactory vf, IRascalValueFactory rvf, PrintWriter stdOut, PrintWriter stdErr, 
			IRascalMonitor monitor) {
		super(true);
		this.shouldVisitAmbiguousNodes = true;
		this.shouldVisitImplicitNames = true;
		this.includeInactiveNodes = true;
		this.shouldVisitTokens = true;

		this.vf = vf;
		this.stdOut = stdOut;
		this.stdErr = stdErr;
		this.monitor = monitor;
		this.builder = new AST(vf);
		this.br = new BindingsResolver(vf, stdOut, stdErr);
		this.locs = new Locations(vf, stdErr);
		this.tr = new TypeResolver(builder, br, locs, vf, stdOut);

		reset();
	}

	private void reset() {
		toM3 = false;
		includeStdLib = false;
		stdLib = vf.listWriter().done();
		declaredType = vf.setWriter();
		functionDefinitions = vf.setWriter();
		implicitDeclarations = vf.setWriter();
		newResolutions = vf.setWriter();
		currentNode = null;
	}

	public IList parseFiles(IList files, IString charset, IBool inferCharset, IList stdLib, IList includeDirs, IMap standardMacros, IMap additionalMacros, IBool includeStdLib) {
		this.includeStdLib = includeStdLib.getValue() || stdLib.isEmpty();
		this.stdLib = stdLib;

		CDTParser parser = new CDTParser(vf, stdOut, stdErr, stdLib, includeDirs, standardMacros, additionalMacros,
				includeStdLib.getValue());
		monitor.jobStart("ClaiR parseFiles");
		IListWriter asts = vf.listWriter();
		for (IValue v : files) {
			if (monitor.jobIsCanceled("ClaiR parseFiles")) {
				monitor.jobEnd("ClaiR parseFiles", false);
				break;
			}
			ISourceLocation file = (ISourceLocation) v;
			IASTTranslationUnit tu = parser.parseFileAsCpp(file, charset, inferCharset);
			try {
				IValue result = convertCdtToRascal(tu, false);
				ISourceLocation tuDecl = URIUtil.correctLocation("cpp+translationUnit", "", file.getPath());
				result = ((IConstructor) result).asWithKeywordParameters().setParameter("decl", tuDecl);
				asts.append(result);
			}
			catch (NullPointerException e) {
				throw new UnsupportedOperationException(
					"Conversion to AST did not work for " + currentNode.getClass().getCanonicalName() + " at " + locs.forNode(currentNode), e);
			}
		}

		reset();
		monitor.jobEnd("ClaiR parseFiles", true);

		return asts.done();
	}

	public IValue parseC(ISourceLocation file, IString charset, IBool inferCharset, IList stdLib, IList includeDirs, IMap standardMacros, IMap additionalMacros,
			IBool includeStdLib) {
		this.includeStdLib = includeStdLib.getValue() || stdLib.isEmpty();
		this.stdLib = stdLib;

		CDTParser parser = new CDTParser(vf, stdOut, stdErr, stdLib, includeDirs, standardMacros, additionalMacros,
				includeStdLib.getValue());
		IASTTranslationUnit tu = parser.parseFileAsC(file, charset, inferCharset);

		try {
			IValue result = convertCdtToRascal(tu, false);
			ISourceLocation tuDecl = URIUtil.correctLocation("cpp+translationUnit", "", file.getPath());
			result = ((IConstructor) result).asWithKeywordParameters().setParameter("decl", tuDecl);

			if (result == null) {
				throw RuntimeExceptionFactory.parseError(file, null, null);
			}
			return result;
		}
		catch (NullPointerException e) {
			throw new UnsupportedOperationException(
				"Conversion to AST did not work for " + currentNode.getClass().getCanonicalName() + " at " + locs.forNode(currentNode), e);
		}
		finally {
			reset();
		}
	}

	public IValue parseCpp(ISourceLocation file, IString charset, IBool inferCharset, IList stdLib, IList includeDirs, IMap standardMacros, IMap additionalMacros,
			IBool includeStdLib) {
		this.includeStdLib = includeStdLib.getValue() || stdLib.isEmpty();
		this.stdLib = stdLib;

		CDTParser parser = new CDTParser(vf, stdOut, stdErr, stdLib, includeDirs, standardMacros, additionalMacros,
				includeStdLib.getValue());
		IASTTranslationUnit tu = parser.parseFileAsCpp(file, charset, inferCharset);

		try {
			IValue result = convertCdtToRascal(tu, false);
			ISourceLocation tuDecl = URIUtil.correctLocation("cpp+translationUnit", "", file.getPath());
			result = ((IConstructor) result).asWithKeywordParameters().setParameter("decl", tuDecl);
			if (result == null) {
				throw RuntimeExceptionFactory.parseError(file, null, null);
			}
			return result;
		}
		catch (NullPointerException e) {
			throw new UnsupportedOperationException(
				"Conversion to AST did not work for " + currentNode.getClass().getCanonicalName() + " at " + locs.forNode(currentNode), e);
		}
		finally {
			reset();
		}
	}

	public ITuple parseCToM3AndAst(ISourceLocation file, IString charset, IBool inferCharset, IList stdLib, IList includeDirs, IMap standardMacros, IMap additionalMacros,
			IBool includeStdLib) {
		this.includeStdLib = includeStdLib.getValue() || stdLib.isEmpty();
		this.stdLib = stdLib;

		IValue m3 = builder.M3_m3(file);
		ISourceLocation tuDecl = URIUtil.correctLocation("cpp+translationUnit", "", file.getPath());
		br.setTranslationUnit(tuDecl);
		CDTParser parser = new CDTParser(vf,stdOut, stdErr, stdLib, includeDirs, standardMacros, additionalMacros,
				includeStdLib.getValue());
		IASTTranslationUnit tu = null;
		try {
			tu = parser.parseFileAsC(file, charset, inferCharset);
		} catch (ClassCastException e) {
			// Encountered this in UNIX C files
			IListWriter error = vf.listWriter();
			error.append(builder.Declaration_problemDeclaration(file, false));
			return vf.tuple(m3, builder.Declaration_translationUnit(error.done(), file, false));
		}
		IList comments = getCommentsFromTranslationUnit(tu);
		ISet macroExpansions = getMacroExpansionsFromTranslationUnit(tu);
		ISet macroDefinitions = getMacroDefinitionsFromTranslationUnit(tu);
		

		m3 = m3.asWithKeywordParameters().setParameter("comments", comments);
		m3 = m3.asWithKeywordParameters().setParameter("macroExpansions", macroExpansions);
		m3 = m3.asWithKeywordParameters().setParameter("macroDefinitions", macroDefinitions);
		m3 = setM3IncludeInformationFromTranslationUnit(tu, m3);

		declaredType = vf.setWriter();
		functionDefinitions = vf.setWriter();
		implicitDeclarations = vf.setWriter();
		newResolutions = vf.setWriter();

		try {
			IValue result = convertCdtToRascal(tu, true);

			// based on side-effects of the conversion
			ISet methodOverrides = getMethodOverrides(tu, newResolutions.done());
			m3 = m3.asWithKeywordParameters().setParameter("methodOverrides", methodOverrides);

			result = ((IConstructor) result).asWithKeywordParameters().setParameter("decl", tuDecl);
			m3 = m3.asWithKeywordParameters().setParameter("declaredType", declaredType.done());
			m3 = m3.asWithKeywordParameters().setParameter("functionDefinitions", functionDefinitions.done());
			m3 = m3.asWithKeywordParameters().setParameter("containment", br.getContainmentRelation());
			m3 = m3.asWithKeywordParameters().setParameter("implicitDeclarations", implicitDeclarations.done());

			return vf.tuple(m3, result);
		}
		catch (NullPointerException e) {
			throw new UnsupportedOperationException(
				"Conversion to AST did not work for " + currentNode.getClass().getCanonicalName() + " at " + locs.forNode(currentNode), e);
		}
		finally {
			reset();
		}
	}

	public ITuple parseCppToM3AndAst(ISourceLocation file, IString charset, IBool inferCharset, IList stdLib, IList includeDirs, IMap standardMacros, IMap additionalMacros,
			IBool includeStdLib) {
		this.includeStdLib = includeStdLib.getValue() || stdLib.isEmpty();
		this.stdLib = stdLib;

		IValue m3 = builder.M3_m3(file);
		ISourceLocation tuDecl = URIUtil.correctLocation("cpp+translationUnit", "", file.getPath());
		br.setTranslationUnit(tuDecl);
		CDTParser parser = new CDTParser(vf, stdOut, stdErr, stdLib, includeDirs, standardMacros, additionalMacros,
				includeStdLib.getValue());
		IASTTranslationUnit tu = parser.parseFileAsCpp(file, charset, inferCharset);
		IList comments = getCommentsFromTranslationUnit(tu);
		ISet macroExpansions = getMacroExpansionsFromTranslationUnit(tu);
		ISet macroDefinitions = getMacroDefinitionsFromTranslationUnit(tu);
		

		m3 = m3.asWithKeywordParameters().setParameter("comments", comments);
		m3 = m3.asWithKeywordParameters().setParameter("macroExpansions", macroExpansions);
		m3 = m3.asWithKeywordParameters().setParameter("macroDefinitions", macroDefinitions);
		m3 = setM3IncludeInformationFromTranslationUnit(tu, m3);

		declaredType = vf.setWriter();
		functionDefinitions = vf.setWriter();
		newResolutions = vf.setWriter();

		try {
			IValue result = convertCdtToRascal(tu, true);

			// based on side-effects from the conversion
			ISet methodOverrides = getMethodOverrides(tu, newResolutions.done());
			m3 = m3.asWithKeywordParameters().setParameter("methodOverrides", methodOverrides);
			
			result = ((IConstructor) result).asWithKeywordParameters().setParameter("decl", tuDecl);
			m3 = m3.asWithKeywordParameters().setParameter("declaredType", declaredType.done());
			m3 = m3.asWithKeywordParameters().setParameter("functionDefinitions", functionDefinitions.done());
			m3 = m3.asWithKeywordParameters().setParameter("containment", br.getContainmentRelation());
			m3 = m3.asWithKeywordParameters().setParameter("implicitDeclarations", implicitDeclarations.done());

			return vf.tuple(m3, result);
		}
		catch (NullPointerException e) {
			throw new UnsupportedOperationException(
				"Conversion to AST did not work for " + currentNode.getClass().getCanonicalName() + " at " + locs.forNode(currentNode), e);
		}
		finally {
			reset();
		}
	}

	public ISet getMethodOverrides(IASTTranslationUnit tu, ISet newResolutions) {
		NameCollector anc = new NameCollector();
		tu.accept(anc);
		Set<IBinding> bindings = new HashSet<>();
		Stream.of(anc.getNames()).forEach(it -> bindings.add(it.resolveBinding()));
		ISetWriter methodOverrides = vf.setWriter();

		// first we add all methods in classes that override some other methods
		bindings.stream().filter(ICPPMethod.class::isInstance).forEach(override -> {
			Stream.of(ClassTypeHelper.findOverridden((ICPPMethod) override)).forEach(base -> {
				try {
					// TODO: should it not be the origin of the declaring method AST?
					methodOverrides.insert(vf.tuple(br.resolveBinding(null, base, locs.forNode(tu)), br.resolveBinding(null, override, locs.forNode(tu))));
				} catch (FactTypeUseException e) {
					err("Got FactTypeUseException\n" + e.getMessage());
				}
			});
		});

		// then we add the template methods that may resolve to more specific methods after expansion
		bindings.stream()
			.filter(CPPFunctionSet.class::isInstance)
			.map(CPPFunctionSet.class::cast)
			.forEach(binding -> {
				// TODO: should it not be the origin of the declaring method AST?
				ISourceLocation base = br.resolveBinding(null, binding, locs.forNode(tu) );

				for (IBinding override : binding.getBindings()) {
					// TODO: should it not be the origin of the declaring method AST?
					methodOverrides.insert(
						vf.tuple(
							base, 
							br.resolveBinding(null, override, locs.forNode(tu))
						)
					);
				}
			});

		// finally we add a mapping from abstract constructor calls to their implementations
		methodOverrides.appendAll(newResolutions);
		return methodOverrides.done();
	}

	public ISet getMacroDefinitionsFromTranslationUnit(IASTTranslationUnit tu) {
		return Stream.of(tu.getMacroDefinitions()).map(it -> {
			return vf.tuple(br.resolveBinding(it, it.getName().resolveBinding(), locs.forNode(it)), locs.forNode(it));
		}).collect(vf.setWriter());
	}

	public IList getCommentsFromTranslationUnit(IASTTranslationUnit tu) {
		return Stream.of(tu.getComments()).map(locs::forNode).collect(vf.listWriter());
	}

	public IList parseForComments(ISourceLocation file, IString charset, IBool inferCharset, IList includePath, IMap standardMacros, IMap additionalMacros) {
		CDTParser parser = new CDTParser(vf, stdOut, stdErr, vf.listWriter().done(), includePath,
				standardMacros, additionalMacros, true);
		IASTTranslationUnit tu = parser.parseFileAsCpp(file, charset, inferCharset);
		reset();
		return getCommentsFromTranslationUnit(tu);
	}

	public IValue setM3IncludeInformationFromTranslationUnit(IASTTranslationUnit tu, IValue m3) {
		ISetWriter includeDirectives = vf.setWriter();
		ISetWriter inactiveIncludes = vf.setWriter();
		ISetWriter includeResolution = vf.setWriter();
		ISetWriter unresolvedIncludes = vf.setWriter();
		Stream.of(tu.getIncludeDirectives()).forEach(it -> {
			ISourceLocation include = BindingsResolver.failedBinding("unknown");
			try {
				include = vf.sourceLocation(it.isSystemInclude() ? "cpp+systemInclude" : "cpp+include", null,
						it.getName().toString());
			} catch (URISyntaxException e) {
				// Shouldn't happen
			}
			if (it.isActive()) {
				if (!it.isResolved()) {
					unresolvedIncludes.insert(vf.tuple(include, locs.forNode(it)));
				}
				includeDirectives.insert(vf.tuple(include, locs.forNode(it)));
				ISourceLocation path = "" == it.getPath() 
					? vf.sourceLocation(URIUtil.rootScheme("unresolved"))
					: vf.sourceLocation(it.getPath())
					;
				includeResolution.insert(vf.tuple(include, path));
			} else {
				inactiveIncludes.insert(vf.tuple(include, locs.forNode(it)));
			}
		});

		m3 = m3.asWithKeywordParameters().setParameter("includeDirectives", includeDirectives.done());
		m3 = m3.asWithKeywordParameters().setParameter("inactiveIncludes", inactiveIncludes.done());
		m3 = m3.asWithKeywordParameters().setParameter("includeResolution", includeResolution.done());
		m3 = m3.asWithKeywordParameters().setParameter("unresolvedIncludes", unresolvedIncludes.done());
		return m3;
	}

	public ISet getMacroExpansionsFromTranslationUnit(IASTTranslationUnit tu) {
		ISetWriter macros = vf.setWriter();
		Stream.of(tu.getMacroExpansions()).forEach(it -> {
			ISourceLocation decl = br.resolveBinding(null, it.getMacroReference().resolveBinding(), locs.forNode(it));
			macros.insert(vf.tuple(locs.forNode(it), decl));
		});
		return macros.done();
	}

	private void addDeclaredType(ISourceLocation decl, IConstructor typ) {
		if (toM3) {
			declaredType.insert(vf.tuple(decl, typ));
		}	
	}

	private void addFunctionDefinition(ISourceLocation decl, ISourceLocation loc) {
		if (toM3) {
			functionDefinitions.insert(vf.tuple(decl, loc));
		}
	}

	public ISet parseForMacros(ISourceLocation file, IString charset, IBool inferCharset, IList includePath, IMap standardMacros, IMap additionalMacros) {
		CDTParser parser = new CDTParser(vf, stdOut, stdErr, vf.listWriter().done(), includePath,
				standardMacros, additionalMacros, true);
		IASTTranslationUnit tu = parser.parseFileAsCpp(file, charset, inferCharset);
		reset();
		return getMacroExpansionsFromTranslationUnit(tu);
	}

	public IValue parseString(IString code) throws CoreException {
		return parseString(code, null);
	}

	public IValue parseString(IString code, ISourceLocation loc) throws CoreException {
		stdLib = vf.listWriter().done();
		FileContent fc = FileContent.create(loc == null ? "" : loc.toString(), code.getValue().toCharArray());
		IScannerInfo si = new ScannerInfo();
		IncludeFileContentProvider ifcp = IncludeFileContentProvider.getEmptyFilesProvider();
		int options = ILanguage.OPTION_PARSE_INACTIVE_CODE;
		IParserLogService log = new DefaultLogService();
		IASTTranslationUnit tu = GPPLanguage.getDefault().getASTTranslationUnit(fc, si, ifcp, null, options, log);

		try {
			IValue result = convertCdtToRascal(tu, false);
			ISourceLocation tuDecl = URIUtil.correctLocation("cpp+translationUnit", "", loc == null ? "" : loc.getPath());
			return ((IConstructor) result).asWithKeywordParameters().setParameter("decl", tuDecl);
		}
		catch (NullPointerException e) {
			throw new UnsupportedOperationException(
				"Conversion to AST did not work for " + currentNode.getClass().getCanonicalName() + " at " + locs.forNode(currentNode), e);
		}
		finally {
			reset();
		}
	}

	public IValue convertCdtToRascal(IASTTranslationUnit translationUnit, boolean toM3) {
		this.toM3 = toM3;
		translationUnit.accept(this);

		if (stack.size() == 1) {
			return stack.pop();
		}

		if (stack.size() == 0) {
			throw new RuntimeException("Stack empty after converting, error");
		}

		IConstructor ast = stack.pop();
		err("Superfluous nodes on the stack after converting:");
		stack.iterator().forEachRemaining(it -> err(it.toString()));
		
		ISourceLocation file = locs.forNode(translationUnit);
		ISourceLocation tuDecl = URIUtil.correctLocation("cpp+translationUnit", "", file.getPath());
		return ast.asWithKeywordParameters().setParameter("decl", tuDecl);
	}

	private int prefix = 0;
	
	private String spaces() {
		return StringUtils.repeat(" ", prefix);
	}

	private void out(String msg) {
		stdOut.println(spaces() + msg.replace("\n", "\n" + spaces()));
	}

	private void err(String msg) {
		stdErr.println(spaces() + msg.replace("\n", "\n" + spaces()));
	}

	public ISourceLocation getTokenSourceLocation(IASTNode node, String literal) {
		ISourceLocation loc = locs.forNode(node);
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

	public boolean isMacroExpansion(IASTNode node) {
		IASTNodeLocation[] nodeLocations = node.getNodeLocations();
		return nodeLocations.length > 1
				|| nodeLocations.length == 1 && nodeLocations[0] instanceof IASTMacroExpansionLocation;
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
		boolean isMacroExpansion = isMacroExpansion(node);
		IListWriter modifiers = vf.listWriter();

		if (node instanceof ICPPASTDeclSpecifier) {
			if (((ICPPASTDeclSpecifier) node).isFriend())
				modifiers.append(builder.Modifier_friend(getTokenSourceLocation(node, "friend"), isMacroExpansion));
			if (((ICPPASTDeclSpecifier) node).isVirtual())
				modifiers.append(builder.Modifier_virtual(getTokenSourceLocation(node, "virtual"), isMacroExpansion));
			if (((ICPPASTDeclSpecifier) node).isExplicit())
				modifiers.append(builder.Modifier_explicit(getTokenSourceLocation(node, "explicit"), isMacroExpansion));
			if (((ICPPASTDeclSpecifier) node).isConstexpr())
				modifiers.append(
						builder.Modifier_constexpr(getTokenSourceLocation(node, "constexpr"), isMacroExpansion));
			if (((ICPPASTDeclSpecifier) node).isThreadLocal())
				modifiers.append(
						builder.Modifier_threadLocal(getTokenSourceLocation(node, "thread_local"), isMacroExpansion));
		}

		if (node instanceof ICPPASTFunctionDeclarator) {
			if (((ICPPASTFunctionDeclarator) node).isMutable())
				modifiers.append(builder.Modifier_mutable(getTokenSourceLocation(node, "mutable"), isMacroExpansion));
			if (((ICPPASTFunctionDeclarator) node).isPureVirtual())
				modifiers.append(
						builder.Modifier_pureVirtual(getTokenSourceLocation(node, "virtual"), isMacroExpansion));// check
		}

		if (node instanceof IASTDeclSpecifier) {
			switch (((IASTDeclSpecifier) node).getStorageClass()) {
			case IASTDeclSpecifier.sc_typedef:
				modifiers.append(builder.Modifier_typedef(getTokenSourceLocation(node, "typedef"), isMacroExpansion));
				break;
			case IASTDeclSpecifier.sc_extern:
				modifiers.append(builder.Modifier_extern(getTokenSourceLocation(node, "extern"), isMacroExpansion));
				break;
			case IASTDeclSpecifier.sc_static:
				modifiers.append(builder.Modifier_static(getTokenSourceLocation(node, "static"), isMacroExpansion));
				break;
			case IASTDeclSpecifier.sc_auto:
				modifiers.append(builder.Modifier_modAuto(getTokenSourceLocation(node, "auto"), isMacroExpansion));
				break;
			case IASTDeclSpecifier.sc_register:
				modifiers.append(builder.Modifier_register(getTokenSourceLocation(node, "register"), isMacroExpansion));
				break;
			case IASTDeclSpecifier.sc_mutable:
				modifiers.append(builder.Modifier_mutable(getTokenSourceLocation(node, "mutable"), isMacroExpansion));
				break;
			}
		}

		if (node instanceof IASTSimpleDeclSpecifier) {
			if (((IASTSimpleDeclSpecifier) node).isSigned())
				modifiers.append(builder.Modifier_signed(getTokenSourceLocation(node, "signed"), isMacroExpansion));
			if (((IASTSimpleDeclSpecifier) node).isUnsigned())
				modifiers.append(builder.Modifier_unsigned(getTokenSourceLocation(node, "unsigned"), isMacroExpansion));
			if (((IASTSimpleDeclSpecifier) node).isShort())
				modifiers.append(builder.Modifier_short(getTokenSourceLocation(node, "short"), isMacroExpansion));
			if (((IASTSimpleDeclSpecifier) node).isLong())
				modifiers.append(builder.Modifier_long(getTokenSourceLocation(node, "long"), isMacroExpansion));
			if (((IASTSimpleDeclSpecifier) node).isLongLong())
				modifiers
						.append(builder.Modifier_longlong(getTokenSourceLocation(node, "long long"), isMacroExpansion));
			if (((IASTSimpleDeclSpecifier) node).isComplex())
				modifiers.append(builder.Modifier_complex(getTokenSourceLocation(node, "_Complex"), isMacroExpansion));
			if (((IASTSimpleDeclSpecifier) node).isImaginary())
				modifiers.append(
						builder.Modifier_imaginary(getTokenSourceLocation(node, "_Imaginary"), isMacroExpansion));
		}

		if (node instanceof ICASTArrayModifier) {
			if (((ICASTArrayModifier) node).isConst())
				modifiers.append(builder.Modifier_const(getTokenSourceLocation(node, "const"), isMacroExpansion));
			if (((ICASTArrayModifier) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(getTokenSourceLocation(node, "volatile"), isMacroExpansion));
			if (((ICASTArrayModifier) node).isRestrict())
				modifiers.append(builder.Modifier_restrict(getTokenSourceLocation(node, "restrict"), isMacroExpansion));
			if (((ICASTArrayModifier) node).isConst())
				modifiers.append(builder.Modifier_const(getTokenSourceLocation(node, "const"), isMacroExpansion));
		} else if (node instanceof ICPPASTFunctionDeclarator) {
			if (((ICPPASTFunctionDeclarator) node).isConst())
				modifiers.append(builder.Modifier_const(getTokenSourceLocation(node, "const"), isMacroExpansion));
			if (((ICPPASTFunctionDeclarator) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(getTokenSourceLocation(node, "volatile"), isMacroExpansion));
		} else if (node instanceof IASTDeclSpecifier) {
			if (((IASTDeclSpecifier) node).isConst())
				modifiers.append(builder.Modifier_const(getTokenSourceLocation(node, "const"), isMacroExpansion));
			if (((IASTDeclSpecifier) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(getTokenSourceLocation(node, "volatile"), isMacroExpansion));
			if (((IASTDeclSpecifier) node).isRestrict())
				modifiers.append(builder.Modifier_restrict(getTokenSourceLocation(node, "restrict"), isMacroExpansion));
			if (((IASTDeclSpecifier) node).isInline())
				modifiers.append(builder.Modifier_inline(getTokenSourceLocation(node, "inline"), isMacroExpansion));
		} else if (node instanceof IASTPointer) {
			if (((IASTPointer) node).isConst())
				modifiers.append(builder.Modifier_const(getTokenSourceLocation(node, "const"), isMacroExpansion));
			if (((IASTPointer) node).isVolatile())
				modifiers.append(builder.Modifier_volatile(getTokenSourceLocation(node, "volatile"), isMacroExpansion));
			if (((IASTPointer) node).isRestrict())
				modifiers.append(builder.Modifier_restrict(getTokenSourceLocation(node, "restrict"), isMacroExpansion));
		} else if (node instanceof ICPPASTNamespaceDefinition) {
			if (((ICPPASTNamespaceDefinition) node).isInline())
				modifiers.append(builder.Modifier_inline(getTokenSourceLocation(node, "inline"), isMacroExpansion));
		}

		if (node instanceof ICPPASTNamedTypeSpecifier) {
			if (((ICPPASTNamedTypeSpecifier) node).isTypename())
				modifiers.append(builder.Modifier_typename(getTokenSourceLocation(node, "typename"), isMacroExpansion));
		} else if (node instanceof ICPPASTUsingDeclaration)
			if (((ICPPASTUsingDeclaration) node).isTypename())
				modifiers.append(builder.Modifier_typename(getTokenSourceLocation(node, "typename"), isMacroExpansion));

		return modifiers.done().stream()
				.sorted((v1, v2) -> ((ISourceLocation) v1.asWithKeywordParameters().getParameter("src")).getOffset()
						- ((ISourceLocation) v2.asWithKeywordParameters().getParameter("src")).getOffset())
				.collect(vf.listWriter());
	}

	@Override
	public int visit(IASTTranslationUnit tu) {
		at(tu);
		ISourceLocation loc = locs.forNode(tu);
		boolean isMacroExpansion = isMacroExpansion(tu);
		IListWriter declarations = vf.listWriter();
		ISourceLocation tuLoc = locs.forNode(tu);
		ISourceLocation tuDecl = URIUtil.correctLocation("cpp+translationUnit", "", tuLoc.getPath());
		
		monitor.jobStart("ClaiR AST marshalling");
		declLoop: for (IASTDeclaration declaration : tu.getDeclarations()) {
			if (monitor.jobIsCanceled("ClaiR AST marshalling")
					|| monitor instanceof Evaluator && ((Evaluator) monitor).isInterrupted()) {
				declarations.append(builder.Declaration_problemDeclaration(
						URIUtil.rootLocation("interrupted"), isMacroExpansion));
				monitor.jobEnd("ClaiR AST marshalling", false);
				break;
			}
			ISourceLocation declLoc = locs.forNode(declaration);
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

		IConstructor translationUnit = builder.Declaration_translationUnit(declarations.done(), loc, isMacroExpansion);
		translationUnit = translationUnit.asWithKeywordParameters().setParameter("decl", tuDecl);

		stack.push(translationUnit);
		monitor.jobEnd("ClaiR AST marshalling", true);
		return PROCESS_ABORT;
	}

	// Names

	@Override
	public int visit(IASTName name) {
		at(name);
		
		try {
			if (name instanceof IASTImplicitName) {
				visit((IASTImplicitName) name);
			}
			else if (name instanceof ICPPASTName) {
				visit((ICPPASTName) name);
			}
			else if (name instanceof CASTName) {
				ISourceLocation loc = locs.forNode(name);
				boolean isMacroExpansion = isMacroExpansion(name);
				stack.push(builder.Name_name(name.toString(), loc, isMacroExpansion));
				return PROCESS_ABORT;
			} else {
				err("No sub-interfaced IASTName? " + name.getClass().getName() + ": " + name.getRawSignature());
				throw new RuntimeException("NYI at " + locs.forNode(name));
			}
		}
		catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}

		return PROCESS_ABORT;
	}

	private int visit(IASTImplicitName name) {
		err("IASTImplicitName " + name.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(name));
	}

	private int visit(ICPPASTName name) throws URISyntaxException {
		at(name);
		ISourceLocation loc = locs.forNode(name);
		boolean isMacroExpansion = isMacroExpansion(name);
		if (name instanceof ICPPASTConversionName)
			visit((ICPPASTConversionName) name);
		else if (name instanceof ICPPASTOperatorName)
			visit((ICPPASTOperatorName) name);
		else if (name instanceof ICPPASTQualifiedName)
			visit((ICPPASTQualifiedName) name);
		else if (name instanceof ICPPASTTemplateId)
			visit((ICPPASTTemplateId) name);
		else {
			stack.push(builder.Name_name(new String(name.toCharArray()), loc, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTConversionName name) {
		ISourceLocation loc = locs.forNode(name);
		boolean isMacroExpansion = isMacroExpansion(name);
		IConstructor typ = tr.resolveType(name);
		name.getTypeId().accept(this);
		stack.push(builder.Name_conversionName(name.toString(), stack.pop(), loc, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTOperatorName name) {
		at(name);
		ISourceLocation loc = locs.forNode(name);
		boolean isMacroExpansion = isMacroExpansion(name);
		stack.push(builder.Name_operatorName(name.toString(), loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTQualifiedName name) throws URISyntaxException {
		at(name);
		ISourceLocation loc = locs.forNode(name);
		boolean isMacroExpansion = isMacroExpansion(name);
		ISourceLocation decl = br.resolveBinding(name, loc);

		IListWriter qualifierWriter = vf.listWriter();
		Stream.of(name.getQualifier()).forEach(it -> {
			it.accept(this);
			qualifierWriter.append(stack.pop());
		});

		IList qualifiers = qualifierWriter.done();
		
		name.getLastName().accept(this);
		IConstructor lastName = stack.pop();
		// TODO: check fullyQualified
		if (name.isFullyQualified())
			;

		stack.push(builder.Name_qualifiedName(qualifiers, lastName, loc, decl, isMacroExpansion));
		
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTTemplateId name) throws URISyntaxException {
		at(name);
		ISourceLocation loc = locs.forNode(name);
		boolean isMacroExpansion = isMacroExpansion(name);
		ISourceLocation decl = br.resolveBinding(name, loc);

		name.getTemplateName().accept(this);
		IConstructor templateName = stack.pop();
		IListWriter templateArguments = vf.listWriter();
		Stream.of(name.getTemplateArguments()).forEach(it -> {
			it.accept(this);
			templateArguments.append(stack.pop());
		});
		stack.push(builder.Name_templateId(templateName, templateArguments.done(), loc, decl, isMacroExpansion));
		return PROCESS_ABORT;
	}

	// Declarations

	@Override
	public int visit(IASTDeclaration declaration) {
		at(declaration);
		if (declaration instanceof IASTASMDeclaration) {
			visit((IASTASMDeclaration) declaration);
		}
		else if (declaration instanceof IASTFunctionDefinition) {
			visit((IASTFunctionDefinition) declaration);
		}
		else if (declaration instanceof IASTSimpleDeclaration) {
			visit((IASTSimpleDeclaration) declaration);
		}
		else if (declaration instanceof ICPPASTAliasDeclaration) {
			visit((ICPPASTAliasDeclaration) declaration);
		}
		else if (declaration instanceof ICPPASTExplicitTemplateInstantiation) {
			visit((ICPPASTExplicitTemplateInstantiation) declaration);
		}
		else if (declaration instanceof ICPPASTLinkageSpecification) {
			visit((ICPPASTLinkageSpecification) declaration);
		}
		else if (declaration instanceof ICPPASTNamespaceAlias) {
			visit((ICPPASTNamespaceAlias) declaration);
		}
		// In ASTVisitor interface, not needed?
		// else if (declaration instanceof ICPPASTNamespaceDefinition)
		// visit((ICPPASTNamespaceDefinition) declaration);
		else if (declaration instanceof ICPPASTStaticAssertDeclaration) {
			visit((ICPPASTStaticAssertDeclaration) declaration);
		}
		else if (declaration instanceof ICPPASTTemplateSpecialization) {
			visit((ICPPASTTemplateSpecialization) declaration);
		}
		else if (declaration instanceof ICPPASTTemplateDeclaration) {
			visit((ICPPASTTemplateDeclaration) declaration);
		}
		else if (declaration instanceof ICPPASTUsingDeclaration) {
			visit((ICPPASTUsingDeclaration) declaration);
		}
		else if (declaration instanceof ICPPASTUsingDirective) {
			visit((ICPPASTUsingDirective) declaration);
		}
		else if (declaration instanceof ICPPASTVisibilityLabel) {
			visit((ICPPASTVisibilityLabel) declaration);
		}
		else if (declaration instanceof IASTProblemDeclaration) {
			// should not happen
			visit((IASTProblemDeclaration) declaration);
		}
		else {
			throw new RuntimeException("Declaration: encountered non-implemented subtype "
					+ declaration.getClass().getName() + " at " + locs.forNode(declaration));
		}

		return PROCESS_ABORT;
	}

	private int visit(ICPPASTAliasDeclaration declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		ISourceLocation decl = br.resolveBinding(declaration, loc);
		IList attributes = getAttributes(declaration);

		declaration.getAlias().accept(this);
		IConstructor alias = stack.pop();
		declaration.getMappingTypeId().accept(this);
		IConstructor mappingTypeId = stack.pop();
		stack.push(builder.Declaration_alias(alias, mappingTypeId, attributes, loc, decl, isMacroExpansion));

		return PROCESS_ABORT;
	}

	private int visit(ICPPASTExplicitTemplateInstantiation declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		declaration.getDeclaration().accept(this);
		switch (declaration.getModifier()) {
		case 0:
			stack.push(builder.Declaration_explicitTemplateInstantiation(stack.pop(), loc, isMacroExpansion));
			break;
		case ICPPASTExplicitTemplateInstantiation.STATIC:
			stack.push(builder.Declaration_explicitTemplateInstantiation(
					builder.Modifier_static(getTokenSourceLocation(declaration, "static"), isMacroExpansion),
					stack.pop(), loc, isMacroExpansion));
			break;
		case ICPPASTExplicitTemplateInstantiation.INLINE:
			stack.push(builder.Declaration_explicitTemplateInstantiation(
					builder.Modifier_inline(getTokenSourceLocation(declaration, "inline"), isMacroExpansion),
					stack.pop(), loc, isMacroExpansion));
			break;
		case ICPPASTExplicitTemplateInstantiation.EXTERN:
			stack.push(builder.Declaration_explicitTemplateInstantiation(
					builder.Modifier_extern(getTokenSourceLocation(declaration, "extern"), isMacroExpansion),
					stack.pop(), loc, isMacroExpansion));
			break;
		default:
			throw new RuntimeException("ICPPASTExplicitTemplateInstantiation encountered unknown modifier "
					+ declaration.getModifier() + " at " + locs.forNode(declaration));
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTLinkageSpecification declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);

		IListWriter declarations = vf.listWriter();
		Stream.of(declaration.getDeclarations()).forEach(it -> {
			it.accept(this);
			declarations.append(stack.pop());
		});
		stack.push(builder.Declaration_linkageSpecification(declaration.getLiteral(), declarations.done(), loc,
				isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTNamespaceAlias declaration)  {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		ISourceLocation decl = br.resolveBinding(declaration, loc);

		declaration.getAlias().accept(this);
		IConstructor alias = stack.pop();
		declaration.getMappingName().accept(this);
		IConstructor mappingName = stack.pop();
		stack.push(builder.Declaration_namespaceAlias(alias, mappingName, loc, decl, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTStaticAssertDeclaration declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		declaration.getCondition().accept(this);
		IConstructor condition = stack.pop();
		ICPPASTLiteralExpression msg = declaration.getMessage();
		if (msg != null) {
			msg.accept(this);
			stack.push(builder.Declaration_staticAssert(condition, stack.pop(), loc, isMacroExpansion));
		}
		else {
			stack.push(builder.Declaration_staticAssert(condition, loc, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTTemplateDeclaration declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		IConstructor typ = tr.resolveType(declaration);
		// The "export" keyword has been removed from the C++ standard
		IListWriter templateParameters = vf.listWriter();
		Stream.of(declaration.getTemplateParameters()).forEach(it -> {
			it.accept(this);
			templateParameters.append(stack.pop());
		});
		declaration.getDeclaration().accept(this);
		stack.push(builder.Declaration_template(templateParameters.done(), stack.pop(), typ, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTTemplateSpecialization declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		declaration.getDeclaration().accept(this);
		stack.push(builder.Declaration_explicitTemplateSpecialization(stack.pop(), loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTUsingDeclaration declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		ISourceLocation decl = br.resolveBinding(declaration, loc);
		IList attributes = getAttributes(declaration);
		IList modifiers = getModifiers(declaration);
		declaration.getName().accept(this);
		stack.push(
				builder.Declaration_usingDeclaration(modifiers, stack.pop(), attributes, loc, decl, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTUsingDirective declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		ISourceLocation decl = br.resolveBinding(declaration, loc);
		IList attributes = getAttributes(declaration);
		IASTName qualifiedName = declaration.getQualifiedName();
		qualifiedName.accept(this);
		stack.push(builder.Declaration_usingDirective(stack.pop(), attributes, loc, decl, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTVisibilityLabel declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		switch (declaration.getVisibility()) {
		case ICPPASTVisibilityLabel.v_public:
			stack.push(builder.Declaration_visibilityLabel(
					builder.Modifier_public(getTokenSourceLocation(declaration, "public"), isMacroExpansion), loc,
					isMacroExpansion));
			break;
		case ICPPASTVisibilityLabel.v_protected:
			stack.push(builder.Declaration_visibilityLabel(
					builder.Modifier_protected(getTokenSourceLocation(declaration, "protected"), isMacroExpansion), loc,
					isMacroExpansion));
			break;
		case ICPPASTVisibilityLabel.v_private:
			stack.push(builder.Declaration_visibilityLabel(
					builder.Modifier_private(getTokenSourceLocation(declaration, "private"), isMacroExpansion), loc,
					isMacroExpansion));
			break;
		default:
			throw new RuntimeException("Unknown CPPVisibilityLabel code " + declaration.getVisibility() + " at "
					+ locs.forNode(declaration) + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTASMDeclaration declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
		stack.push(builder.Declaration_asmDeclaration(declaration.getAssembly(), loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTFunctionDefinition definition) {
		at(definition);
		ISourceLocation loc = locs.forNode(definition);
		boolean isMacroExpansion = isMacroExpansion(definition);
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

			if (isDefaulted && isDeleted) {
				err("WARNING: IASTFunctionDefinition both deleted and defaulted");
			}

			if ((isDefaulted || isDeleted) && definition instanceof ICPPASTFunctionWithTryBlock) {
				throw new RuntimeException("IASTFunctionDefinition defaulted/deleted and with try? at " + loc);
			}

			if (isDefaulted) {
				stack.push(builder.Declaration_defaultedFunctionDefinition(declSpecifier, memberInitializers.done(),
						declarator, attributes, loc, isMacroExpansion));
			}
			else if (isDeleted) {
				stack.push(builder.Declaration_deletedFunctionDefinition(declSpecifier, memberInitializers.done(),
						declarator, attributes, loc, isMacroExpansion));
			}
			else if (definition instanceof ICPPASTFunctionWithTryBlock) {
				IListWriter catchHandlers = vf.listWriter();
				Stream.of(((ICPPASTFunctionWithTryBlock) definition).getCatchHandlers()).forEach(it -> {
					it.accept(this);
					catchHandlers.append(stack.pop());
				});
				definition.getBody().accept(this);
				stack.push(builder.Declaration_functionWithTryBlockDefinition(declSpecifier, declarator,
						memberInitializers.done(), stack.pop(), catchHandlers.done(), attributes, loc,
						isMacroExpansion));
			} 
			else {
				definition.getBody().accept(this);
				stack.push(builder.Declaration_functionDefinition(declSpecifier, declarator, memberInitializers.done(),
						stack.pop(), attributes, loc, isMacroExpansion));
			}
		} 
		else { // C Function definition
			// TODO: add separate AST entry and remove fixed empty memberinitializers and
			// attributes
			definition.getDeclSpecifier().accept(this);
			IConstructor declSpecifier = stack.pop();
			definition.getDeclarator().accept(this);
			IConstructor declarator = stack.pop();

			definition.getBody().accept(this);
			stack.push(builder.Declaration_functionDefinition(declSpecifier, declarator, vf.listWriter().done(),
					stack.pop(), vf.listWriter().done(), loc, isMacroExpansion));
		}
		
		addDeclaredType(br.resolveBinding(definition.getDeclarator(), locs.forNode(definition.getDeclarator())), tr.resolveType(definition.getDeclarator()));
		addFunctionDefinition(br.resolveBinding(definition.getDeclarator(), loc), loc);
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTParameterDeclaration parameterDeclaration) {
		at(parameterDeclaration);
		ISourceLocation loc = locs.forNode(parameterDeclaration);
		boolean isMacroExpansion = isMacroExpansion(parameterDeclaration);
		if (parameterDeclaration instanceof ICPPASTParameterDeclaration
				|| parameterDeclaration instanceof CASTParameterDeclaration) {
			// TODO: add isParameterPack() for ICPPASTParameterDeclaration
			parameterDeclaration.getDeclSpecifier().accept(this);
			IConstructor declSpecifier = stack.pop();
			if (parameterDeclaration.getDeclarator() == null)
				stack.push(builder.Declaration_parameter(declSpecifier, loc, isMacroExpansion));
			else {
				parameterDeclaration.getDeclarator().accept(this);
				stack.push(builder.Declaration_parameter(declSpecifier, stack.pop(), loc, isMacroExpansion));
			}
		} else {
			throw new RuntimeException("NYI: ParameterDeclaration at " + loc);
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTProblemDeclaration declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		boolean isMacroExpansion = isMacroExpansion(declaration);
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
		stack.push(builder.Declaration_problemDeclaration(loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTSimpleDeclaration declaration) {
		at(declaration);
		ISourceLocation loc = locs.forNode(declaration);
		
		boolean isMacroExpansion = isMacroExpansion(declaration);
		IList attributes = getAttributes(declaration);

		declaration.getDeclSpecifier().accept(this);
		IConstructor declSpecifier = stack.pop();

		IListWriter declarators = vf.listWriter();
		Stream.of(declaration.getDeclarators()).forEach(it -> {
			it.accept(this);
			declarators.append(stack.pop());
			addDeclaredType(br.resolveBinding(it, locs.forNode(it)), tr.resolveType(it));
		});
		stack.push(builder.Declaration_simpleDeclaration(declSpecifier, declarators.done(), attributes, loc,
				isMacroExpansion));
		
		
		return PROCESS_ABORT;
	}

	// Initializers

	@Override
	public int visit(IASTInitializer initializer) {
		at(initializer);
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
					+ initializer.getClass().getSimpleName() + " at " + locs.forNode(initializer));
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTEqualsInitializer initializer) {
		at(initializer);
		ISourceLocation loc = locs.forNode(initializer);
		boolean isMacroExpansion = isMacroExpansion(initializer);
		initializer.getInitializerClause().accept(this);
		stack.push(builder.Expression_equalsInitializer(stack.pop(), loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTInitializerList initializer) {
		at(initializer);
		// TODO: cpp: check isPackExpansion, maybe getSize
		ISourceLocation loc = locs.forNode(initializer);
		boolean isMacroExpansion = isMacroExpansion(initializer);
		IListWriter clauses = vf.listWriter();
		Stream.of(initializer.getClauses()).forEach(it -> {
			it.accept(this);
			clauses.append(stack.pop());
		});
		stack.push(builder.Expression_initializerList(clauses.done(), loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICASTDesignatedInitializer initializer) {
		at(initializer);
		ISourceLocation loc = locs.forNode(initializer);
		boolean isMacroExpansion = isMacroExpansion(initializer);

		IListWriter designators = vf.listWriter();
		Stream.of(initializer.getDesignators()).forEach(it -> {
			it.accept(this);
			designators.append(stack.pop());
		});

		initializer.getOperand().accept(this);
		stack.push(builder.Expression_designatedInitializer(designators.done(), stack.pop(), loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTConstructorChainInitializer initializer) {
		at(initializer);
		// TODO: check isPackExpansion
		ISourceLocation loc = locs.forNode(initializer);
		boolean isMacroExpansion = isMacroExpansion(initializer);
		ISourceLocation decl = br.resolveBinding(initializer, loc);

		initializer.getMemberInitializerId().accept(this);
		IConstructor memberInitializerId = stack.pop();
		initializer.getInitializer().accept(this);
		IConstructor memberInitializer = stack.pop();

		stack.push(builder.Expression_constructorChainInitializer(memberInitializerId, memberInitializer, loc, decl,
				isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTConstructorInitializer initializer) {
		at(initializer);
		ISourceLocation loc = locs.forNode(initializer);
		boolean isMacroExpansion = isMacroExpansion(initializer);

		IListWriter arguments = vf.listWriter();
		Stream.of(initializer.getArguments()).forEach(it -> {
			it.accept(this);
			arguments.append(stack.pop());
		});

		stack.push(builder.Expression_constructorInitializer(arguments.done(), loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTDesignatedInitializer initializer) {
		at(initializer);
		ISourceLocation loc = locs.forNode(initializer);
		boolean isMacroExpansion = isMacroExpansion(initializer);

		IListWriter designators = vf.listWriter();
		Stream.of(initializer.getDesignators()).forEach(it -> {
			it.accept(this);
			designators.append(stack.pop());
		});

		initializer.getOperand().accept(this);
		stack.push(builder.Expression_designatedInitializer(designators.done(), stack.pop(), loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	// Declarators

	@Override
	public int visit(IASTDeclarator declarator) {
		at(declarator);
		if (declarator instanceof IASTArrayDeclarator) {
			visit((IASTArrayDeclarator) declarator);
		}
		else if (declarator instanceof IASTFunctionDeclarator) {
			visit((IASTFunctionDeclarator) declarator);
		}
		else if (declarator instanceof ICPPASTDeclarator) {
			visit((ICPPASTDeclarator) declarator);
		}
		else if (declarator instanceof CASTDeclarator) {
			visit((CASTDeclarator) declarator);
		} else {
			throw new RuntimeException("Unknown IASTDeclarator subclass " + declarator.getClass().getName() + " at "
					+ locs.forNode(declarator));
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTArrayDeclarator declarator) {
		at(declarator);
		ISourceLocation loc = locs.forNode(declarator);
		boolean isMacroExpansion = isMacroExpansion(declarator);
		ISourceLocation decl = br.resolveBinding(declarator, loc);
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
						attributes, loc, decl, isMacroExpansion));
			else {
				declarator.getInitializer().accept(this);
				stack.push(builder.Declarator_arrayDeclarator(pointerOperators.done(), name, arrayModifiers.done(),
						stack.pop(), attributes, loc, decl, isMacroExpansion));
			}
		} else {
			declarator.getNestedDeclarator().accept(this);
			IConstructor nestedDeclarator = stack.pop();
			if (declarator.getInitializer() == null)
				stack.push(builder.Declarator_arrayDeclaratorNested(pointerOperators.done(), nestedDeclarator,
						arrayModifiers.done(), attributes, loc, decl, isMacroExpansion));
			else {
				declarator.getInitializer().accept(this);
				stack.push(builder.Declarator_arrayDeclaratorNested(pointerOperators.done(), nestedDeclarator,
						arrayModifiers.done(), stack.pop(), attributes, loc, decl, isMacroExpansion));
			}
		}

		return PROCESS_ABORT;
	}

	private int visit(IASTFunctionDeclarator declarator) {
		at(declarator);
		if (declarator instanceof IASTStandardFunctionDeclarator) {
			visit((IASTStandardFunctionDeclarator) declarator);
		}
		else if (declarator instanceof ICASTKnRFunctionDeclarator) {
			visit((ICASTKnRFunctionDeclarator) declarator);
		}
		else {
			throw new RuntimeException("Unknown FunctionDeclarator subtype " + declarator.getClass().getName() + " at "
					+ locs.forNode(declarator) + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTStandardFunctionDeclarator declarator) {
		at(declarator);

		if (declarator instanceof ICPPASTFunctionDeclarator)
			visit((ICPPASTFunctionDeclarator) declarator);
		else if (declarator instanceof CASTFunctionDeclarator)
			visit((CASTFunctionDeclarator) declarator);
		else {
			throw new RuntimeException("Unknown StandardFunctionDeclarator subtype " + declarator.getClass().getName()
					+ " at " + locs.forNode(declarator) + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	private int visit(ICASTKnRFunctionDeclarator declarator) {
		at(declarator);

		// TODO: check nestedDeclarator and initializer
		ISourceLocation loc = locs.forNode(declarator);
		boolean isMacroExpansion = isMacroExpansion(declarator);
		ISourceLocation decl = br.resolveBinding(declarator, loc);
		IList modifiers = getModifiers(declarator);

		// TODO: fix when name == null
//		IConstructor name = builder.Name_name("", vf.sourceLocation(loc, loc.getOffset(), 0), isMacroExpansion);
//		IASTName _name = declarator.getName();
//		if (_name != null) {
//			_name.accept(this);
//			name = stack.pop();
//		}

		IListWriter parameterNames = vf.listWriter();
		Stream.of(declarator.getParameterNames()).forEach(it -> {
			it.accept(this);
			parameterNames.append(stack.pop());
		});

		IListWriter parameterDeclarations = vf.listWriter();
		Stream.of(declarator.getParameterDeclarations()).forEach(it -> {
			it.accept(this);
			parameterDeclarations.append(stack.pop());
		});

		IListWriter pointerOperators = vf.listWriter();
		Stream.of(declarator.getPointerOperators()).forEach(it -> {
			it.accept(this);
			pointerOperators.append(stack.pop());
		});

		stack.push(builder.Declarator_knrFunctionDeclarator(pointerOperators.done(), modifiers, parameterNames.done(),
				parameterDeclarations.done(), loc, decl, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(CASTDeclarator declarator) {
		at(declarator);

		// TODO: deduplicate with ICPPASTDeclarator
		ISourceLocation loc = locs.forNode(declarator);
		boolean isMacroExpansion = isMacroExpansion(declarator);
		ISourceLocation decl = br.resolveBinding(declarator, loc);
		IList attributes = getAttributes(declarator);

		declarator.getName().accept(this);
		IConstructor name = stack.pop();

		IListWriter pointerOperators = vf.listWriter();
		Stream.of(declarator.getPointerOperators()).forEach(it -> {
			it.accept(this);
			pointerOperators.append(stack.pop());
		});

		IASTInitializer initializer = declarator.getInitializer();
		if (initializer == null) {
			stack.push(builder.Declarator_declarator(pointerOperators.done(), name, attributes, loc, decl,
					isMacroExpansion));
		} else {
			initializer.accept(this);
			stack.push(builder.Declarator_declarator(pointerOperators.done(), name, stack.pop(), attributes, loc, decl,
					isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTDeclarator declarator) {
		at(declarator);

		if (declarator instanceof ICPPASTArrayDeclarator) {
			visit((ICPPASTArrayDeclarator) declarator);
		}
		else if (declarator instanceof ICPPASTFieldDeclarator) {
			visit((ICPPASTFieldDeclarator) declarator);
		}
		else if (declarator instanceof ICPPASTFunctionDeclarator) {
			visit((ICPPASTFunctionDeclarator) declarator);
		}
		else {
			ISourceLocation loc = locs.forNode(declarator);
			boolean isMacroExpansion = isMacroExpansion(declarator);
			ISourceLocation decl = br.resolveBinding(declarator, loc);
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
				stack.push(builder.Declarator_declarator(pointerOperators.done(), name, attributes, loc, decl,
						isMacroExpansion));
			} else {
				initializer.accept(this);
				stack.push(builder.Declarator_declarator(pointerOperators.done(), name, stack.pop(), attributes, loc,
						decl, isMacroExpansion));
			}
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTArrayDeclarator declarator) {
		at(declarator);

		visit((IASTArrayDeclarator) declarator);
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTFieldDeclarator declarator) {
		at(declarator);

		ISourceLocation loc = locs.forNode(declarator);
		boolean isMacroExpansion = isMacroExpansion(declarator);
		ISourceLocation decl = br.resolveBinding(declarator, loc);
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
					decl, isMacroExpansion));
		} else {
			initializer.accept(this);
			stack.push(builder.Declarator_fieldDeclarator(pointerOperators.done(), name, bitFieldSize, stack.pop(),
					attributes, loc, decl, isMacroExpansion));
		}

		return PROCESS_ABORT;
	}

	private int visit(CASTFunctionDeclarator declarator) {
		at(declarator);
		// TODO: check nestedDeclarator and initializer
		// TODO: add separate AST node, remove fixed empty virtSpecifiers
		// exceptionSpecifiers
		ISourceLocation loc = locs.forNode(declarator);
		boolean isMacroExpansion = isMacroExpansion(declarator);
		ISourceLocation decl = br.resolveBinding(declarator, loc);
//		IConstructor typ = tr.resolveType(declarator);
		IList attributes = getAttributes(declarator);
		IList modifiers = getModifiers(declarator);

		// TODO: fix when name == null
		IConstructor name = builder.Name_name("", vf.sourceLocation(loc, loc.getOffset(), 0), isMacroExpansion);
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
			parameters.append(builder.Declaration_varArgs(getTokenSourceLocation(declarator, "..."), isMacroExpansion));

		IListWriter pointerOperators = vf.listWriter();
		Stream.of(declarator.getPointerOperators()).forEach(it -> {
			it.accept(this);
			pointerOperators.append(stack.pop());
		});

		stack.push(builder.Declarator_functionDeclarator(pointerOperators.done(), modifiers, name, parameters.done(),
				vf.listWriter().done(), attributes, loc, decl, isMacroExpansion));

		return PROCESS_ABORT;
	}

	private int visit(ICPPASTFunctionDeclarator declarator) {
		at(declarator);

		// TODO: check refQualifier and declaresParameterPack
		ISourceLocation loc = locs.forNode(declarator);
		boolean isMacroExpansion = isMacroExpansion(declarator);
		ISourceLocation decl = br.resolveBinding(declarator, loc);
//		IConstructor typ = tr.resolveType(declarator);
		IList attributes = getAttributes(declarator);
		IList modifiers = getModifiers(declarator);

		IASTDeclarator _nestedDeclarator = declarator.getNestedDeclarator();
		IASTInitializer _initializer = declarator.getInitializer();
		IASTTypeId _trailingReturnType = declarator.getTrailingReturnType();
		IASTTypeId[] _exceptionSpecification = declarator.getExceptionSpecification();
		ICPPASTExpression _noexceptExpression = declarator.getNoexceptExpression();

		// TODO: fix when name == null
		IConstructor name = builder.Name_name("", vf.sourceLocation(loc, loc.getOffset(), 0), isMacroExpansion);
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
			parameters.append(builder.Declaration_varArgs(getTokenSourceLocation(declarator, "..."), isMacroExpansion));

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
						nestedDeclarator, parameters.done(), virtSpecifiers.done(), attributes, loc, decl,
						isMacroExpansion));
			else {
				_initializer.accept(this);
				stack.push(builder.Declarator_functionDeclaratorNested(pointerOperators.done(), modifiers,
						nestedDeclarator, parameters.done(), virtSpecifiers.done(), stack.pop(), attributes, loc, decl,
						isMacroExpansion));
			}
			// if
			// (!(_exceptionSpecification.equals(ICPPASTFunctionDeclarator.NO_EXCEPTION_SPECIFICATION)))
			// err("WARNING: ICPPASTFunctionDeclaration had nestedDeclarator and
			// also exceptionSpecification");
		} else if (_exceptionSpecification.equals(ICPPASTFunctionDeclarator.NO_EXCEPTION_SPECIFICATION)) {
			if (_trailingReturnType == null)
				stack.push(builder.Declarator_functionDeclarator(pointerOperators.done(), modifiers, name,
						parameters.done(), virtSpecifiers.done(), attributes, loc, decl, isMacroExpansion));
			else {
				_trailingReturnType.accept(this);
				stack.push(builder.Declarator_functionDeclarator(pointerOperators.done(), modifiers, name,
						parameters.done(), virtSpecifiers.done(), stack.pop(), attributes, loc, decl,
						isMacroExpansion));
			}
		} else if (_exceptionSpecification.equals(IASTTypeId.EMPTY_TYPEID_ARRAY)) {
			if (_trailingReturnType != null)
				throw new RuntimeException(
						"FunctionDeclarator: Trailing return type and exception specification? at " + loc);
			stack.push(builder.Declarator_functionDeclaratorWithES(pointerOperators.done(), modifiers, name,
					parameters.done(), virtSpecifiers.done(), attributes, loc, decl, isMacroExpansion));
		} else if (_noexceptExpression != null) {
			if (_trailingReturnType != null)
				throw new RuntimeException(
						"FunctionDeclarator: Trailing return type and noexceptExpression? at " + loc);
			if (_initializer != null)
				throw new RuntimeException("FunctionDeclarator: Initializer and noexceptExpression? at " + loc);
			_noexceptExpression.accept(this);
			stack.push(builder.Declarator_functionDeclaratorNoexcept(pointerOperators.done(), modifiers, name,
					parameters.done(), virtSpecifiers.done(), stack.pop(), attributes, loc, decl, isMacroExpansion));
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
					parameters.done(), virtSpecifiers.done(), exceptionSpecification.done(), attributes, loc, decl,
					isMacroExpansion));
		}

		return PROCESS_ABORT;
	}

	// DeclSpecifiers

	@Override
	public int visit(IASTDeclSpecifier declSpec) {
		at(declSpec);

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
		else
			throw new RuntimeException("Unknown sub-class encountered: " + declSpec.getClass().getName() + " at "
					+ locs.forNode(declSpec) + ". Exiting");
		return PROCESS_ABORT;
	}

	private int visit(IASTCompositeTypeSpecifier declSpec) {
		at(declSpec);
		if (declSpec instanceof ICASTCompositeTypeSpecifier) {
			visit((ICASTCompositeTypeSpecifier) declSpec);
		}
		else if (declSpec instanceof ICPPASTCompositeTypeSpecifier) {
			visit((ICPPASTCompositeTypeSpecifier) declSpec);
		}
		else {
			throw new RuntimeException("Unknown IASTCompositeTypeSpecifier subinterface "
					+ declSpec.getClass().getName() + " at " + locs.forNode(declSpec));
		}
		return PROCESS_ABORT;
	}

	private int visit(ICASTCompositeTypeSpecifier declSpec) {
		at(declSpec);

		// TODO: add separate entries in AST, remove fixed empty baseSpecifiers
		ISourceLocation loc = locs.forNode(declSpec);
		boolean isMacroExpansion = isMacroExpansion(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec, loc);
		IList attributes = getAttributes(declSpec);
		IList modifiers = getModifiers(declSpec);

		declSpec.getName().accept(this);
		IConstructor name = stack.pop();

		IListWriter members = vf.listWriter();
		Stream.of(declSpec.getMembers()).forEach(it -> {
			it.accept(this);
			members.append(stack.pop());
		});

		switch (declSpec.getKey()) {
		case ICPPASTCompositeTypeSpecifier.k_struct:
			stack.push(builder.DeclSpecifier_structFinal(modifiers, name, vf.listWriter().done(), members.done(),
					attributes, loc, decl, isMacroExpansion));
			break;
		case ICPPASTCompositeTypeSpecifier.k_union:
			stack.push(builder.DeclSpecifier_unionFinal(modifiers, name, vf.listWriter().done(), members.done(),
					attributes, loc, decl, isMacroExpansion));
			break;
		case ICPPASTCompositeTypeSpecifier.k_class:
			stack.push(builder.DeclSpecifier_classFinal(modifiers, name, vf.listWriter().done(), members.done(),
					attributes, loc, decl, isMacroExpansion));
			break;
		default:
			throw new RuntimeException(
					"Unknown IASTCompositeTypeSpecifier code " + declSpec.getKey() + "at" + loc + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	private int visit(ICPPASTCompositeTypeSpecifier declSpec) {
		at(declSpec);

		ISourceLocation loc = locs.forNode(declSpec);
		boolean isMacroExpansion = isMacroExpansion(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec, loc);
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
						attributes, loc, decl, isMacroExpansion));
			else
				stack.push(builder.DeclSpecifier_structFinal(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl, isMacroExpansion));
			break;
		case ICPPASTCompositeTypeSpecifier.k_union:
			if (virtSpecifier == null)
				stack.push(builder.DeclSpecifier_union(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl, isMacroExpansion));
			else
				stack.push(builder.DeclSpecifier_unionFinal(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl, isMacroExpansion));
			break;
		case ICPPASTCompositeTypeSpecifier.k_class:
			if (virtSpecifier == null)
				stack.push(builder.DeclSpecifier_class(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl, isMacroExpansion));
			else
				stack.push(builder.DeclSpecifier_classFinal(modifiers, name, baseSpecifiers.done(), members.done(),
						attributes, loc, decl, isMacroExpansion));
			break;
		default:
			throw new RuntimeException(
					"Unknown IASTCompositeTypeSpecifier code " + declSpec.getKey() + "at" + loc + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	private int visit(ICASTElaboratedTypeSpecifier declSpec) {
		at(declSpec);

		// TODO: deduplicate with IASTElaboratedTypeSpecifier
		ISourceLocation loc = locs.forNode(declSpec);
		boolean isMacroExpansion = isMacroExpansion(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec, loc);
		IList modifiers = getModifiers(declSpec);

		declSpec.getName().accept(this);
		switch (declSpec.getKind()) {
		case ICASTElaboratedTypeSpecifier.k_enum:
			stack.push(builder.DeclSpecifier_etsEnum(modifiers, stack.pop(), loc, decl, isMacroExpansion));
			break;
		case ICASTElaboratedTypeSpecifier.k_struct:
			stack.push(builder.DeclSpecifier_etsStruct(modifiers, stack.pop(), loc, decl, isMacroExpansion));
			break;
		case ICASTElaboratedTypeSpecifier.k_union:
			stack.push(builder.DeclSpecifier_etsUnion(modifiers, stack.pop(), loc, decl, isMacroExpansion));
			break;
		default:
			throw new RuntimeException(
					"ICASTElaboratedTypeSpecifier encountered unknown kind " + declSpec.getKind() + " at " + loc);
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTElaboratedTypeSpecifier declSpec) {
		at(declSpec);

		ISourceLocation loc = locs.forNode(declSpec);
		if (declSpec instanceof ICASTElaboratedTypeSpecifier) {
			visit((ICASTElaboratedTypeSpecifier) declSpec);
		} else if (declSpec instanceof ICPPASTElaboratedTypeSpecifier) {
			boolean isMacroExpansion = isMacroExpansion(declSpec);
			ISourceLocation decl = br.resolveBinding(declSpec, loc);
			IList modifiers = getModifiers(declSpec);

			declSpec.getName().accept(this);
			switch (declSpec.getKind()) {
			case ICPPASTElaboratedTypeSpecifier.k_enum:
				stack.push(builder.DeclSpecifier_etsEnum(modifiers, stack.pop(), loc, decl, isMacroExpansion));
				break;
			case ICPPASTElaboratedTypeSpecifier.k_struct:
				stack.push(builder.DeclSpecifier_etsStruct(modifiers, stack.pop(), loc, decl, isMacroExpansion));
				break;
			case ICPPASTElaboratedTypeSpecifier.k_union:
				stack.push(builder.DeclSpecifier_etsUnion(modifiers, stack.pop(), loc, decl, isMacroExpansion));
				break;
			case ICPPASTElaboratedTypeSpecifier.k_class:
				stack.push(builder.DeclSpecifier_etsClass(modifiers, stack.pop(), loc, decl, isMacroExpansion));
				break;
			default:
				throw new RuntimeException(
						"IASTElaboratedTypeSpecifier encountered unknown kind " + declSpec.getKind() + " at " + loc);
			}
		} else {
			throw new RuntimeException("Unknown IASTElaboratedTypeSpecifier subclass "
					+ declSpec.getClass().getSimpleName() + " at " + loc);
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTEnumerationSpecifier declSpec) {
		at(declSpec);

		if (declSpec instanceof ICPPASTEnumerationSpecifier) 
			visit((ICPPASTEnumerationSpecifier) declSpec);
		else if (declSpec instanceof ICASTEnumerationSpecifier)
			visit((ICASTEnumerationSpecifier) declSpec);
		else
			throw new RuntimeException("NYI at " + locs.forNode(declSpec));
		return PROCESS_ABORT;
	}

	private int visit(IASTNamedTypeSpecifier declSpec) {
		at(declSpec);

		ISourceLocation loc = locs.forNode(declSpec);
		boolean isMacroExpansion = isMacroExpansion(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec, loc);
		IList modifiers = getModifiers(declSpec);
		declSpec.getName().accept(this);
		stack.push(builder.DeclSpecifier_namedTypeSpecifier(modifiers, stack.pop(), loc, decl, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTSimpleDeclSpecifier declSpec) {
		at(declSpec);

		if (declSpec instanceof ICPPASTSimpleDeclSpecifier) {
			visit((ICPPASTSimpleDeclSpecifier) declSpec);
			return PROCESS_ABORT;
		} else if (declSpec instanceof ICASTSimpleDeclSpecifier) {
			visit((ICASTSimpleDeclSpecifier) declSpec);
			return PROCESS_ABORT;
		}
		throw new RuntimeException("NYI: C SimpleDeclSpecifier");
	}

	private int visit(ICASTDeclSpecifier declSpec) {
		at(declSpec);

		if (declSpec instanceof ICASTCompositeTypeSpecifier)
			visit((ICASTCompositeTypeSpecifier) declSpec);
		if (declSpec instanceof ICASTElaboratedTypeSpecifier)
			visit((ICASTElaboratedTypeSpecifier) declSpec);
		if (declSpec instanceof ICASTEnumerationSpecifier)
			visit((ICASTEnumerationSpecifier) declSpec);
		if (declSpec instanceof ICASTSimpleDeclSpecifier)
			visit((ICASTSimpleDeclSpecifier) declSpec);
//		Not needed? Only adds `restrict` keyword
//		if (declSpec instanceof ICASTTypedefNameSpecifier)
//			visit((ICASTTypedefNameSpecifier) declSpec);
		else {
			out("CDeclSpecifier: " + declSpec.getRawSignature());
			throw new RuntimeException("NYI at " + locs.forNode(declSpec));
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTDeclSpecifier declSpec) {
		at(declSpec);

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
			throw new RuntimeException("NYI at " + locs.forNode(declSpec));
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTElaboratedTypeSpecifier declSpec) {
		at(declSpec);

		out("CPPElaboratedTypeSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(declSpec));
	}

	private int visit(ICASTEnumerationSpecifier declSpec) {
		at(declSpec);

		ISourceLocation loc = locs.forNode(declSpec);
		boolean isMacroExpansion = isMacroExpansion(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec, loc);
		IList attributes = getAttributes(declSpec);
		IList modifiers = getModifiers(declSpec);

		declSpec.getName().accept(this);
		IConstructor name = stack.pop();

		IListWriter enumerators = vf.listWriter();
		Stream.of(declSpec.getEnumerators()).forEach(it -> {
			it.accept(this);
			enumerators.append(stack.pop());
		});

		stack.push(builder.DeclSpecifier_enum(modifiers, name, enumerators.done(), attributes, loc, decl,
				isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTEnumerationSpecifier declSpec) {
		at(declSpec);

		ISourceLocation loc = locs.forNode(declSpec);
		boolean isMacroExpansion = isMacroExpansion(declSpec);
		ISourceLocation decl = br.resolveBinding(declSpec, loc);
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
					stack.push(builder.DeclSpecifier_enumScopedOpaque(modifiers, name, attributes, loc, decl,
							isMacroExpansion));
				else
					stack.push(builder.DeclSpecifier_enumScoped(modifiers, name, enumerators.done(), attributes, loc,
							decl, isMacroExpansion));
			} else
				stack.push(builder.DeclSpecifier_enum(modifiers, name, enumerators.done(), attributes, loc, decl,
						isMacroExpansion));
		} else {
			baseType.accept(this);
			if (declSpec.isScoped()) {
				if (declSpec.isOpaque())
					stack.push(builder.DeclSpecifier_enumScopedOpaque(modifiers, stack.pop(), name, attributes, loc,
							decl, isMacroExpansion));
				else
					stack.push(builder.DeclSpecifier_enumScoped(modifiers, stack.pop(), name, enumerators.done(),
							attributes, loc, decl, isMacroExpansion));
			} else {
				if (declSpec.isOpaque())
					stack.push(builder.DeclSpecifier_enumOpaque(modifiers, stack.pop(), name, attributes, loc, decl,
							isMacroExpansion));
				else
					stack.push(builder.DeclSpecifier_enum(modifiers, stack.pop(), name, enumerators.done(), attributes,
							loc, decl, isMacroExpansion));
			}
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTNamedTypeSpecifier declSpec) {
		at(declSpec);

		out("CPPNamedTypeSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(declSpec));
	}

	private int visit(ICASTSimpleDeclSpecifier declSpec) {
		at(declSpec);

		// TODO: deduplicate with ICPPASTSimpleDeclSpecifier
		ISourceLocation loc = locs.forNode(declSpec);
		boolean isMacroExpansion = isMacroExpansion(declSpec);
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
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_unspecified(location, isMacroExpansion), attributes, loc, isMacroExpansion));
			break;
		}
		case IASTSimpleDeclSpecifier.t_void:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_void(getTokenSourceLocation(declSpec, "void"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_char:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_char(getTokenSourceLocation(declSpec, "char"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_int:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_integer(getTokenSourceLocation(declSpec, "int"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_float:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_float(getTokenSourceLocation(declSpec, "float"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_double:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_double(getTokenSourceLocation(declSpec, "double"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_bool:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_bool(getTokenSourceLocation(declSpec, "bool"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_wchar_t:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_wchar_t(getTokenSourceLocation(declSpec, "wchar_t"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_typeof:
			declSpec.getDeclTypeExpression().accept(this);
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_typeof(stack.pop(), getTokenSourceLocation(declSpec, "typeof"), isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decltype:
			declSpec.getDeclTypeExpression().accept(this);
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decltype(stack.pop(), getTokenSourceLocation(declSpec, "decltype"), isMacroExpansion), attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_auto:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_auto(getTokenSourceLocation(declSpec, "auto"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_char16_t:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_char16_t(getTokenSourceLocation(declSpec, "char16_t"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_char32_t:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_char32_t(getTokenSourceLocation(declSpec, "char32_t"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_int128:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_int128(getTokenSourceLocation(declSpec, "__int128"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_float128:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_float128(getTokenSourceLocation(declSpec, "__float128"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decimal32:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decimal128(getTokenSourceLocation(declSpec, "_Decimal32"), isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decimal64:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decimal64(getTokenSourceLocation(declSpec, "_Decimal64"), isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decimal128:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decimal128(getTokenSourceLocation(declSpec, "_Decimal128"), isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decltype_auto:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers, builder.Type_declTypeAuto(loc, isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		default:
			throw new RuntimeException(
					"Unknown IASTSimpleDeclSpecifier kind " + declSpec.getType() + " at " + loc + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTSimpleDeclSpecifier declSpec) {
		at(declSpec);

		ISourceLocation loc = locs.forNode(declSpec);
		boolean isMacroExpansion = isMacroExpansion(declSpec);
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
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_unspecified(location, isMacroExpansion), attributes, loc, isMacroExpansion));
			break;
		}
		case IASTSimpleDeclSpecifier.t_void:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_void(getTokenSourceLocation(declSpec, "void"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_char:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_char(getTokenSourceLocation(declSpec, "char"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_int:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_integer(getTokenSourceLocation(declSpec, "int"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_float:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_float(getTokenSourceLocation(declSpec, "float"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_double:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_double(getTokenSourceLocation(declSpec, "double"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_bool:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_bool(getTokenSourceLocation(declSpec, "bool"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_wchar_t:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_wchar_t(getTokenSourceLocation(declSpec, "wchar_t"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_typeof:
			declSpec.getDeclTypeExpression().accept(this);
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_typeof(stack.pop(), getTokenSourceLocation(declSpec, "typeof"), isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decltype:
			declSpec.getDeclTypeExpression().accept(this);
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decltype(stack.pop(), getTokenSourceLocation(declSpec, "decltype"), isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_auto:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_auto(getTokenSourceLocation(declSpec, "auto"), isMacroExpansion), attributes, loc,
					isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_char16_t:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_char16_t(getTokenSourceLocation(declSpec, "char16_t"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_char32_t:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_char32_t(getTokenSourceLocation(declSpec, "char32_t"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_int128:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_int128(getTokenSourceLocation(declSpec, "__int128"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_float128:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_float128(getTokenSourceLocation(declSpec, "__float128"), isMacroExpansion), attributes,
					loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decimal32:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decimal128(getTokenSourceLocation(declSpec, "_Decimal32"), isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decimal64:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decimal64(getTokenSourceLocation(declSpec, "_Decimal64"), isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decimal128:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers,
					builder.Type_decimal128(getTokenSourceLocation(declSpec, "_Decimal128"), isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		case IASTSimpleDeclSpecifier.t_decltype_auto:
			stack.push(builder.DeclSpecifier_declSpecifier(modifiers, builder.Type_declTypeAuto(loc, isMacroExpansion),
					attributes, loc, isMacroExpansion));
			break;
		default:
			throw new RuntimeException(
					"Unknown IASTSimpleDeclSpecifier kind " + declSpec.getType() + " at " + loc + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTTypeTransformationSpecifier declSpec) {
		at(declSpec);

		// TODO: implement, check operator and operand
		err("ICPPASTTypeTransformationSpecifier: " + declSpec.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(declSpec));
	}

	@Override
	public int visit(IASTArrayModifier arrayModifier) {
		at (arrayModifier);

		ISourceLocation loc = locs.forNode(arrayModifier);
		boolean isMacroExpansion = isMacroExpansion(arrayModifier);
		IList attributes = getAttributes(arrayModifier);
		IList modifiers = getModifiers(arrayModifier);

		IASTExpression constantExpression = arrayModifier.getConstantExpression();
		if (constantExpression == null)
			stack.push(builder.Expression_arrayModifier(modifiers, attributes, loc, isMacroExpansion));
		else {
			constantExpression.accept(this);
			stack.push(builder.Expression_arrayModifier(modifiers, stack.pop(), attributes, loc, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTPointerOperator ptrOperator) {
		at (ptrOperator);

		if (ptrOperator instanceof IASTPointer)
			visit((IASTPointer) ptrOperator);
		else if (ptrOperator instanceof ICPPASTReferenceOperator)
			visit((ICPPASTReferenceOperator) ptrOperator);
		else
			throw new RuntimeException("Unknown IASTPointerOperator subtype +" + ptrOperator.getClass().getName()
					+ " at " + locs.forNode(ptrOperator) + ". Exiting");
		return PROCESS_ABORT;
	}

	private int visit(IASTPointer pointer) {
		at(pointer);

		ISourceLocation loc = locs.forNode(pointer);
		boolean isMacroExpansion = isMacroExpansion(pointer);
		IList attributes = getAttributes(pointer);
		IList modifiers = getModifiers(pointer);
		if (pointer instanceof ICPPASTPointerToMember) {
			ISourceLocation decl = br.resolveBinding(((ICPPASTPointerToMember) pointer), locs.forNode(pointer));
			((ICPPASTPointerToMember) pointer).getName().accept(this);
			stack.push(builder.Declaration_pointerToMember(modifiers, stack.pop(), attributes, loc, decl,
					isMacroExpansion));
		} else
			stack.push(builder.Declaration_pointer(modifiers, attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTReferenceOperator referenceOperator) {
		at(referenceOperator);

		ISourceLocation loc = locs.forNode(referenceOperator);
		boolean isMacroExpansion = isMacroExpansion(referenceOperator);
		IList attributes = getAttributes(referenceOperator);
		if (referenceOperator.isRValueReference())
			stack.push(builder.Declaration_rvalueReference(attributes, loc, isMacroExpansion));
		else
			stack.push(builder.Declaration_reference(attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTAttribute attribute) {
		at (attribute);
		ISourceLocation src = locs.forNode(attribute);
		if (attribute.getArgumentClause() == null || attribute.getArgumentClause().getTokenCharImage() == null)
			stack.push(builder.Attribute_attribute(new String(attribute.getName()), src));
		else
			stack.push(builder.Attribute_attribute(new String(attribute.getName()),
					new String(attribute.getArgumentClause().getTokenCharImage()), src));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTAttributeSpecifier specifier) {
		at(specifier);

		ISourceLocation src = locs.forNode(specifier);
		if (specifier instanceof ICPPASTAlignmentSpecifier) {
			IASTExpression expression = ((ICPPASTAlignmentSpecifier) specifier).getExpression();
			if (expression != null)
				expression.accept(this);
			else
				((ICPPASTAlignmentSpecifier) specifier).getTypeId().accept(this);
			stack.push(builder.Attribute_alignmentSpecifier(stack.pop(), src));
		} else if (specifier instanceof IASTAttributeList) {
			IASTAttributeList list = (IASTAttributeList) specifier;
			IListWriter attributes = vf.listWriter();
			Stream.of(list.getAttributes()).forEach(it -> {
				it.accept(this);
				attributes.append(stack.pop());
			});
			if (specifier instanceof IMSASTDeclspecList)
				stack.push(builder.Attribute_msDeclspecList(attributes.done(), src));
			else if (specifier instanceof IGCCASTAttributeList)
				stack.push(builder.Attribute_gccAttributeList(attributes.done(), src));
			else
				stack.push(builder.Attribute_attributeSpecifier(attributes.done(), src));
		} else {
			throw new RuntimeException(
					"Unknown AttributeSpecifier type: " + specifier.getClass().getSimpleName() + " at " + src);
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTToken token) {
		at (token);
		err("Token: " + new String(token.getTokenCharImage()));
		throw new RuntimeException("NYI at " + locs.forNode(token));
	}

	@Override
	public int visit(IASTExpression expression) {
		at (expression);

		if (expression instanceof IASTArraySubscriptExpression)
			visit((IASTArraySubscriptExpression) expression);
		else if (expression instanceof IASTBinaryExpression)
			visit((IASTBinaryExpression) expression);
		else if (expression instanceof IASTBinaryTypeIdExpression)
			visit((IASTBinaryTypeIdExpression) expression);
		else if (expression instanceof IASTCastExpression)
			visit((IASTCastExpression) expression);
		else if (expression instanceof IASTConditionalExpression)
			visit((IASTConditionalExpression) expression);
		else if (expression instanceof IASTExpressionList)
			visit((IASTExpressionList) expression);
		else if (expression instanceof IASTFieldReference)
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
		else if (expression instanceof CASTCompoundStatementExpression) {
			visit((CASTCompoundStatementExpression) expression);
		} else {
			throw new RuntimeException("Expression: encountered non-implemented subtype "
					+ expression.getClass().getName() + " at " + locs.forNode(expression));
		}
		return PROCESS_ABORT;
	}

	private int visit(CASTArraySubscriptExpression expression) {
		at(expression);
		// TODO: deduplicate from ICPPASTArraySubscriptExpression
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getArrayExpression().accept(this);
		IConstructor arrayExpression = stack.pop();
		expression.getArgument().accept(this);
		IConstructor argument = stack.pop();

		stack.push(builder.Expression_arraySubscriptExpression(arrayExpression, argument, loc, null, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTArraySubscriptExpression expression) {
		at(expression);

		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getArrayExpression().accept(this);
		IConstructor arrayExpression = stack.pop();
		expression.getArgument().accept(this);
		IConstructor argument = stack.pop();

		stack.push(builder.Expression_arraySubscriptExpression(arrayExpression, argument, loc, null, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTBinaryExpression expression) {
		at(expression);

		out("CPPBinaryExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(expression));
	}

	private int visit(ICPPASTCastExpression expression) {
		at(expression);

		out("CPPCastExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(expression));
	}

	private int visit(ICPPASTDeleteExpression expression) {
		at(expression);

		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);
	
		expression.getOperand().accept(this);
		if (expression.isGlobal()) {
			if (expression.isVectored())
				stack.push(builder.Expression_globalVectoredDelete(stack.pop(), loc, null, typ, isMacroExpansion));
			else
				stack.push(builder.Expression_globalDelete(stack.pop(), loc, null, typ, isMacroExpansion));
		} else {
			if (expression.isVectored())
				stack.push(builder.Expression_vectoredDelete(stack.pop(), loc, null, typ, isMacroExpansion));
			else
				stack.push(builder.Expression_delete(stack.pop(), loc, null, typ, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTExpressionList expression) {
		at(expression);

		// has typ
		out("CPPExpressionList: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(expression));
	}

	private int visit(ICPPASTFieldReference expression) {
		at(expression);
		// has typ
		out("CPPFieldReference: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(expression));
	}

	private int visit(ICPPASTFunctionCallExpression expression) {
		at(expression);
		// has typ
		out("CPPFunctionCallExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(expression));
	}

	private int visit(ICPPASTLambdaExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		ISourceLocation decl = URIUtil.correctLocation("missingDecl", "", "");
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
					vf.sourceLocation(endOfCapture, endOfCapture.getOffset() + 1, 0), decl, isMacroExpansion);
		} else {
			expression.getDeclarator().accept(this);
			declarator = stack.pop();
		}

		expression.getBody().accept(this);
		IConstructor body = stack.pop();

		switch (captureDefault) {
		case BY_COPY:
			stack.push(builder.Expression_lambda(
					builder.Modifier_captDefByCopy(getTokenSourceLocation(expression, "="), isMacroExpansion),
					captures.done(), declarator, body, loc, null, typ, isMacroExpansion));
			break;
		case BY_REFERENCE:
			stack.push(builder.Expression_lambda(
					builder.Modifier_captDefByReference(getTokenSourceLocation(expression, "&"), isMacroExpansion),
					captures.done(), declarator, body, loc, null, typ, isMacroExpansion));
			break;
		case UNSPECIFIED:
			stack.push(builder.Expression_lambda(
					builder.Modifier_captDefUnspecified(vf.sourceLocation(loc, loc.getOffset(), 0), isMacroExpansion),
					captures.done(), declarator, body, loc, null, typ, isMacroExpansion));
			break;
		default:
			throw new RuntimeException("Unknown default capture type " + captureDefault + " encountered at "
					+ locs.forNode(expression) + ", exiting");
		}

		return PROCESS_ABORT;
	}

	private int visit(ICPPASTLiteralExpression expression) {
		at(expression);
		// This may never be reached
		visit((IASTLiteralExpression) expression);
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTNaryTypeIdExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);
		IListWriter args = vf.listWriter();
		Stream.of(expression.getOperands()).forEach(it -> {
			it.accept(this);
			args.append(stack.pop());
		});
		switch (expression.getOperator()) {
		case __is_constructible:
			stack.push(builder.Expression_isConstructable(args.done(), loc, null, typ, isMacroExpansion));
			return PROCESS_ABORT;
		case __is_trivially_constructible:
			stack.push(builder.Expression_isTriviallyConstructable(args.done(), loc, null, typ, isMacroExpansion));
			return PROCESS_ABORT;
		default:
			throw new RuntimeException("Operator " + expression.getOperator() + " unknown at " + loc + ", exiting");
		}
	}

	private ISourceLocation changeScheme(ISourceLocation l, String newScheme) {
		try {
			return URIUtil.changeScheme(l, newScheme);
		} catch (URISyntaxException e) {
			// this never happens
			assert false;
			return l;
		}
	}

	private int visit(ICPPASTNewExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);
		
		expression.getTypeId().accept(this);
		IConstructor typeId = stack.pop();
		IBinding constructor = CPPSemantics.findImplicitlyCalledConstructor(expression);
		ISourceLocation decl = constructor != null ? br.resolveBinding(null, constructor, loc) : null;
		ISourceLocation newDecl = decl != null 
			? changeScheme(decl, "cpp+new") 
			: URIUtil.correctLocation("cpp+new", "", 
				"builtinAllocator" + ((typ != null) ? ("/" + typ.getName()) : ""));

		if (decl != null && newDecl != null) {
			newResolutions.append(vf.tuple(newDecl, decl));
		}

		// we always introduce an implicit `cpp+new` that is used in methodInvocations
		// and because we annotate the tree with `decl` to this, they also end up in the uses
		// relation of M3. Therefore we add them here so we can check completeness and accuracy
		// of the decls, uses and containment relation even though these quasi-constructors have
		// never been declared anywwhere.
		implicitDeclarations.insert(newDecl);
		

		IASTInitializerClause[] _placementArguments = expression.getPlacementArguments();
		IASTInitializer _initializer = expression.getInitializer();
		final IListWriter placementArguments = vf.listWriter();
		IConstructor initializer = null;

		if (_placementArguments != null) {
			Stream.of(_placementArguments).forEach(it -> {
				it.accept(this);
				placementArguments.append(stack.pop());
			});
		}

		if (_initializer != null) {
			_initializer.accept(this);
			initializer = stack.pop();
		}

		if (_placementArguments != null) {
			if (expression.isGlobal()) {
				if (initializer == null) {
					stack.push(builder.Expression_globalNewWithArgs(placementArguments.done(), typeId, loc, newDecl, typ, isMacroExpansion));
				}
				else {
					stack.push(builder.Expression_globalNewWithArgs(placementArguments.done(), typeId, initializer, loc, newDecl, typ, isMacroExpansion));
				}
			}
			else {
				if (_initializer == null) {
					stack.push(builder.Expression_newWithArgs(placementArguments.done(), typeId, loc, newDecl, typ, isMacroExpansion));
				}
				else {
					stack.push(builder.Expression_newWithArgs(placementArguments.done(), typeId, initializer, loc, newDecl, typ, isMacroExpansion));
				}
			}
		}
		else {
			if (expression.isGlobal()) {
				if (_initializer == null) {
					stack.push(builder.Expression_globalNew(typeId, loc, newDecl, typ, isMacroExpansion));
				}
				else {
					stack.push(builder.Expression_globalNew(typeId, initializer, loc, newDecl, typ, isMacroExpansion));
				}
			}
			else {
				if (initializer == null) {
					stack.push(builder.Expression_new(typeId, loc, newDecl, typ, isMacroExpansion));
				}
				else {
					stack.push(builder.Expression_new(typeId, initializer, loc, newDecl, typ, isMacroExpansion));
				}
			}
		}
		
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTPackExpansionExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);
		expression.getPattern().accept(this);
		stack.push(builder.Expression_packExpansion(stack.pop(), loc, null, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTSimpleTypeConstructorExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getDeclSpecifier().accept(this);
		IConstructor declSpecifier = stack.pop();
		expression.getInitializer().accept(this);
		IConstructor initializer = stack.pop();

		stack.push(builder.Expression_simpleTypeConstructor(declSpecifier, initializer, loc, null, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTTypeIdExpression expression) {
		at(expression);
		// has typ
		out("CPPTypeIdExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(expression));
	}

	private int visit(ICPPASTUnaryExpression expression) {
		at(expression);
		out("CPPUnaryExpression: " + expression.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(expression));
	}

	private int visit(CASTCompoundStatementExpression expression) {
		at(expression);
		// TODO: Deduplicate from CPPASTCompoundStatementExpression
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ;
		try {
			typ = tr.resolveType(expression);
		} catch (Throwable t) {
			err("CASTCompoundStatement couldn't get type at " + loc);
			t.printStackTrace(stdErr);
			typ = builder.TypeSymbol_any();
		}
		expression.getCompoundStatement().accept(this);
		stack.push(builder.Expression_compoundStatementExpression(stack.pop(), loc, null, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(CPPASTCompoundStatementExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ;
		try {
			typ = tr.resolveType(expression);
		} catch (Throwable t) {
			err("CPPASTCompoundStatement couldn't get type at " + loc);
			t.printStackTrace(stdErr);
			typ = builder.TypeSymbol_any();
		}
		expression.getCompoundStatement().accept(this);
		stack.push(builder.Expression_compoundStatementExpression(stack.pop(), loc, null, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTArraySubscriptExpression expression) {
		at(expression);
		if (expression instanceof ICPPASTArraySubscriptExpression)
			visit((ICPPASTArraySubscriptExpression) expression);
		else if (expression instanceof CASTArraySubscriptExpression)
			visit((CASTArraySubscriptExpression) expression);
		else
			throw new RuntimeException("NYI at " + locs.forNode(expression));
		return PROCESS_ABORT;
	}

	private int visit(IASTBinaryExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getOperand1().accept(this);
		IConstructor lhs = stack.pop();
		expression.getInitOperand2().accept(this);
		IConstructor rhs = stack.pop();

		ISourceLocation decl = null;

		if (expression instanceof ICPPASTBinaryExpression) {
			// C++ operators can be overloaded, we want to know which ones we are using
			ICPPASTBinaryExpression cppe = (ICPPASTBinaryExpression) expression;
			ICPPFunction overload = cppe.getOverload();

			if (overload != null) {
				decl = br.resolveBinding(overload, loc);
			}
		}

		switch (expression.getOperator()) {
		case IASTBinaryExpression.op_multiply:
			stack.push(builder.Expression_multiply(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_divide:
			stack.push(builder.Expression_divide(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_modulo:
			stack.push(builder.Expression_modulo(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_plus:
			stack.push(builder.Expression_plus(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_minus:
			stack.push(builder.Expression_minus(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_shiftLeft:
			stack.push(builder.Expression_shiftLeft(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_shiftRight:
			stack.push(builder.Expression_shiftRight(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_lessThan:
			stack.push(builder.Expression_lessThan(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_greaterThan:
			stack.push(builder.Expression_greaterThan(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_lessEqual:
			stack.push(builder.Expression_lessEqual(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_greaterEqual:
			stack.push(builder.Expression_greaterEqual(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_binaryAnd:
			stack.push(builder.Expression_binaryAnd(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_binaryXor:
			stack.push(builder.Expression_binaryXor(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_binaryOr:
			stack.push(builder.Expression_binaryOr(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_logicalAnd:
			stack.push(builder.Expression_logicalAnd(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_logicalOr:
			stack.push(builder.Expression_logicalOr(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_assign:
			stack.push(builder.Expression_assign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_multiplyAssign:
			stack.push(builder.Expression_multiplyAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_divideAssign:
			stack.push(builder.Expression_divideAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_moduloAssign:
			stack.push(builder.Expression_moduloAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_plusAssign:
			stack.push(builder.Expression_plusAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_minusAssign:
			stack.push(builder.Expression_minusAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_shiftLeftAssign:
			stack.push(builder.Expression_shiftLeftAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_shiftRightAssign:
			stack.push(builder.Expression_shiftRightAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_binaryAndAssign:
			stack.push(builder.Expression_binaryAndAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_binaryXorAssign:
			stack.push(builder.Expression_binaryXorAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_binaryOrAssign:
			stack.push(builder.Expression_binaryOrAssign(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_equals:
			stack.push(builder.Expression_equals(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_notequals:
			stack.push(builder.Expression_notEquals(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_pmdot:
			stack.push(builder.Expression_pmDot(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_pmarrow:
			stack.push(builder.Expression_pmArrow(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_max:
			stack.push(builder.Expression_max(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_min:
			stack.push(builder.Expression_min(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		case IASTBinaryExpression.op_ellipses:
			stack.push(builder.Expression_ellipses(lhs, rhs, loc, decl, typ, isMacroExpansion));
			break;
		default:
			throw new RuntimeException("Operator " + expression.getOperator() + " unknown at " + loc + ", exiting");
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTBinaryTypeIdExpression expression) {
		at(expression);
		// has typ
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getOperand1().accept(this);
		IConstructor lhs = stack.pop();
		expression.getOperand2().accept(this);
		IConstructor rhs = stack.pop();

		switch (expression.getOperator()) {
		case __is_base_of:
			stack.push(builder.Expression_isBaseOf(lhs, rhs, loc, null, typ, isMacroExpansion));
			break;
		case __is_trivially_assignable:
			stack.push(builder.Expression_isTriviallyAssignable(lhs, rhs, loc, null, typ, isMacroExpansion));
			break;
		default:
			throw new RuntimeException(
					"Unknown binary TypeId expression " + expression.getOperator().name() + " at " + loc);
		}

		return PROCESS_ABORT;
	}

	private int visit(IASTCastExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getOperand().accept(this);
		IConstructor operand = stack.pop();
		expression.getTypeId().accept(this);
		IConstructor type = stack.pop();

		switch (expression.getOperator()) {
		case ICPPASTCastExpression.op_cast:
			stack.push(builder.Expression_cast(type, operand, loc, null, typ, isMacroExpansion));
			break;
		case ICPPASTCastExpression.op_dynamic_cast:
			stack.push(builder.Expression_dynamicCast(type, operand, loc, null, typ, isMacroExpansion));
			break;
		case ICPPASTCastExpression.op_static_cast:
			stack.push(builder.Expression_staticCast(type, operand, loc, null, typ, isMacroExpansion));
			break;
		case ICPPASTCastExpression.op_reinterpret_cast:
			stack.push(builder.Expression_reinterpretCast(type, operand, loc, null, typ, isMacroExpansion));
			break;
		case ICPPASTCastExpression.op_const_cast:
			stack.push(builder.Expression_constCast(type, operand, loc, null, typ, isMacroExpansion));
			break;
		default:
			throw new RuntimeException("Unknown cast type " + expression.getOperator() + " at " + loc);
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTConditionalExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getLogicalConditionExpression().accept(this);
		IConstructor condition = stack.pop();
		expression.getNegativeResultExpression().accept(this);
		IConstructor negative = stack.pop();

		IASTExpression positiveResultExpression = expression.getPositiveResultExpression();
		if (positiveResultExpression != null) {
			positiveResultExpression.accept(this);
			IConstructor positive = stack.pop();
			stack.push(builder.Expression_conditional(condition, positive, negative, loc, null, typ, isMacroExpansion));

		} else {// GNU extension: `?:`
			stack.push(builder.Expression_conditional(condition, negative, loc, null, typ, isMacroExpansion));
		}

		return PROCESS_ABORT;
	}

	private int visit(IASTExpressionList expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);
		IListWriter expressions = vf.listWriter();
		Stream.of(expression.getExpressions()).forEach(it -> {
			it.accept(this);
			expressions.append(stack.pop());
		});
		stack.push(builder.Expression_expressionList(expressions.done(), loc, null, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTFieldReference expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		ISourceLocation decl = br.resolveBinding(expression, loc);
		IConstructor typ = tr.resolveType(expression);

		if (expression instanceof ICPPASTFieldReference) {
			// TODO: implement isTemplate
			expression.getFieldOwner().accept(this);
			IConstructor fieldOwner = stack.pop();
			expression.getFieldName().accept(this);
			IConstructor fieldName = stack.pop();

			if (expression.isPointerDereference())
				stack.push(builder.Expression_fieldReferencePointerDeref(fieldOwner, fieldName, loc, decl, typ,
						isMacroExpansion));
			else
				stack.push(builder.Expression_fieldReference(fieldOwner, fieldName, loc, decl, typ, isMacroExpansion));
		} else if (expression instanceof CASTFieldReference) {
			expression.getFieldOwner().accept(this);
			IConstructor fieldOwner = stack.pop();
			expression.getFieldName().accept(this);
			IConstructor fieldName = stack.pop();

			if (expression.isPointerDereference())
				stack.push(builder.Expression_fieldReferencePointerDeref(fieldOwner, fieldName, loc, decl, typ,
						isMacroExpansion));
			else
				stack.push(builder.Expression_fieldReference(fieldOwner, fieldName, loc, decl, typ, isMacroExpansion));
		} else
			throw new RuntimeException("IASTFieldReference: NYI at " + loc);
		return PROCESS_ABORT;
	}

	private int visit(IASTFunctionCallExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);
		ISourceLocation decl = null;

		// call syntax can be overloaded, register where we are resolving to here
		if (expression instanceof ICPPASTFunctionCallExpression) {
			ICPPASTFunctionCallExpression cppe = (ICPPASTFunctionCallExpression) expression;
			ICPPFunction overload = cppe.getOverload();

			if (overload != null) {
				decl = br.resolveBinding(overload, loc);
			}
		}

		expression.getFunctionNameExpression().accept(this);
		IConstructor functionName = stack.pop();

		IListWriter arguments = vf.listWriter();
		Stream.of(expression.getArguments()).forEach(it -> {
			it.accept(this);
			arguments.append(stack.pop());
		});
		stack.push(builder.Expression_functionCall(functionName, arguments.done(), loc, decl, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTIdExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		ISourceLocation decl = br.resolveBinding(expression, loc);
		IConstructor typ = tr.resolveType(expression);
		expression.getName().accept(this);
		stack.push(builder.Expression_idExpression(stack.pop(), loc, decl, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTLiteralExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);
		ISourceLocation decl = null; // TODO: could literal syntax be overloaded?

		String value = new String(expression.getValue());
		switch (expression.getKind()) {
		case IASTLiteralExpression.lk_integer_constant:
			stack.push(builder.Expression_integerConstant(value, loc, decl, typ, isMacroExpansion));
			break;
		case IASTLiteralExpression.lk_float_constant:
			stack.push(builder.Expression_floatConstant(value, loc, decl, typ, isMacroExpansion));
			break;
		case IASTLiteralExpression.lk_char_constant:
			stack.push(builder.Expression_charConstant(value, loc, decl, typ, isMacroExpansion));
			break;
		case IASTLiteralExpression.lk_string_literal:
			stack.push(builder.Expression_stringLiteral(value, loc, decl, typ, isMacroExpansion));
			break;
		case IASTLiteralExpression.lk_this:
			stack.push(builder.Expression_this(loc, decl, typ, isMacroExpansion));
			break;
		case IASTLiteralExpression.lk_true:
			stack.push(builder.Expression_true(loc, decl, typ, isMacroExpansion));
			break;
		case IASTLiteralExpression.lk_false:
			stack.push(builder.Expression_false(loc, decl, typ, isMacroExpansion));
			break;
		case IASTLiteralExpression.lk_nullptr:
			stack.push(builder.Expression_nullptr(loc, decl, typ, isMacroExpansion));
			break;
		default:
			throw new RuntimeException(
					"Encountered unknown literal kind " + expression.getKind() + " at " + loc + ". Exiting");
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTProblemExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IASTProblem problem = expression.getProblem();
		if (doProblemLogging)
			err("ProblemExpression " + expression.getRawSignature() + ":" + problem.getMessageWithLocation());
		stack.push(builder.Expression_problemExpression(loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTTypeIdInitializerExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);
		expression.getTypeId().accept(this);
		IConstructor typeId = stack.pop();
		expression.getInitializer().accept(this);
		stack.push(builder.Expression_typeIdInitializerExpression(typeId, stack.pop(), loc, null, typ, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTTypeIdExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);

		expression.getTypeId().accept(this);
		switch (expression.getOperator()) {
		case IASTTypeIdExpression.op_sizeof:
			stack.push(builder.Expression_sizeof(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_typeid:
			stack.push(builder.Expression_typeid(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_alignof: // gnu-only?
			stack.push(builder.Expression_alignOf(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_typeof:
			stack.push(builder.Expression_typeof(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_has_nothrow_assign:
			stack.push(builder.Expression_hasNothrowAssign(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_has_nothrow_copy:
			stack.push(builder.Expression_hasNothrowCopy(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_has_nothrow_constructor:
			stack.push(builder.Expression_hasNothrowConstructor(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_has_trivial_assign:
			stack.push(builder.Expression_hasTrivialAssign(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_has_trivial_constructor:
			stack.push(builder.Expression_hasTrivialConstructor(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_has_trivial_copy:
			stack.push(builder.Expression_hasTrivialCopy(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_has_trivial_destructor:
			stack.push(builder.Expression_hasTrivialDestructor(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_has_virtual_destructor:
			stack.push(builder.Expression_hasVirtualDestructor(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_abstract:
			stack.push(builder.Expression_isAbstract(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_class:
			stack.push(builder.Expression_isClass(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_empty:
			stack.push(builder.Expression_isEmpty(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_enum:
			stack.push(builder.Expression_isEnum(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_pod:
			stack.push(builder.Expression_isPod(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_polymorphic:
			stack.push(builder.Expression_isPolymorphic(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_union:
			stack.push(builder.Expression_isUnion(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_literal_type:
			stack.push(builder.Expression_isLiteralType(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_standard_layout:
			stack.push(builder.Expression_isStandardLayout(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_trivial:
			stack.push(builder.Expression_isTrivial(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_sizeofParameterPack:
			stack.push(builder.Expression_sizeofParameterPack(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_final:
			stack.push(builder.Expression_isFinal(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		case IASTTypeIdExpression.op_is_trivially_copyable:
			stack.push(builder.Expression_isTriviallyCopyable(stack.pop(), loc, null, typ, isMacroExpansion));
			break;
		default:
			throw new RuntimeException("ERROR: IASTTypeIdExpression called with unimplemented/unknown operator "
					+ expression.getOperator() + " at " + loc);
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTUnaryExpression expression) {
		at(expression);
		ISourceLocation loc = locs.forNode(expression);
		boolean isMacroExpansion = isMacroExpansion(expression);
		IConstructor typ = tr.resolveType(expression);
		ISourceLocation decl = null;

		IConstructor operand = null;
		if (expression.getOperand() != null) {
			expression.getOperand().accept(this);
			operand = stack.pop();
		}

		if (expression instanceof ICPPASTUnaryExpression) {
			ICPPASTUnaryExpression cppe = (ICPPASTUnaryExpression) expression;
			ICPPFunction overload = cppe.getOverload();

			if (overload != null) {
				decl = br.resolveBinding(overload, loc);
			}
		}
		
		switch (expression.getOperator()) {
		case IASTUnaryExpression.op_prefixIncr:
			stack.push(builder.Expression_prefixIncr(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_prefixDecr:
			stack.push(builder.Expression_prefixDecr(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_plus:
			stack.push(builder.Expression_plus(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_minus:
			stack.push(builder.Expression_minus(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_star:
			stack.push(builder.Expression_star(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_amper:
			stack.push(builder.Expression_amper(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_tilde:
			stack.push(builder.Expression_tilde(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_not:
			stack.push(builder.Expression_not(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_sizeof:
			stack.push(builder.Expression_sizeof(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_postFixIncr:
			stack.push(builder.Expression_postfixIncr(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_postFixDecr:
			stack.push(builder.Expression_postfixDecr(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_bracketedPrimary:
			stack.push(builder.Expression_bracketed(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_throw:
			if (operand == null)
				stack.push(builder.Expression_throw(loc, decl, typ, isMacroExpansion));
			else
				stack.push(builder.Expression_throw(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_typeid:
			stack.push(builder.Expression_typeid(operand, loc, decl, typ, isMacroExpansion));
			break;
		// case IASTUnaryExpression.op_typeof: (14) typeOf is deprecated
		case IASTUnaryExpression.op_alignOf:
			stack.push(builder.Expression_alignOf(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_sizeofParameterPack:
			stack.push(builder.Expression_sizeofParameterPack(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_noexcept:
			stack.push(builder.Expression_noexcept(operand, loc, decl, typ, isMacroExpansion));
			break;
		case IASTUnaryExpression.op_labelReference:
			stack.push(builder.Expression_labelReference(operand, loc, decl, typ, isMacroExpansion));
			break;
		default:
			throw new RuntimeException(
					"Unknown unary operator " + expression.getOperator() + " at " + loc + ". Exiting");
		}

		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTStatement statement) {
		at(statement);

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
					+ statement.getClass().getName() + " at " + locs.forNode(statement));
		}
		return PROCESS_ABORT;
	}

	private int visit(IGNUASTGotoStatement statement) {
		at(statement);
		visit(statement.getLabelNameExpression());
		IList attributes = getAttributes(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		
		stack.push(builder.Statement_computedGoto(stack.pop(), attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTCatchHandler statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);

		statement.getCatchBody().accept(this);
		IConstructor catchBody = stack.pop();

		if (statement.isCatchAll())
			stack.push(builder.Statement_catchAll(catchBody, attributes, loc, isMacroExpansion));
		else {
			statement.getDeclaration().accept(this);
			stack.push(builder.Statement_catch(stack.pop(), catchBody, attributes, loc, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTRangeBasedForStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);

		statement.getDeclaration().accept(this);
		IConstructor declaration = stack.pop();
		statement.getInitializerClause().accept(this);
		IConstructor initializerClause = stack.pop();
		statement.getBody().accept(this);
		IConstructor body = stack.pop();

		stack.push(builder.Statement_rangeBasedFor(declaration, initializerClause, body, attributes, loc,
				isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(ICPPASTTryBlockStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);

		statement.getTryBody().accept(this);
		IConstructor tryBody = stack.pop();

		IListWriter catchHandlers = vf.listWriter();
		Stream.of(statement.getCatchHandlers()).forEach(it -> {
			it.accept(this);
			catchHandlers.append(stack.pop());
		});

		stack.push(builder.Statement_tryBlock(tryBody, catchHandlers.done(), attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTAmbiguousStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
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

	private int visit(IASTBreakStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);
		stack.push(builder.Statement_break(attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTCaseStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);
		statement.getExpression().accept(this);
		IConstructor expression = stack.pop();
		stack.push(builder.Statement_case(expression, attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTCompoundStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);
		IListWriter statements = vf.listWriter();
		Stream.of(statement.getStatements()).forEach(it -> {
			it.accept(this);
			statements.append(stack.pop());
		});
		stack.push(builder.Statement_compoundStatement(statements.done(), attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTContinueStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);
		stack.push(builder.Statement_continue(attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTDeclarationStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);
		statement.getDeclaration().accept(this);
		stack.push(builder.Statement_declarationStatement(stack.pop(), attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTDefaultStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);
		stack.push(builder.Statement_defaultCase(attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTDoStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);

		statement.getBody().accept(this);
		IConstructor body = stack.pop();
		statement.getCondition().accept(this);
		IConstructor condition = stack.pop();
		stack.push(builder.Statement_do(body, condition, attributes, loc, isMacroExpansion));

		return PROCESS_ABORT;
	}

	private int visit(IASTExpressionStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);
		statement.getExpression().accept(this);
		stack.push(builder.Statement_expressionStatement(stack.pop(), attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTForStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
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
					vf.sourceLocation(initializerLoc, initializerLoc.getOffset() + initializerLoc.getLength(), 0),
					isMacroExpansion);
		} else {
			_condition.accept(this);
			condition = stack.pop();
		}

		IASTExpression _iteration = statement.getIterationExpression();
		IConstructor iteration;
		if (_iteration == null) {
			ISourceLocation conditionLoc = (ISourceLocation) condition.asWithKeywordParameters().getParameter("src");
			iteration = builder.Expression_empty(
					vf.sourceLocation(conditionLoc, conditionLoc.getOffset() + conditionLoc.getLength(), 0),
					isMacroExpansion);
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
				stack.push(builder.Statement_forWithDecl(initializer, stack.pop(), iteration, body, attributes, loc,
						isMacroExpansion));
				return PROCESS_ABORT;
			}
		}
		stack.push(builder.Statement_for(initializer, condition, iteration, body, attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTGotoStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		ISourceLocation decl = br.resolveBinding(statement, loc);
		IList attributes = getAttributes(statement);
		statement.getName().accept(this);
		stack.push(builder.Statement_goto(stack.pop(), attributes, loc, decl, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTIfStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
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
				stack.push(builder.Statement_ifWithDecl(stack.pop(), thenClause, attributes, loc, isMacroExpansion));
			} else {
				stack.push(builder.Statement_ifWithDecl(stack.pop(), thenClause, elseClause, attributes, loc,
						isMacroExpansion));
			}
		} else {
			statement.getConditionExpression().accept(this);
			if (elseClause == null) {
				stack.push(builder.Statement_if(stack.pop(), thenClause, attributes, loc, isMacroExpansion));
			} else {
				stack.push(
						builder.Statement_if(stack.pop(), thenClause, elseClause, attributes, loc, isMacroExpansion));
			}
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTLabelStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		ISourceLocation decl = br.resolveBinding(statement, loc);
		IList attributes = getAttributes(statement);

		statement.getName().accept(this);
		IConstructor name = stack.pop();
		statement.getNestedStatement().accept(this);
		IConstructor nestedStatement = stack.pop();

		stack.push(builder.Statement_label(name, nestedStatement, attributes, loc, decl, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTNullStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);
		stack.push(builder.Statement_nullStatement(attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTProblemStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		if (doProblemLogging) {
			err("IASTProblemStatement:");
			prefix += 4;
			err(statement.getProblem().getMessageWithLocation());
			err("" + statement.getProblem().getID());
			err(statement.getRawSignature());
			prefix -= 4;
		}
		stack.push(builder.Statement_problem(statement.getRawSignature(), loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTReturnStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);
		IASTExpression returnValue = statement.getReturnValue();
		IASTInitializerClause returnArgument = statement.getReturnArgument();
		if (returnValue == null && returnArgument == null)
			stack.push(builder.Statement_return(attributes, loc, isMacroExpansion));
		else if (returnValue != null) {
			returnValue.accept(this);
			stack.push(builder.Statement_return(stack.pop(), attributes, loc, isMacroExpansion));
		} else {
			returnArgument.accept(this);
			// Note: InitializerClause is currently mapped on Expression
			stack.push(builder.Statement_return(stack.pop(), attributes, loc, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	private int visit(IASTSwitchStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);

		statement.getBody().accept(this);
		IConstructor body = stack.pop();

		IASTExpression _controller = statement.getControllerExpression();
		if (_controller == null && statement instanceof ICPPASTSwitchStatement) {
			((ICPPASTSwitchStatement) statement).getControllerDeclaration().accept(this);
			stack.push(builder.Statement_switchWithDecl(stack.pop(), body, attributes, loc, isMacroExpansion));
			return PROCESS_ABORT;
		}

		_controller.accept(this);
		stack.push(builder.Statement_switch(stack.pop(), body, attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	private int visit(IASTWhileStatement statement) {
		at(statement);
		ISourceLocation loc = locs.forNode(statement);
		boolean isMacroExpansion = isMacroExpansion(statement);
		IList attributes = getAttributes(statement);

		statement.getBody().accept(this);
		IConstructor body = stack.pop();

		IASTExpression _condition = statement.getCondition();
		if (_condition == null && statement instanceof ICPPASTWhileStatement) {
			((ICPPASTWhileStatement) statement).getConditionDeclaration().accept(this);
			stack.push(builder.Statement_whileWithDecl(stack.pop(), body, attributes, loc, isMacroExpansion));
			return PROCESS_ABORT;
		}
		_condition.accept(this);
		stack.push(builder.Statement_while(stack.pop(), body, attributes, loc, isMacroExpansion));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTTypeId typeId) {
		at(typeId);
		ISourceLocation loc = locs.forNode(typeId);
		boolean isMacroExpansion = isMacroExpansion(typeId);
		if (typeId instanceof IASTProblemTypeId) {
			if (typeId.getRawSignature().equals("...") || typeId.getRawSignature().contains("_THROW1("))
				stack.push(builder.Expression_typeId(
						builder.DeclSpecifier_msThrowEllipsis(loc, BindingsResolver.failedBinding("unknown"), false), loc,
						isMacroExpansion));
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
				ISourceLocation declaratorLoc = locs.forNode(typeId.getAbstractDeclarator());
				abstractDeclarator = abstractDeclarator.set("name",
						builder.Name_abstractEmptyName(declaratorLoc, false));
				abstractDeclarator = abstractDeclarator.asWithKeywordParameters().unsetParameter("decl");
			}
			stack.push(builder.Expression_typeId(declSpecifier, abstractDeclarator, loc, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTEnumerator enumerator) {
		at(enumerator);
		ISourceLocation loc = locs.forNode(enumerator);
		boolean isMacroExpansion = isMacroExpansion(enumerator);
		ISourceLocation decl = br.resolveBinding(enumerator, loc);

		enumerator.getName().accept(this);
		IConstructor name = stack.pop();

		IASTExpression value = enumerator.getValue();
		if (value == null)
			stack.push(builder.Declaration_enumerator(name, loc, decl, isMacroExpansion));
		else {
			value.accept(this);
			stack.push(builder.Declaration_enumerator(name, stack.pop(), loc, decl, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(IASTProblem problem) {
		at(problem);
		err("Problem: " + problem.getMessage());
		throw new RuntimeException("NYI at " + locs.forNode(problem));
	}

	@Override
	public int visit(ICPPASTBaseSpecifier baseSpecifier) {
		at(baseSpecifier);
		ISourceLocation loc = locs.forNode(baseSpecifier);
		boolean isMacroExpansion = isMacroExpansion(baseSpecifier);
		ISourceLocation decl = br.resolveBinding(baseSpecifier, loc);

		IListWriter modifiers = vf.listWriter();
		switch (baseSpecifier.getVisibility()) {
		case ICPPASTBaseSpecifier.v_public:
			modifiers
					.append(builder.Modifier_public(getTokenSourceLocation(baseSpecifier, "public"), isMacroExpansion));
			break;
		case ICPPASTBaseSpecifier.v_protected:
			modifiers.append(
					builder.Modifier_protected(getTokenSourceLocation(baseSpecifier, "protected"), isMacroExpansion));
			break;
		case ICPPASTBaseSpecifier.v_private:
			modifiers.append(
					builder.Modifier_private(getTokenSourceLocation(baseSpecifier, "private"), isMacroExpansion));
			break;
		case 0:
			modifiers.append(builder.Modifier_unspecifiedInheritance(vf.sourceLocation(loc, loc.getOffset(), 0),
					isMacroExpansion));
			break;
		default:
			throw new RuntimeException(
					"Unknown BaseSpecifier visibility code " + baseSpecifier.getVisibility() + " at " + loc);
		}
		if (baseSpecifier.isVirtual())
			modifiers.append(
					builder.Modifier_virtual(getTokenSourceLocation(baseSpecifier, "virtual"), isMacroExpansion));

		ICPPASTNameSpecifier nameSpecifier = baseSpecifier.getNameSpecifier();
		if (nameSpecifier == null)
			stack.push(builder.Declaration_baseSpecifier(modifiers.done(), loc, decl, isMacroExpansion));
		else {
			nameSpecifier.accept(this);
			stack.push(builder.Declaration_baseSpecifier(modifiers.done(), stack.pop(), loc, decl, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTNamespaceDefinition namespaceDefinition) {
		at(namespaceDefinition);
		ISourceLocation loc = locs.forNode(namespaceDefinition);
		boolean isMacroExpansion = isMacroExpansion(namespaceDefinition);
		ISourceLocation decl = br.resolveBinding(namespaceDefinition, loc);
		IList attributes = getAttributes(namespaceDefinition);

		namespaceDefinition.getName().accept(this);
		IConstructor name = stack.pop();

		IListWriter declarations = vf.listWriter();
		Stream.of(namespaceDefinition.getDeclarations()).forEach(it -> {
			it.accept(this);
			declarations.append(stack.pop());
		});

		if (namespaceDefinition.isInline())
			stack.push(builder.Declaration_namespaceDefinitionInline(name, declarations.done(), attributes, loc, decl,
					isMacroExpansion));
		else
			stack.push(builder.Declaration_namespaceDefinition(name, declarations.done(), attributes, loc, decl,
					isMacroExpansion));
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTTemplateParameter templateParameter) {
		at(templateParameter);
		ISourceLocation loc = locs.forNode(templateParameter);
		boolean isMacroExpansion = isMacroExpansion(templateParameter);
		// boolean isParameterPack = templateParameter.isParameterPack();
		// if (isParameterPack)
		// err("WARNING: ICPPASTTemplateParameter has isParameterPack=true,
		// unimplemented");
		if (templateParameter instanceof ICPPASTSimpleTypeTemplateParameter) {
			ISourceLocation decl = br.resolveBinding((ICPPASTSimpleTypeTemplateParameter) templateParameter, loc);

			ICPPASTSimpleTypeTemplateParameter parameter = (ICPPASTSimpleTypeTemplateParameter) templateParameter;
			parameter.getName().accept(this);
			IConstructor name = stack.pop();

			if (parameter.getDefaultType() != null) {
				parameter.getDefaultType().accept(this);
				switch (parameter.getParameterType()) {
				case ICPPASTSimpleTypeTemplateParameter.st_class:
					stack.push(builder.Declaration_sttClass(name, stack.pop(), loc, decl, isMacroExpansion));
					break;
				case ICPPASTSimpleTypeTemplateParameter.st_typename:
					stack.push(builder.Declaration_sttTypename(name, stack.pop(), loc, decl, isMacroExpansion));
					break;
				default:
					throw new RuntimeException("ICPPASTTemplateParameter encountered non-implemented parameter type "
							+ parameter.getParameterType() + " at " + loc);
				}
			} else {
				switch (parameter.getParameterType()) {
				case ICPPASTSimpleTypeTemplateParameter.st_class:
					stack.push(builder.Declaration_sttClass(name, loc, decl, isMacroExpansion));
					break;
				case ICPPASTSimpleTypeTemplateParameter.st_typename:
					stack.push(builder.Declaration_sttTypename(name, loc, decl, isMacroExpansion));
					break;
				default:
					throw new RuntimeException("ICPPASTTemplateParameter encountered non-implemented parameter type "
							+ parameter.getParameterType() + " at " + loc);
				}
			}
		} else if (templateParameter instanceof ICPPASTTemplatedTypeTemplateParameter) {
			ISourceLocation decl = br.resolveBinding((ICPPASTTemplatedTypeTemplateParameter) templateParameter, locs.forNode(templateParameter));
			IListWriter templateParameters = vf.listWriter();
			Stream.of(((ICPPASTTemplatedTypeTemplateParameter) templateParameter).getTemplateParameters())
					.forEach(it -> {
						it.accept(this);
						templateParameters.append(stack.pop());
					});
			((ICPPASTTemplatedTypeTemplateParameter) templateParameter).getName().accept(this);
			IConstructor name = stack.pop();
			IASTExpression defaultValue = ((ICPPASTTemplatedTypeTemplateParameter) templateParameter).getDefaultValue();
			if (defaultValue == null) {
				stack.push(
						builder.Declaration_tttParameter(templateParameters.done(), name, loc, decl, isMacroExpansion));
			} else {
				defaultValue.accept(this);
				stack.push(builder.Declaration_tttParameterWithDefault(templateParameters.done(), name, stack.pop(),
						loc, decl, isMacroExpansion));
			}
		} else
			throw new RuntimeException("ICPPASTTemplateParameter encountered unknown subtype "
					+ templateParameter.getClass().getName() + " at " + loc + ". Exiting");
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTCapture capture) {
		at(capture);
		// TODO: check isPackExpansion
		ISourceLocation loc = locs.forNode(capture);
		boolean isMacroExpansion = isMacroExpansion(capture);
		if (capture.capturesThisPointer())
			stack.push(builder.Expression_captureThisPtr(loc, isMacroExpansion));
		else {
			ISourceLocation decl = br.resolveBinding(capture, locs.forNode(capture));
			capture.getIdentifier().accept(this);
			if (capture.isByReference())
				stack.push(builder.Expression_captureByRef(stack.pop(), loc, decl, isMacroExpansion));
			else
				stack.push(builder.Expression_capture(stack.pop(), loc, decl, isMacroExpansion));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICASTDesignator designator) {
		at(designator);
		// TODO: deduplicate from ICPPASTDesignator
		ISourceLocation loc = locs.forNode(designator);
		boolean isMacroExpansion = isMacroExpansion(designator);
		if (designator instanceof ICASTArrayDesignator) {
			((ICASTArrayDesignator) designator).getSubscriptExpression().accept(this);
			stack.push(builder.Expression_arrayDesignator(stack.pop(), loc, isMacroExpansion));
		} else if (designator instanceof ICASTFieldDesignator) {
			((ICASTFieldDesignator) designator).getName().accept(this);
			stack.push(builder.Expression_fieldDesignator(stack.pop(), loc, isMacroExpansion));
		} else if (designator instanceof IGCCASTArrayRangeDesignator) {
			((IGCCASTArrayRangeDesignator) designator).getRangeFloor().accept(this);
			IConstructor rangeFloor = stack.pop();
			((IGCCASTArrayRangeDesignator) designator).getRangeCeiling().accept(this);
			IConstructor rangeCeiling = stack.pop();
			stack.push(builder.Expression_arrayRangeDesignator(rangeFloor, rangeCeiling, loc, isMacroExpansion));
		} else {
			err("Designator: " + designator.getRawSignature());
			throw new RuntimeException("Unknown Designator at " + locs.forNode(designator));
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTDesignator designator) {
		at(designator);
		ISourceLocation loc = locs.forNode(designator);
		boolean isMacroExpansion = isMacroExpansion(designator);
		if (designator instanceof ICPPASTArrayDesignator) {
			((ICPPASTArrayDesignator) designator).getSubscriptExpression().accept(this);
			stack.push(builder.Expression_arrayDesignator(stack.pop(), loc, isMacroExpansion));
		} else if (designator instanceof ICPPASTFieldDesignator) {
			((ICPPASTFieldDesignator) designator).getName().accept(this);
			stack.push(builder.Expression_fieldDesignator(stack.pop(), loc, isMacroExpansion));
		} else if (designator instanceof IGPPASTArrayRangeDesignator) {
			((IGPPASTArrayRangeDesignator) designator).getRangeFloor().accept(this);
			IConstructor rangeFloor = stack.pop();
			((IGPPASTArrayRangeDesignator) designator).getRangeCeiling().accept(this);
			IConstructor rangeCeiling = stack.pop();
			stack.push(builder.Expression_arrayRangeDesignator(rangeFloor, rangeCeiling, loc, isMacroExpansion));
		} else
			throw new RuntimeException("ICPPASTDesignator encountered unknown subclass at " + loc + ", exiting");
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTVirtSpecifier virtSpecifier) {
		at(virtSpecifier);

		ISourceLocation loc = locs.forNode(virtSpecifier);
		boolean isMacroExpansion = isMacroExpansion(virtSpecifier);
		switch (virtSpecifier.getKind()) {
		case Final:
			stack.push(builder.Declaration_virtSpecifier(
					builder.Modifier_final(getTokenSourceLocation(virtSpecifier, "final"), isMacroExpansion), loc,
					isMacroExpansion));
			break;
		case Override:
			stack.push(builder.Declaration_virtSpecifier(
					builder.Modifier_override(getTokenSourceLocation(virtSpecifier, "override"), isMacroExpansion), loc,
					isMacroExpansion));
			break;
		default:
			throw new RuntimeException("ICPPASTVirtSpecifier encountered unknown SpecifierKind "
					+ virtSpecifier.getKind().name() + " at " + loc);
		}
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ICPPASTClassVirtSpecifier classVirtSpecifier) {
		at(classVirtSpecifier);
		err("ClassVirtSpecifier: " + classVirtSpecifier.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(classVirtSpecifier));
	}

	@Override
	public int visit(ICPPASTDecltypeSpecifier decltypeSpecifier) {
		at(decltypeSpecifier);

		decltypeSpecifier.getDecltypeExpression().accept(this);
		stack.push(builder.Name_decltypeName(stack.pop(), locs.forNode(decltypeSpecifier), isMacroExpansion(decltypeSpecifier)));
		
		return PROCESS_ABORT;
	}

	@Override
	public int visit(ASTAmbiguousNode astAmbiguousNode) {
		at(astAmbiguousNode);
		err("AstAmbiguousNode: " + astAmbiguousNode.getRawSignature());
		throw new RuntimeException("NYI at " + locs.forNode(astAmbiguousNode));
	}
}
