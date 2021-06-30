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

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.index.IIndex;
import org.eclipse.cdt.core.index.IIndexFileLocation;
import org.eclipse.cdt.core.model.ILanguage;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.IParserLogService;
import org.eclipse.cdt.core.parser.IScannerInfo;
import org.eclipse.cdt.core.parser.ScannerInfo;
import org.eclipse.cdt.internal.core.dom.IIncludeFileResolutionHeuristics;
import org.eclipse.cdt.internal.core.index.CIndex;
import org.eclipse.cdt.internal.core.index.IIndexFragment;
import org.eclipse.cdt.internal.core.parser.IMacroDictionary;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContent;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContentProvider;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.rascalmpl.exceptions.RuntimeExceptionFactory;
import org.rascalmpl.library.Prelude;
import org.rascalmpl.values.IRascalValueFactory;

import io.usethesource.vallang.IList;
import io.usethesource.vallang.IMap;
import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IString;
import io.usethesource.vallang.ITuple;
import io.usethesource.vallang.IValueFactory;
import io.usethesource.vallang.type.TypeStore;

@SuppressWarnings("restriction")
public class CDTParser {
	private final IValueFactory vf;
	private final IRascalValueFactory rvf;
	private final PrintWriter stdOut;
	private final PrintWriter stdErr;
	private final TypeStore ts;
	private final IScannerInfo scannerInfo;
	private final InternalFileContentProvider ifcp;
	private final IIndex idx;
	private final int options;
	private final IParserLogService log;

	private final List<String> path;

	private static final Map<String, String> standardMacros;

	static {
		standardMacros = new HashMap<>();
		// from WinDiscoveredPathInfo.java:
		standardMacros.put("_M_IX86", "600");
		standardMacros.put("_WIN32", "1");
		// macros.put("_MSC_VER", "1400");
		standardMacros.put("__cdecl", "");
		standardMacros.put("__fastcall", "");
		standardMacros.put("__restrict", "");
		standardMacros.put("__sptr", "");
		standardMacros.put("__stdcall", "");
		standardMacros.put("__unaligned", "");
		standardMacros.put("__uptr", "");
		standardMacros.put("__w64", "");
		standardMacros.put("__forceinline", "__inline");
		standardMacros.put("__int8", "char");
		standardMacros.put("__int16", "short");
		standardMacros.put("__int32", "int");
		standardMacros.put("__int64", "long long");

		// additional:
		standardMacros.put("_MSC_VER", "1700");
		standardMacros.put("__cplusplus", "199711L");
		standardMacros.put("__thiscall", "");
		standardMacros.put("_CHAR16T", "");
		standardMacros.put("_NATIVE_WCHAR_T_DEFINED", "1");
		standardMacros.put("__nullptr", "nullptr");
		standardMacros.put("_MSC_EXTENSIONS", "1");
		standardMacros.put("__inline", "inline");
		standardMacros.put("__ptr32", "");
		standardMacros.put("__ptr64", "");
		standardMacros.put("__interface", "struct");

		standardMacros.put("__pragma(A)", "");
		standardMacros.put("__identifier(A)", "A");
//		standardMacros.put("__declspec(A)", "");
		standardMacros.put("_stdcall", "");

		standardMacros.put("_USE_DECLSPECS_FOR_SAL", "0");
		standardMacros.put("_DLL", "1");

		standardMacros.put("NDEBUG", "");
		standardMacros.put("WIN32", "");
		standardMacros.put("_WINDOWS", "");
		standardMacros.put("_WIN32_DCOM", "");
		standardMacros.put("_USRDLL", "");
		standardMacros.put("SSCF1_INCLUDED", "");
		standardMacros.put("LOGGINGTRACING_INCLUDED", "");
		standardMacros.put("_WINDLL", "");
		standardMacros.put("_UNICODE", "");
		standardMacros.put("UNICODE", "");
		standardMacros.put("_AFXDLL", "");

//		standardMacros.put("__INTELLISENSE__", "1");
	}

	public CDTParser(IValueFactory vf, IRascalValueFactory rvf, PrintWriter stdOut, PrintWriter stdErr, TypeStore ts,
			IList stdLib, IList includePath, IMap additionalMacros, boolean includeStdLib) {
		this.vf = vf;
		this.rvf = rvf;
		this.stdOut = stdOut;
		this.stdErr = stdErr;
		this.ts = ts;

		Map<String, String> macros = new HashMap<String, String>();
		additionalMacros.stream().map(ITuple.class::cast).forEach(tuple -> macros
				.put(tuple.get(0).toString().replace("\"", ""), tuple.get(1).toString().replace("\"", "")));
		macros.putAll(standardMacros);

		this.scannerInfo = new ScannerInfo(macros, null);

		this.ifcp = new InternalFileContentProvider() {
			@Override
			public InternalFileContent getContentForInclusion(String filePath, IMacroDictionary macroDictionary) {
				return (InternalFileContent) FileContent.createForExternalFileLocation(filePath);
			}

			@Override
			public InternalFileContent getContentForInclusion(IIndexFileLocation ifl, String astPath) {
				return (InternalFileContent) FileContent.create(ifl);
			}
		};

		this.path = new ArrayList<>();
		try {
			path.add(ResourcesPlugin.getWorkspace().getRoot().getProject("clair").getLocation().toString()
					+ "/includes");
		} catch (Throwable t) {
			stdOut.println("WARNING: ResourcesPlugin was null, can't get workspace; not overriding include files");
		}
		includePath.stream().forEach(it -> path.add(locToPath((ISourceLocation) it)));
		stdLib.stream().forEach(it -> path.add(locToPath((ISourceLocation) it)));

		IIncludeFileResolutionHeuristics ifrh = new IIncludeFileResolutionHeuristics() {
			@Override
			public String findInclusion(String include, String currentFile) {
				return findIncludeInPath(include, currentFile);
			}
		};

		this.ifcp.setIncludeResolutionHeuristics(ifrh);
		this.idx = new CIndex(new IIndexFragment[] {});
		this.options = ILanguage.OPTION_PARSE_INACTIVE_CODE;

		this.log = new IParserLogService() {
			@Override
			public void traceLog(String message) {
				// ctx.getStdErr().println(message);
			}

			@Override
			public boolean isTracing() {
				return true;
			}
		};
	}

	public IASTTranslationUnit parseFile(ISourceLocation file) {
		FileContent fc = FileContent.create(file.toString(),
				((IString) new Prelude(vf, rvf, stdOut, ts).readFile(file)).getValue().toCharArray());

		try {
			return GPPLanguage.getDefault().getASTTranslationUnit(fc, scannerInfo, ifcp, idx, options, log);
		} catch (CoreException e) {
			throw RuntimeExceptionFactory.io(vf.string(e.getMessage()), null, null);
		}
	}

	private String locToPath(ISourceLocation loc) {
		if (!loc.getScheme().equals("file"))
			throw new IllegalArgumentException("Will not convert non-file loc");
		return loc.getAuthority() + loc.getPath();
	}

	private boolean isRightFile(String include, String toMatch) {
		if (System.getProperty("os.name").contains("Win"))
			return include.equalsIgnoreCase(toMatch);
		return include.equals(toMatch);
	}

	private String checkDirectory(File dir, String fileName) {
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

	public String findIncludeInPath(String include, String currentFile) {
		include = include.trim().replace("\\", "/");
		String includeFilePath = include.substring(0, include.lastIndexOf('/') + 1);
		String includeFileName = include.substring(include.lastIndexOf('/') + 1);
		File currentFileDirectory;
		if (currentFile.startsWith("|file://")) {
			currentFileDirectory = new File(new File(currentFile.substring("|file://".length())).getParent(),
					includeFilePath);
		} else {
			currentFileDirectory = new File(new File(currentFile).getParentFile(), includeFilePath);
		}
		String found = checkDirectory(currentFileDirectory, includeFileName);
		if (found != null) {
			return found;
		}
		for (String path : path) {
			found = checkDirectory(new File(path, includeFilePath), includeFileName);
			if (found != null) {
				return found;
			}
		}
		stdErr.println("Include " + include + " for " + currentFile + " not found");
		stdErr.flush();
		return null;// TODO: restore exception here
	}
}