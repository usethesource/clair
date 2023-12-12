/** 
 * Copyright (c) 2016-2022, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
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
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage;
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

import io.usethesource.vallang.IBool;
import io.usethesource.vallang.IList;
import io.usethesource.vallang.IMap;
import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IString;
import io.usethesource.vallang.ITuple;
import io.usethesource.vallang.IValueFactory;

public class CDTParser {
	private final IValueFactory vf;
	private final PrintWriter stdErr;
	private final IScannerInfo scannerInfo;
	private final InternalFileContentProvider ifcp;
	private final IIndex idx;
	private final int options;
	private final IParserLogService log;

	private final List<String> path;

	public CDTParser(IValueFactory vf, PrintWriter stdOut, PrintWriter stdErr, IList stdLib, IList includePath, IMap standardMacros, IMap additionalMacros, boolean includeStdLib) {
		this.vf = vf;
		this.stdErr = stdErr;

		Map<String, String> macros = new HashMap<String, String>();
		additionalMacros.stream()
			.map(ITuple.class::cast)
			.forEach(tuple -> macros.put(((IString) tuple.get(0)).getValue(), ((IString) tuple.get(1)).getValue()));

		standardMacros.stream()
			.map(ITuple.class::cast)
			.forEach(tuple -> macros.put(((IString) tuple.get(0)).getValue(), ((IString) tuple.get(1)).getValue()));


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

	public IASTTranslationUnit parseFileAsC(ISourceLocation file, IString charset, IBool inferCharset) {
		FileContent fc = FileContent.create(file.toString(),
				((IString) Prelude.readFile(vf, false, file, charset.getValue(), inferCharset.getValue())).getValue().toCharArray());

		try {
			return GCCLanguage.getDefault().getASTTranslationUnit(fc, scannerInfo, ifcp, idx, options, log);
		} catch (CoreException e) {
			throw RuntimeExceptionFactory.io(vf.string(e.getMessage()));
		}
	}

	public IASTTranslationUnit parseFileAsCpp(ISourceLocation file, IString charset, IBool inferCharset) {
		FileContent fc = FileContent.create(file.toString(),
				((IString) Prelude.readFile(vf, false, file, charset.getValue(), inferCharset.getValue())).getValue().toCharArray());

		try {
			return GPPLanguage.getDefault().getASTTranslationUnit(fc, scannerInfo, ifcp, idx, options, log);
		} catch (CoreException e) {
			throw RuntimeExceptionFactory.io(vf.string(e.getMessage()));
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