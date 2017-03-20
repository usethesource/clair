package lang.cpp.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.index.IIndexFileLocation;
import org.eclipse.cdt.core.model.ILanguage;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.ParserUtil;
import org.eclipse.cdt.core.parser.ScannerInfo;
import org.eclipse.cdt.internal.core.dom.IIncludeFileResolutionHeuristics;
import org.eclipse.cdt.internal.core.parser.IMacroDictionary;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContent;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContentProvider;
import org.eclipse.core.runtime.CoreException;
import org.rascalmpl.interpreter.IEvaluatorContext;

import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IValue;
import io.usethesource.vallang.IValueFactory;

@SuppressWarnings("restriction")
public class Test {
	private AST builder;
	private IValueFactory vf;

	public Test(IValueFactory vf) {
		this.vf = vf;
		this.builder = new AST(vf);
	};

	public static IASTTranslationUnit ast;

	public IValue foobar(ISourceLocation loc, IEvaluatorContext ctx) {
		IASTTranslationUnit ast = bla(loc, ctx);

		Parser parser = new Parser(vf);
		parser.setIEvaluatorContext(ctx);

		IValue v = null;
		try {
			v = parser.convertCdtToRascal(ast);
			return v;
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return builder.Declaration_translationUnit(vf.listWriter().done(), vf.sourceLocation("NYI"));
	}

	public IASTTranslationUnit bla(ISourceLocation loc, IEvaluatorContext ctx) {
		ILanguage lang = GPPLanguage.getDefault();

		FileContent fc = FileContent
				.createForExternalFileLocation("C:\\Users\\310263173\\workspace\\clair\\src\\test\\test.cpp");

		try {

			InternalFileContentProvider crf = new InternalFileContentProvider() {

				@Override
				public InternalFileContent getContentForInclusion(IIndexFileLocation ifl, String astPath) {
					return (InternalFileContent) FileContent.create(ifl);
				}

				@Override
				public InternalFileContent getContentForInclusion(String path, IMacroDictionary macroDictionary) {
					return (InternalFileContent) FileContent.createForExternalFileLocation(path);
				}
			};

			IIncludeFileResolutionHeuristics ifrh = new IIncludeFileResolutionHeuristics() {

				String[] classPath;

				{
					classPath = new String[1];
					classPath[0] = "c:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\include";
				}

				@Override
				public String findInclusion(String include, String currentFile) {
					if (include.contains("/")) {
						include = include.substring(include.lastIndexOf("/") + 1, include.length());
					}

					FileSearch fileSearch = new FileSearch();

					for (int i = 0; i < classPath.length; i++) {
						fileSearch.searchDirectory(new File(classPath[i]), include);
					}

					if (fileSearch.getResult().size() == 0)
						return null;

					String[] temp = new String[fileSearch.getResult().size()];

					for (int i = 0; i < temp.length; i++) {
						temp[i] = fileSearch.getResult().get(i);
					}

					String bestLocation = selectBest(temp, currentFile.toCharArray());
					if (bestLocation == null)
						return null;
					return bestLocation;
				}

				private String selectBest(String[] files, char[] currentFullPath) {
					String best = files[0];
					int bestScore = computeScore(best.toCharArray(), currentFullPath);

					for (int i = 1; i < files.length; i++) {
						String file = files[i];
						int score = computeScore(file.toCharArray(), currentFullPath);
						if (score > bestScore) {
							bestScore = score;
							best = file;
						}
					}
					return best;
				}

				private int computeScore(char[] path1, char[] path2) {
					final int limit = Math.min(path1.length, path2.length);
					int match = 0;
					for (int i = 0; i < limit; i++) {
						if (path1[i] != path2[i])
							break;
						if (path1[i] == '/')
							match = i;
					}
					// Prefer shortest path with longest matches with.
					return (match << 16) - path1.length;
				}
			};

			crf.setIncludeResolutionHeuristics(ifrh);

			ast = lang.getASTTranslationUnit(fc, new ScannerInfo(), crf, null,
					ILanguage.OPTION_IS_SOURCE_UNIT | ILanguage.OPTION_PARSE_INACTIVE_CODE,
					ParserUtil.getParserLogService());
			// I'm not sure how to compile the files
			ctx.getStdOut().println(ast.getRawSignature());
			return ast;

		} catch (CoreException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	public static class FileSearch {

		private String fileNameToSearch;
		private List<String> result = new ArrayList<String>();

		public String getFileNameToSearch() {
			return fileNameToSearch;
		}

		public void setFileNameToSearch(String fileNameToSearch) {
			this.fileNameToSearch = fileNameToSearch;
		}

		public List<String> getResult() {
			return result;
		}

		public void searchDirectory(File directory, String fileNameToSearch) {

			setFileNameToSearch(fileNameToSearch);

			if (directory.isDirectory()) {
				search(directory);
			}

		}

		private void search(File file) {

			if (file.isDirectory()) {
				// System.out.println("Searching directory ... " +
				// file.getAbsoluteFile());

				// do you have permission to read this directory?
				if (file.canRead()) {
					for (File temp : file.listFiles()) {
						if (temp.isDirectory()) {
							search(temp);
						} else {
							if (getFileNameToSearch().equals(temp.getName())) {
								result.add(temp.getAbsoluteFile().toString());
							}

						}
					}

				}
			}

		}

	}
}