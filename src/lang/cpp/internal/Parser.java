package lang.cpp.internal;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage;
import org.eclipse.cdt.core.index.IIndex;
import org.eclipse.cdt.core.model.ILanguage;
import org.eclipse.cdt.core.parser.DefaultLogService;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.IParserLogService;
import org.eclipse.cdt.core.parser.IScannerInfo;
import org.eclipse.cdt.core.parser.IncludeFileContentProvider;
import org.eclipse.cdt.core.parser.ScannerInfo;
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
	CdtToRascalVisitor visitor;

	public Parser(IValueFactory vf) {
		this.vf = vf;
		this.builder = new AST(vf);
		this.visitor = new CdtToRascalVisitor(vf);
	}

	public IValue parseCpp(ISourceLocation file, IEvaluatorContext ctx) {
		if (ctx != null) {
			this.ctx = ctx;
			visitor.setIEvaluatorContext(ctx);
		}
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
		return visitor.convert(translationUnit);
	}
}
