package lang.cpp;

import java.net.URISyntaxException;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;
import org.eclipse.cdt.core.CCorePlugin;
import org.eclipse.cdt.core.model.CoreModel;
import org.eclipse.cdt.core.model.ICProject;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Plugin;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.utils.RuntimeExceptionFactory;

import io.usethesource.vallang.ISet;
import io.usethesource.vallang.ISetWriter;
import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IValueFactory;

public class Util extends Plugin {
	private final IValueFactory vf;
	private IEvaluatorContext ctx;
	private static final IWorkspaceRoot ROOT = ResourcesPlugin.getWorkspace().getRoot();

	public Util(IValueFactory vf, IEvaluatorContext ctx) {
		this.vf = vf;
		this.ctx = ctx;
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

	public void printProjectNames() {
		IProject[] projects = ROOT.getProjects();
		out("Projects:");
		prefix += 4;
		Stream.of(projects).forEach(it -> out(it.getName()));
		prefix -= 4;
	}

	public void foo() {
		CoreModel cm = CoreModel.getDefault();
		IProject foo = ROOT.getProject("Foo");
		ICProject cp = cm.create(foo);
		// out("" + cp.exists());
		// out("" + cp.getCModel());

		try {
			IProjectDescription descr = foo.getDescription();
			IProgressMonitor mon = new NullProgressMonitor();
			CCorePlugin ccp = CCorePlugin.getDefault();
			out("CCorePlugin=" + ccp);
			IProject cprj = ccp.createCProject(descr, foo, mon, "prj");
		} catch (OperationCanceledException | CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public ISet projects() {
		IProject[] projects = ROOT.getProjects();
		ISetWriter w = vf.setWriter();

		for (IProject p : projects) {
			w.insert(makeProject(p));
		}

		return w.done();
	}

	public ISourceLocation makeProject(IProject r) {
		try {
			return vf.sourceLocation("project", r.getName(), "");
		} catch (URISyntaxException e) {
			// won't happen
			throw RuntimeExceptionFactory.malformedURI("project://" + r.getName(), null, null);
		}
	}
}
