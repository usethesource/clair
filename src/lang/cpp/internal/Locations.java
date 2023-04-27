package lang.cpp.internal;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.util.UUID;

import org.eclipse.cdt.core.dom.ast.IASTFileLocation;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.rascalmpl.uri.URIUtil;

import io.usethesource.vallang.ISourceLocation;
import io.usethesource.vallang.IValueFactory;
import io.usethesource.vallang.exceptions.FactParseError;
import io.usethesource.vallang.exceptions.FactTypeUseException;
import io.usethesource.vallang.io.StandardTextReader;
import io.usethesource.vallang.type.TypeFactory;

public class Locations {
    private final IValueFactory vf;
    private final StandardTextReader locParser = new StandardTextReader();
    
    public Locations(IValueFactory vf, PrintWriter stdErr) {
        this.vf = vf;
    }

	public ISourceLocation forNode(IASTNode node) {
		try {
		IASTFileLocation astFileLocation = node.getFileLocation();

		if (astFileLocation != null) {
			String fileName = astFileLocation.getFileName();

			// TODO: JV; I find this looking kind of tricky. Where would the wrong
			// slashes come from and should we not solve the problem at the source?
			// is there a way to get a "File" abstraction or Resource from CDT?
			fileName = fileName.replace('\\', '/');

			if (fileName.trim().startsWith("|")) {
				try {
					ISourceLocation tmp = (ISourceLocation) locParser.read(vf, TypeFactory.getInstance().sourceLocationType(), new StringReader(fileName));
					
					return vf.sourceLocation(
							tmp,
							astFileLocation.getNodeOffset(), 
							astFileLocation.getNodeLength());
				} catch (FactParseError | FactTypeUseException | IOException e) {
		
					return unknownPreciseLoc();
				}
			}

			if (!fileName.startsWith("/")) {
				fileName = "/" + fileName;
			}

			return vf.sourceLocation(
						vf.sourceLocation(fileName), 
						astFileLocation.getNodeOffset(),
						astFileLocation.getNodeLength()
					);
		}

		return unknownPreciseLoc();
	}
	catch (RuntimeException e) {
		throw new RuntimeException("AST at " + node + " failed", e);
	}
	}

	private ISourceLocation unknownPreciseLoc() {
		return vf.sourceLocation(URIUtil.correctLocation("unknown", "", UUID.randomUUID().toString()), 0, 0);
	}
}
