package lang.cpp.internal;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.net.URISyntaxException;
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

				if (fileName.trim().startsWith("|")) {
					// here we used Rascal loc syntax which ended up in the CDT's file path somehow
					
					try {
						ISourceLocation tmp = (ISourceLocation) locParser.read(vf, TypeFactory.getInstance().sourceLocationType(), new StringReader(fileName));
						
						// that would be weird, and also we'd overwrite it below
						assert !tmp.hasOffsetLength();

						return vf.sourceLocation(
								tmp,
								astFileLocation.getNodeOffset(), 
								astFileLocation.getNodeLength());
					} 
					catch (FactParseError | FactTypeUseException | IOException e) {
						return unknownPreciseLoc();
					}
				}
				else {
					// this parses the fileName in an OS-ARCH dependent way
					ISourceLocation loc = URIUtil.createFileLocation(fileName.trim());

					return vf.sourceLocation(
						loc, 
						astFileLocation.getNodeOffset(),
						astFileLocation.getNodeLength()
					);
				}
			}

			return unknownPreciseLoc();
		}
		catch (RuntimeException e) {
			throw new RuntimeException("getting location for node " + node + " failed", e);
		} 
		catch (URISyntaxException e) {
			throw new RuntimeException("location for not " + node + " has URISyntaxException: " + e, e);
		}
	}

	private ISourceLocation unknownPreciseLoc() {
		return vf.sourceLocation(URIUtil.correctLocation("unknown", "", UUID.randomUUID().toString()), 0, 0);
	}
}
