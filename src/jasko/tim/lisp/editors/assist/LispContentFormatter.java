package jasko.tim.lisp.editors.assist;

import jasko.tim.lisp.util.*;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.formatter.*;

/**
 * 
 * @author Tim
 * @deprecated Not actually in use at this time.
 */
public class LispContentFormatter implements IContentFormatter {

	public void format(IDocument doc, IRegion region) {
		System.out.println("format");
		try {
			int firstLine = doc.getLineOfOffset(region.getOffset());
			int lastLine = doc.getLineOfOffset(region.getOffset() + region.getLength());
			for (int funcLine = firstLine; funcLine <= lastLine; ++funcLine) {
			//	int funcLine = doc.getLineOfOffset(comm.offset);
				IRegion lineInfo = doc.getLineInformation(funcLine);
				
				LispUtil.FunctionInfo fi = LispUtil.getCurrentFunctionInfo(doc, lineInfo.getOffset());
				
				String indent = LispIndenter.calculateIndent(fi, doc);
				String line = doc.get(lineInfo.getOffset(), lineInfo.getLength());
				
				String newLine = indent + line.trim();
				doc.replace(lineInfo.getOffset(), lineInfo.getLength(), newLine);
				
			}
		} catch (BadLocationException e) {
			
		}
	}

	public IFormattingStrategy getFormattingStrategy(String contentType) {
		System.out.println("getFormattingStrategy");
		return null;
	}

}
