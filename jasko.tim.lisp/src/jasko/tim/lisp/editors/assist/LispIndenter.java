package jasko.tim.lisp.editors.assist;

import jasko.tim.lisp.*;
import jasko.tim.lisp.editors.LispPartitionScanner;
import jasko.tim.lisp.swank.SwankInterface;
import jasko.tim.lisp.util.*;

import org.eclipse.jface.text.*;

/**
 * Doesn't actually handle the indenting that is done when you press Tab.
 *  I find the caret moves more predictably if I do that in IndentAction
 * @see jasko.tim.lisp.editors.actions.IndentAction
 * @author Tim
 *
 */
public class LispIndenter implements IAutoEditStrategy {


	public void customizeDocumentCommand(IDocument doc, DocumentCommand comm) {
		try {
			if ((comm.text.endsWith("\n") || comm.text.endsWith("\r"))
					&& doc.getContentType(comm.offset) != LispPartitionScanner.LISP_STRING) {
				LispUtil.FunctionInfo fi = LispUtil.getCurrentFunctionInfo(doc, comm.offset);
				
				comm.text += calculateIndent(fi, doc);
				
			}
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}
	
	public static String calculateIndent(LispUtil.FunctionInfo fi, IDocument doc) {
		if (fi.offset < 0) {
			return "";
		}
		LispUtil.FunctionInfo prevFunc = LispUtil.getCurrentFunctionInfo(doc, fi.offset);
		LispUtil.FunctionInfo prev2Func = LispUtil.getCurrentFunctionInfo(doc, prevFunc.offset);
		
		//System.out.println("***" + fi.name);
		//System.out.println(prevFunc.name);
		//System.out.println(prev2Func.name);
		
		try {
			int funcLine = doc.getLineOfOffset(fi.offset);
			int funcLineOffset = doc.getLineOffset(funcLine);
			
			String indent = "";
			for (int i=funcLineOffset; i<fi.offset; ++i) {
				if (doc.getChar(i) == '\t') {
					indent += "\t";
				} else {
					indent += " ";
				}
			}
			
			SwankInterface swank = LispPlugin.getDefault().getSwank();
			if (swank.fletIndents.containsKey(prev2Func.name)) {
				indent += swank.fletIndents.get(prev2Func.name);
			} else if (swank.indents.containsKey(fi.name)) {
				indent += swank.indents.get(fi.name);
			} else {
				if (fi.name.startsWith("def") || fi.name.startsWith(":")) {
					indent += "  ";
				} else {
					for (int i=fi.offset+fi.name.length()+1; i<doc.getLength(); ++i) {
						char c = doc.getChar(i);
						if (!Character.isWhitespace(c)) {
							for (int j=0; j<i-fi.offset; ++j) {
								if (c == '\t') {
									indent += "\t";
								} else {
									indent += " ";
								}
							}
							indent = indent.replace("        ", "\t");
							return indent;
						} else if (c == '\n') {
							break;
						}
					}
					indent += "  ";
				}
			}
			indent = indent.replace("        ", "\t");
			return indent;
		} catch (BadLocationException e) {
			e.printStackTrace();
			return "";
		}
	}
	

}
