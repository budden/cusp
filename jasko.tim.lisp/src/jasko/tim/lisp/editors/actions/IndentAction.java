package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.util.*;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.editors.LispPartitionScanner;
import jasko.tim.lisp.editors.assist.*;

import org.eclipse.jface.text.*;

/**
 * This is probably not the right place to do indentation, but it's a lot easier to do it here than in
 *  IAutoEditStrategy.
 * If somebody has a better understanding of these things, feel free to implement this in a more
 *  proper fashion.
 * @author Tim Jasko
 * @see jasko.tim.lisp.editors.assist.LispIndenter
 */
public class IndentAction extends LispAction {
	public static final String ID = "jasko.tim.lisp.actions.IndentAction";
	
	public IndentAction() {
	}
	
	public IndentAction(LispEditor editor) {
		super(editor);
	}
	
	public void run() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());

		try {
			int firstLine = doc.getLineOfOffset(offset);
			int lastLine = doc.getLineOfOffset(offset + ts.getLength());
			for (int funcLine = firstLine; funcLine <= lastLine; ++funcLine) {
				IRegion lineInfo = doc.getLineInformation(funcLine);
				
                // do not mess with indentation of multiline strings! 
                // todo -- it might be cleaner architecturally to have this check happen in LispUtil,
                //     but the code there is still somewhat opaque to me at the moment, and clobbering spacing in 
                //     multiline strings can be *really* bad, so let expediency overtake purity for the nonce.
                //     - Chas Emerick
                if (doc.getContentType(lineInfo.getOffset()).equals(LispPartitionScanner.LISP_STRING) &&
                        !doc.get(lineInfo.getOffset(), lineInfo.getLength()).trim().startsWith("\"")) continue;
                
				LispUtil.FunctionInfo fi = LispUtil.getCurrentFunctionInfo(doc, lineInfo.getOffset());
				
				String indent = LispIndenter.calculateIndent(fi, doc);
				String line = doc.get(lineInfo.getOffset(), lineInfo.getLength());
				
				String newLine = indent + line.trim();
				doc.replace(lineInfo.getOffset(), lineInfo.getLength(), newLine);
				
			}
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}

}
