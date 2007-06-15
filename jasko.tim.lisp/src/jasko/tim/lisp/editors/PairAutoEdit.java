package jasko.tim.lisp.editors;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;

public class PairAutoEdit implements IAutoEditStrategy {

	// insert paired "", (), #| |#
	public void customizeDocumentCommand(IDocument d,
			DocumentCommand c) {
		try{
			if("(".equals(c.text)){
				c.text = "()";
				c.shiftsCaret = false;
				c.caretOffset = c.offset + 1;
				c.doit = false;
				return;
			}
			if("\"".equals(c.text) && !(c.offset > 0 && d.getChar(c.offset-1) == '\\')){
				c.text = "\"\"";
				c.shiftsCaret = false;
				c.caretOffset = c.offset + 1;
				c.doit = false;
				return;
			}
			if("|".equals(c.text) && c.offset > 0 && d.getChar(c.offset-1) == '#'){
				c.text = "|  |#";
				c.shiftsCaret = false;
				c.caretOffset = c.offset + 2;
				c.doit = false;
				return;				
			}
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}

}
