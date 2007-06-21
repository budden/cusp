package jasko.tim.lisp.editors;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.preferences.PreferenceConstants;
import jasko.tim.lisp.util.LispUtil;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;

public class PairAutoEdit implements IAutoEditStrategy {

	private void cmdEnd(DocumentCommand c){
		c.shiftsCaret = false;
		c.caretOffset = c.offset + 1;
		c.doit = false;
		return;
	}
	// insert paired "", (), #| |#
	public void customizeDocumentCommand(IDocument d,
			DocumentCommand c) {
		try{
			IPreferenceStore prefs = LispPlugin.getDefault().getPreferenceStore();
			if("(".equals(c.text) && prefs.getBoolean(PreferenceConstants.PAIR_EDIT_BRACKETS)){
				c.text = "()";
				cmdEnd(c);
				return;
			}
			if( ("(".equals(c.text) && prefs.getBoolean(PreferenceConstants.PAIR_EDIT_BRACKETS) 
					&& prefs.getBoolean(PreferenceConstants.PAIR_SMART_BRACKETS) 
					&& d.getChar(c.offset) == '(')
			  ||("[".equals(c.text) && d.getChar(c.offset) == '(' 
				  && prefs.getBoolean(PreferenceConstants.PAIR_EDIT_BRACES) )){
				String txt = "( " + LispUtil.getCurrentExpression(d, c.offset, 0) + ")";
				c.text = txt;
				c.length = txt.length()-2;
				cmdEnd(c);
				return;						
			}
			if("\"".equals(c.text) && !(c.offset > 0 && d.getChar(c.offset-1) == '\\')
					&& prefs.getBoolean(PreferenceConstants.PAIR_EDIT_QUOTES)){
				c.text = "\"\"";
				cmdEnd(c);
				return;
			}
			if("|".equals(c.text) && c.offset > 0 && d.getChar(c.offset-1) == '#' 
				&& prefs.getBoolean(PreferenceConstants.PAIR_EDIT_COMMENTS)){
				c.text = "|  |#";
				cmdEnd(c);
				return;				
			}
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}

}
