package jasko.tim.lisp.editors;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.preferences.PreferenceConstants;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;

public class PairAutoEdit implements IAutoEditStrategy {

	// insert paired "", (), #| |#
	public void customizeDocumentCommand(IDocument d,
			DocumentCommand c) {
		try{
			IPreferenceStore prefs = LispPlugin.getDefault().getPreferenceStore();
			if("(".equals(c.text) && prefs.getBoolean(PreferenceConstants.PAIR_EDIT_BRACKETS)){
				c.text = "()";
				c.shiftsCaret = false;
				c.caretOffset = c.offset + 1;
				c.doit = false;
				return;
			}
			if("\"".equals(c.text) && !(c.offset > 0 && d.getChar(c.offset-1) == '\\')
					&& prefs.getBoolean(PreferenceConstants.PAIR_EDIT_QUOTES)){
				c.text = "\"\"";
				c.shiftsCaret = false;
				c.caretOffset = c.offset + 1;
				c.doit = false;
				return;
			}
			if("|".equals(c.text) && c.offset > 0 && d.getChar(c.offset-1) == '#' 
				&& prefs.getBoolean(PreferenceConstants.PAIR_EDIT_COMMENTS)){
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
