package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.ILispEditor;
import jasko.tim.lisp.util.LispUtil;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.*;
import org.eclipse.ui.IEditorPart;

public class CommentingAction extends LispAction {

    private ILispEditor editor;
    
    public CommentingAction () {}
    
    public CommentingAction (ILispEditor editor) {
        this.editor = editor;
    }
    
    public void setActiveEditor(IAction action, IEditorPart targetEditor) {
        editor = (ILispEditor)targetEditor;
    }
    
    public void run () {
        ITextSelection ts = 
        	(ITextSelection) editor.getSelectionProvider().getSelection();
        int offset = ts.getOffset();
        IDocument doc = editor.getDocument();
        
        // if selection length > 0, comment out
        if( ts.getLength() > 0 ){
        	String txt = "#|" + ts.getText() + "|#";
        	try{
        		doc.replace(offset, ts.getLength(), txt);
        		editor.getSelectionProvider()
        		  .setSelection(new TextSelection(doc,offset,txt.length()));
        	}
        	catch (BadLocationException e){
                e.printStackTrace();        		
        	}
        } else {  
    	// if selection length = 0 and caret is before #| or after |#
        // remove commenting symbols
        	        	
        }
    }
    
}
