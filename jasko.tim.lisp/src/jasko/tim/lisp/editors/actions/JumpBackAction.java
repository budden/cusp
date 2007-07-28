package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.ILispEditor;
import jasko.tim.lisp.util.LispUtil;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.*;
import org.eclipse.ui.IEditorPart;

public class JumpBackAction extends LispAction {

    private ILispEditor editor;
    
    public JumpBackAction () {}
    
    public JumpBackAction (ILispEditor editor) {
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
        int docLen = doc.getLength();
        int jumpTo = 0;
        
        if( offset > docLen || offset <= 0 ){
        	return;
        }
        
        try {
        	char c = doc.getChar(offset-1);
        	if( c == '\n' ){
        		jumpTo = offset - 1;
        	} else {
            	if( c == ')'){
                    int[] range = LispUtil.getCurrentExpressionRange(doc, offset-1);
                    if( range == null ){
                    	jumpTo = 0;
                    } else {
                        jumpTo = range[0];                	
                    }
            	} else if( Character.isWhitespace(c) ){
            		jumpTo = offset - 1;
            	} else if (c == '('){
            			jumpTo = offset-1;
            	} else {
                	int[] range = 
                		LispUtil.getCurrentFullWordRange(doc, offset-1, false);
                	if( range == null ){
                		jumpTo = 0;
                	} else {
                    	jumpTo = range[0];
                	}
            	}
            	if( jumpTo < docLen && jumpTo > 0){
            		c = doc.getChar(jumpTo-1);
            	
    	        	while( jumpTo > 0 && Character.isWhitespace(c) 
    	            		&& c != '\n'){
    	        		--jumpTo;
                		c = doc.getChar(jumpTo-1);
    	        	}
            	}        		
        	}        	
        } catch (BadLocationException ex) {
            ex.printStackTrace();
        }
        
        
        editor.getSelectionProvider()
            .setSelection(new TextSelection(doc, jumpTo, 0));
    }
    
}
