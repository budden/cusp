package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.ILispEditor;
import jasko.tim.lisp.util.LispUtil;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.*;
import org.eclipse.ui.IEditorPart;

public class SelectCurrentExpressionAction extends LispAction {

    private ILispEditor editor;
    
    public SelectCurrentExpressionAction () {}
    
    public SelectCurrentExpressionAction (ILispEditor editor) {
        this.editor = editor;
    }
    
    public void setActiveEditor(IAction action, IEditorPart targetEditor) {
        editor = (ILispEditor)targetEditor;
    }
    
    public void run () {
        ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
        int offset = ts.getOffset();
        IDocument doc = editor.getDocument();
        
        try {
            int[] range = LispUtil.getCurrentExpressionRange(doc, offset);
            if (range != null) editor.getSelectionProvider().setSelection(new TextSelection(doc, range[0], range[1]));
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
        
    }
    
}
