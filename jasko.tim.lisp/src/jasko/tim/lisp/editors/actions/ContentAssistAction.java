package jasko.tim.lisp.editors.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.ui.IEditorPart;

import jasko.tim.lisp.editors.ILispEditor;

public class ContentAssistAction extends LispAction {
    private ILispEditor editor;
    
    public ContentAssistAction () {}
    
    public ContentAssistAction (ILispEditor editor) {
        this.editor = editor;
    }
    
    public void setActiveEditor(IAction action, IEditorPart targetEditor) {
        editor = (ILispEditor)targetEditor;
    }
    
    public void run() {
        editor.showContentCompletions();
    }
}
