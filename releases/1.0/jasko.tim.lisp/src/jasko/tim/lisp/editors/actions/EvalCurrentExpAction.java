package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.LispEditor;

public class EvalCurrentExpAction extends LispAction {
    public EvalCurrentExpAction () {}
    
    public EvalCurrentExpAction (LispEditor editor) {
        super(editor);
    }
    
    public void run() {
    	EvalTopLevelExpAction.runStr(getCurrentExpression());
    }
}
