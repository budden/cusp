/**
 * 
 */
package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.LispEditor;

/**
 * @author sk
 *
 */
public class EvalCurrentExpQuickAction extends LispAction {
    public EvalCurrentExpQuickAction () {}
    
    public EvalCurrentExpQuickAction (LispEditor editor) {
        super(editor);
    }
    
    public void run() {
    	EvalTopLevelExpQuickAction.runStr(getCurrentExpression());
    }

}
