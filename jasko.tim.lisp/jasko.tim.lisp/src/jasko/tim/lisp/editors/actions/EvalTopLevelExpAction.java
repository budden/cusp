
package jasko.tim.lisp.editors.actions;

import org.eclipse.ui.PlatformUI;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.editors.*;
import jasko.tim.lisp.preferences.PreferenceConstants;
import jasko.tim.lisp.views.ReplView;

public class EvalTopLevelExpAction extends LispAction {
	
	public EvalTopLevelExpAction() {
	}
	
	public EvalTopLevelExpAction(LispEditor editor) {
		super(editor);
	}

	public static void runStr(String exp){
        if (exp.length() == 0) return;

        if( LispPlugin.getDefault().getPreferenceStore()
        		.getBoolean(PreferenceConstants.SHOW_EVAL_IN_REPL) ){
    		try {
    			ReplView repl = (ReplView)PlatformUI.getWorkbench().getActiveWorkbenchWindow()
    				.getActivePage().findView(ReplView.ID);
    			String cleanCmd = exp.replace("\r", "");
    			repl.EvalStateHandle(exp+"\n", cleanCmd);
    			//ReplView.switchToRepl();
    		} catch ( Exception e ){
    			getSwank().sendEval(exp, null);
    		}        	
        } else {
			getSwank().sendEval(exp, null);        	
        }
	}
	
	public void run() {
		runStr(getTopLevel());
	}
}
