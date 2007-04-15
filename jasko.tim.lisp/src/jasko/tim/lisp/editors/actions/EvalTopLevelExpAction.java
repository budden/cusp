
package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.*;
import jasko.tim.lisp.swank.*;

public class EvalTopLevelExpAction extends LispAction {
	
	public EvalTopLevelExpAction() {
	}
	
	public EvalTopLevelExpAction(LispEditor editor) {
		super(editor);
	}
	
	public void run() {
		String exp = getTopLevel();
		
		getSwank().sendEval(exp, new SwankRunnable() {
			public void run() {
				LispNode res = this.result.getf(":return").getf(":ok").getf(":present");
				String display = "";
				for (LispNode kid: res.params) {
					display += kid.get(0).value + "\n";
				}
				display = display.trim();
				editor.showPopupInfo("=> " + display);
			}
		});
		
	}
}
