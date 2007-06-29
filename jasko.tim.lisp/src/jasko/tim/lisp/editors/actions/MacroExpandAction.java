package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.editors.*;


public class MacroExpandAction extends LispAction {
	private boolean all;
	
	public MacroExpandAction() {
		all = false;
	}
	
	public MacroExpandAction(LispEditor editor, boolean all) {
		super(editor);
		this.all = all;
	}
	
	public void run() {
		String exp = getExpression();
		
		getSwank().sendMacroExpand(exp, new SwankRunnable() {
			public void run() {
				String expanded = result.getf(":return").getf(":ok").value;
				editor.showPopupInfo(expanded);
			}
		}, all, getPackage());
		
	}


}
