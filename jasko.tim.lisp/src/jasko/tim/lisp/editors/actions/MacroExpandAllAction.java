package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.LispEditor;

public class MacroExpandAllAction extends MacroExpandAction {
	public MacroExpandAllAction() {
		super();
	}
	
	public MacroExpandAllAction(LispEditor editor) {
		super (editor, false);
	}
}
