package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.LispEditor;


public class CompileTopAction extends LispAction {
	boolean quick;
	
	public CompileTopAction() {
		quick = false;
	}
	
	public CompileTopAction(LispEditor editor, boolean quick) {
		super(editor);
		this.quick = quick;
	}

	public void run() {
		FileCompiler.compileString(editor, quick);
	}
}
