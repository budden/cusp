package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.LispEditor;


public class CompileSlowAction extends LispAction {
	
	public CompileSlowAction() {
	}
	
	public CompileSlowAction(LispEditor editor) {
		super(editor);
	}
	
	public void run() {
		FileCompiler.compileFile(editor, false);
	}
	
}