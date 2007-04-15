package jasko.tim.lisp.editors.actions;


import jasko.tim.lisp.editors.LispEditor;



public class CompileAction extends LispAction {
	
	public CompileAction() {
	}
	
	public CompileAction(LispEditor editor) {
		super(editor);
	}
	
	public void run() {
		FileCompiler.compileFile(editor, true);
	}

}
