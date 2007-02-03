package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.LispEditor;

import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.*;

public class CompileTopAction extends Action implements IEditorActionDelegate {
	private LispEditor editor;
	boolean quick;
	
	public CompileTopAction() {
		quick = false;
	}
	
	public CompileTopAction(LispEditor editor, boolean quick) {
		this.editor = editor;
		this.quick = quick;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (LispEditor) targetEditor;
	}
	
	public void run() {
		FileCompiler.compileString(editor, quick);
	}

	public void run(IAction action) {
		run();
	}
	

	public void selectionChanged(IAction action, ISelection selection) {
	}
}
