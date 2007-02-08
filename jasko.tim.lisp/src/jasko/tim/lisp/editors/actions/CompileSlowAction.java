package jasko.tim.lisp.editors.actions;

import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.*;
import org.eclipse.ui.texteditor.AbstractTextEditor;

public class CompileSlowAction extends Action implements IEditorActionDelegate {
	private AbstractTextEditor editor;
	
	public CompileSlowAction() {
	}
	
	public CompileSlowAction(AbstractTextEditor editor) {
		this.editor = editor;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (AbstractTextEditor) targetEditor;
	}
	
	public void run() {
		FileCompiler.compileFile(editor, false);
	}

	public void run(IAction action) {
		FileCompiler.compileFile(editor, false);
	}

	public void selectionChanged(IAction action, ISelection selection) {

	}
}