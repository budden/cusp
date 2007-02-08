package jasko.tim.lisp.editors.actions;


import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;
import org.eclipse.ui.texteditor.*;


public class CompileAction extends Action implements IEditorActionDelegate {
	private AbstractTextEditor editor;
	
	public CompileAction() {
	}
	
	public CompileAction(AbstractTextEditor editor) {
		this.editor = editor;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (AbstractTextEditor) targetEditor;
	}
	
	public void run() {
		FileCompiler.compileFile(editor, true);
	}

	public void run(IAction action) {
		FileCompiler.compileFile(editor, true);
	}

	public void selectionChanged(IAction action, ISelection selection) {

	}
}
