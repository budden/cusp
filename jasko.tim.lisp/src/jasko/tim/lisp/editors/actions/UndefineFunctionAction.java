package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.*;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.util.LispUtil;

import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.*;
import org.eclipse.ui.texteditor.*;


public class UndefineFunctionAction extends Action implements IEditorActionDelegate {
	private LispEditor editor;
	
	public UndefineFunctionAction() {
	}
	
	public UndefineFunctionAction(AbstractTextEditor editor) {
		this.editor = (LispEditor) editor;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (LispEditor) targetEditor;
	}
	
	public void run() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		String symbol = LispUtil.getCurrentFullWord(
				editor.getDocumentProvider().getDocument(editor.getEditorInput()), offset);
		SwankInterface swank = LispPlugin.getDefault().getSwank();
		
		InputDialog win = new InputDialog(editor.getSite().getShell(), "Undefine",
				"Undefine the following symbol:", symbol, new IInputValidator() {
			public String isValid(String newText) {
				if (newText.equals("")) {
					return "Symbol may not be blank.";
				}
				
				return null;
			}
		});
		
		if (win.open() == InputDialog.OK && !win.getValue().equals("")) {
			swank.sendUndefine(win.getValue(), editor.getPackage(), null);
		}
	}

	public void run(IAction action) {
		run();
	}

	public void selectionChanged(IAction action, ISelection selection) {

	}
}
