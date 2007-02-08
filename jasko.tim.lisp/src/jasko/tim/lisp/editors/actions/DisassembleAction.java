package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.util.LispUtil;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.*;

public class DisassembleAction extends Action implements IEditorActionDelegate {
	private LispEditor editor;
	
	public DisassembleAction() {
	}
	
	public DisassembleAction(LispEditor editor) {
		this.editor = editor;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (LispEditor) targetEditor;
	}
	
	public void run() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider()
				.getSelection();
		int offset = ts.getOffset();
		IDocument doc = editor.getDocumentProvider().getDocument(
				editor.getEditorInput());

		String sym = LispUtil.getCurrentFullWord(doc, offset);

		LispPlugin.getDefault().getSwank().sendDisassemble(sym, editor.getPackage(),
				new SwankRunnable() {
			public void run() {
				String assembly = result.getf(":return").getf(":ok").value;
				if (assembly.equals("nil")) {
					editor.showPopupInfo("Function not found.");
				} else {
					editor.showPopupInfo(assembly);
				}
			}
		});

		
	}

	public void run(IAction action) {
		run();
	}
	

	public void selectionChanged(IAction action, ISelection selection) {

	}
}
