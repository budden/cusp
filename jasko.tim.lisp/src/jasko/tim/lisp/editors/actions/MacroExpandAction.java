package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.*;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.editors.*;
import jasko.tim.lisp.util.*;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;

public class MacroExpandAction extends Action implements IEditorActionDelegate {
	private LispEditor editor;
	private boolean all;
	
	public MacroExpandAction() {
		all = false;
	}
	
	public MacroExpandAction(LispEditor editor, boolean all) {
		this.editor = editor;
		this.all = all;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (LispEditor) targetEditor;
	}
	
	public void run() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		
		String exp = LispUtil.getCurrentFullExpression(doc, offset);
		
		LispPlugin.getDefault().getSwank().sendMacroExpand(exp, new SwankRunnable() {
			public void run() {
				String expanded = result.getf(":return").getf(":ok").value;
				editor.showPopupInfo(expanded);
			}
		}, all, editor.getPackage());
		
	}

	public void run(IAction action) {
		run();
	}
	

	public void selectionChanged(IAction action, ISelection selection) {

	}

}
