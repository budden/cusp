
package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.editors.*;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.util.*;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;

public class EvalTopLevelExpAction extends Action implements IEditorActionDelegate {
	private LispEditor editor;
	
	public EvalTopLevelExpAction() {
	}
	
	public EvalTopLevelExpAction(LispEditor editor) {
		this.editor = editor;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (LispEditor) targetEditor;
	}
	
	public void run() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		
		String exp = LispUtil.getTopLevelExpression(doc, offset);
		
		System.out.println("*" + exp);
		SwankInterface swank = LispPlugin.getDefault().getSwank();
		swank.sendEval(exp, new SwankRunnable() {
			public void run() {
				LispNode res = this.result.getf(":return").getf(":ok").getf(":present");
				String display = "";
				for (LispNode kid: res.params) {
					display += kid.get(0).value + "\n";
				}
				display = display.trim();
				editor.showPopupInfo("=> " + display);
			}
		});
		
	}

	public void run(IAction action) {
		run();
	}
	

	public void selectionChanged(IAction action, ISelection selection) {

	}

}
