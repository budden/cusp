package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.views.ReplView;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.AbstractTextEditor;

public class SwitchToReplAction extends Action implements IEditorActionDelegate {	
	public SwitchToReplAction() {
	}
	
	public SwitchToReplAction(AbstractTextEditor editor) {
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
	}
	
	public void run() {
		ReplView.switchToRepl();
	}

	public void run(IAction action) {
		run();
	}

	public void selectionChanged(IAction action, ISelection selection) {

	}
}