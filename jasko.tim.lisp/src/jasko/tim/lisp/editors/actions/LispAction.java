package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.SwankInterface;
import jasko.tim.lisp.util.LispUtil;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.*;

public abstract class LispAction extends Action implements IEditorActionDelegate {
	protected LispEditor editor;
	
	public LispAction() {
	}
	
	public LispAction(LispEditor editor) {
		this.editor = editor;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (LispEditor) targetEditor;
	}
	
	
	public void run(IAction action) {
		run();
	}
	

	public void selectionChanged(IAction action, ISelection selection) {

	}
	
	// Handy util functions
	
	protected SwankInterface getSwank() {
		return LispPlugin.getDefault().getSwank();
	}
	
	protected String getSymbol() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		
		String symbol = LispUtil.getCurrentFullWord(doc, offset);
		symbol = symbol.replace("'", "");
		symbol = symbol.replace("`", "");
		
		return symbol;
	}
	
	protected String getTopLevel() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		
		return LispUtil.getTopLevelExpression(doc, offset);
	}
	
	protected String getExpression() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		
		return LispUtil.getCurrentFullExpression(doc, offset);
	}
}