package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.util.*;
import jasko.tim.lisp.views.ReplView;
import jasko.tim.lisp.editors.ILispEditor;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.builder.*;

import org.eclipse.jface.text.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.*;

public class FileCompiler {
	
	public static void compileString(ILispEditor editor, boolean switchToRepl) {
		IDocument doc = editor.getDocument();
		
		if (LispUtil.doParensBalance(doc)) {
			
			ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
			int offset = ts.getOffset();
			
			String exp = LispUtil.getTopLevelExpression(doc, offset);
			int topLevelOffset = LispUtil.getTopLevelOffset(doc, offset);

			LispBuilder.compileFilePart(editor.getIFile(), exp, topLevelOffset);
			if (switchToRepl) {
				IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
				try {
					page.showView(ReplView.ID);
				} catch (PartInitException e) {
					e.printStackTrace();
				}
			}
		} else {
			MessageBox mbox = new MessageBox(editor.getTextWidget().getShell(),
					SWT.CANCEL | SWT.ICON_ERROR | SWT.APPLICATION_MODAL);
	       mbox.setText(")Mismatch(");
	       mbox.setMessage("Your parentheses are not balanced");
	       mbox.open();
		}
	}
	
	
	public static void compileFile(ILispEditor editor, boolean switchToRepl) {
		if(editor instanceof LispEditor)
		{
			((LispEditor)editor).doSaveNoCompile();
		}
		boolean cancompile = LispBuilder.checkLisp(editor.getIFile());

		if(cancompile){
			
			LispBuilder.compileFile(editor.getIFile(),true);
			if (switchToRepl) {
				IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
				try {
					page.showView(ReplView.ID);
				} catch (PartInitException e) {
					e.printStackTrace();
				}
			}
		} else {
			MessageBox mbox = new MessageBox(editor.getTextWidget().getShell(),
					SWT.CANCEL | SWT.ICON_ERROR | SWT.APPLICATION_MODAL);
	       mbox.setText("Errors");
	       mbox.setMessage("Your file has errors.");
	       mbox.open();			
		}
	}
}
