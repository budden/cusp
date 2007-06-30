package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.*;
import jasko.tim.lisp.util.*;
import jasko.tim.lisp.views.ReplView;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.builder.*;

import org.eclipse.jface.text.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.eclipse.core.resources.IFile;

public class FileCompiler {
	
	public static void compileString(LispEditor editor, boolean switchToRepl) {
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		
		if (LispUtil.doParensBalance(doc)) {
			FileEditorInput file = (FileEditorInput)editor.getEditorInput();
			
			ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
			int offset = ts.getOffset();
			
			String exp = LispUtil.getTopLevelExpression(doc, offset);
			int topLevelOffset = LispUtil.getTopLevelOffset(doc, offset);
			
			SwankInterface swank = LispPlugin.getDefault().getSwank();
			String fileName = file.getName();
			String filePath = file.getPath().toString().replace(fileName, ""); //TODO: this will not behave well for c:\my-lisp.lisp\my-lisp.lisp file
			swank.sendCompileString(exp, fileName, filePath, topLevelOffset, 
					LispUtil.getPackage(editor.getDocumentProvider().getDocument(editor.getEditorInput()).get(),topLevelOffset), 
					new LispBuilder.CompileListener(file.getFile()));
			
			if (switchToRepl) {
				IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
				try {
					page.showView(ReplView.ID);
				} catch (PartInitException e) {
					e.printStackTrace();
				}
			}
		} else {
			MessageBox mbox = new MessageBox(editor.getSite().getShell(),
					SWT.CANCEL | SWT.ICON_ERROR | SWT.APPLICATION_MODAL);
	       mbox.setText(")Mismatch(");
	       mbox.setMessage("Your parentheses are not balanced");
	       mbox.open();
		}
	}
	
	
	public static void compileFile(LispEditor editor, boolean switchToRepl) {
		editor.doSave(null); //TODO: if Autobuild is on, it will be compiled here

		IFile file = ((FileEditorInput)editor.getEditorInput()).getFile();
		boolean cancompile = LispBuilder.checkLisp(file);
		if(cancompile){
			LispBuilder.compileFile(file,true);
			if (switchToRepl) {
				IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
				try {
					page.showView(ReplView.ID);
				} catch (PartInitException e) {
					e.printStackTrace();
				}
			}
		} else {
			MessageBox mbox = new MessageBox(editor.getSite().getShell(),
					SWT.CANCEL | SWT.ICON_ERROR | SWT.APPLICATION_MODAL);
	       mbox.setText("Errors");
	       mbox.setMessage("Your file has errors.");
	       mbox.open();			
		}
	}
}
