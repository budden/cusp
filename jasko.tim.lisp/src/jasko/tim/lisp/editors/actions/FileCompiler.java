package jasko.tim.lisp.editors.actions;

import java.util.*;

import jasko.tim.lisp.*;
import jasko.tim.lisp.util.*;
import jasko.tim.lisp.views.ReplView;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.*;

import org.eclipse.jface.text.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.eclipse.ui.texteditor.*;

public class FileCompiler {
	public static final String COMPILE_PROBLEM_MARKER = "jasko.tim.lisp.lispCompile";
	
	public static void compileString(LispEditor editor, boolean switchToRepl) {
		IDocument doc = editor.getDocumentProvider().getDocument(
				  editor.getEditorInput());
		
		if (LispUtil.doParensBalance(doc)) {
			FileEditorInput file = (FileEditorInput)editor.getEditorInput();
			
			ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
			int offset = ts.getOffset();
			
			String exp = LispUtil.getTopLevelExpression(doc, offset);
			int topLevelOffset = LispUtil.getTopLevelOffset(doc, offset);
			
			SwankInterface swank = LispPlugin.getDefault().getSwank();
			String fileName = file.getName();
			String filePath = file.getPath().toString().replace(fileName, "");
			swank.sendCompileString(exp, fileName, filePath, topLevelOffset, editor.getPackage(), new CompileListener(editor));
			
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
	
	public static void compileFile(AbstractTextEditor editor, boolean switchToRepl) {
		editor.doSave(null);

		IDocument doc = editor.getDocumentProvider().getDocument(
				  editor.getEditorInput());
		
		if (LispUtil.doParensBalance(doc)) {
			FileEditorInput file = (FileEditorInput)editor.getEditorInput();
			
			SwankInterface swank = LispPlugin.getDefault().getSwank();
			swank.sendCompileFile(file.getPath().toString(), new CompileListener(editor));
			
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
	
	
	
	private static class CompileListener extends SwankRunnable {
		AbstractTextEditor editor;
		
		public CompileListener(AbstractTextEditor editor) {
			this.editor = editor;
		}
		
		public void run() {
			IFile file = ((FileEditorInput)editor.getEditorInput()).getFile();
			try {
				file.deleteMarkers(COMPILE_PROBLEM_MARKER, true, IResource.DEPTH_ZERO);
			} catch (CoreException e1) {
				e1.printStackTrace();
			}
			
			LispNode guts = result.getf(":return").getf(":ok");
			if (! guts.value.equals("nil")) {
				
				IDocument doc = editor.getDocumentProvider().getDocument(
						  editor.getEditorInput());
				
				for (LispNode error: guts.params) {
					String msg = error.getf(":message").value;
					String severity = error.getf(":severity").value;
					LispNode location = error.getf(":location");
					String fileName = location.getf(":file").value.replace("\\", "/");
					String buffer = location.getf(":buffer").value;
					int offset = 0;
					try {
						offset = Integer.parseInt(location.getf(":position").value);
					} catch (NumberFormatException e) {
					}
					
					int sev = IMarker.SEVERITY_WARNING;
					if (severity.equals(":error")) {
						sev = IMarker.SEVERITY_ERROR;
					}
					
					FileEditorInput input = (FileEditorInput)editor.getEditorInput(); 
					if (input.getPath().toString().replace("\\", "/").equals(fileName)
							|| input.getName().equals(buffer)) {
						Map<String, Object> attr = new HashMap<String, Object>();
						try {
							attr.put(IMarker.LINE_NUMBER, new Integer(doc.getLineOfOffset(offset) + 1));
						} catch (BadLocationException e1) {
							// This shouldn't ever happen, but just in case
							attr.put(IMarker.CHAR_START, new Integer(offset));
							attr.put(IMarker.CHAR_END, new Integer(offset+1));
							e1.printStackTrace();
						}
						
						attr.put(IMarker.MESSAGE, msg);
						attr.put(IMarker.SEVERITY, sev);
						
						try {
							MarkerUtilities.createMarker(file, attr, COMPILE_PROBLEM_MARKER);
						} catch (CoreException e) {
							System.out.println(e);
						}
						//LispBuilder.addMarker(file, msg, line, sev);
					} else {
						System.out.println(((FileEditorInput)editor.getEditorInput()).getPath().toString() + ":" + fileName);
					}
					
				}
			}
			
		}
	}

}
