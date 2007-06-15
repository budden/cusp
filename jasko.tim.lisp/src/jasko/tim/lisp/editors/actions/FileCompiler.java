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
import org.eclipse.core.runtime.Path;

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
			swank.sendCompileString(exp, fileName, filePath, topLevelOffset, editor.getPackage(), 
					new CompileListener(((FileEditorInput)editor.getEditorInput()).getFile()));
			
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
			swank.sendCompileFile(file.getPath().toString(), 
					new CompileListener(((FileEditorInput)editor.getEditorInput()).getFile()));
			
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
	
	
	
	public static class CompileListener extends SwankRunnable {
 		IFile file;
  		
 		public CompileListener(IFile file) {
 			this.file = file;
  		}
  		
  		public void run() {
 			ArrayList<String> files = new ArrayList<String>(); 
 			if ( file != null ){
 				try {
 					file.deleteMarkers(COMPILE_PROBLEM_MARKER, true, IResource.DEPTH_ZERO);
 				} catch (CoreException e1) {
 					e1.printStackTrace();
 				}				
  			}
  			
  			LispNode guts = result.getf(":return").getf(":ok");
 			if (! guts.value.equalsIgnoreCase("nil")) {
 				IWorkspaceRoot wk = ResourcesPlugin.getWorkspace().getRoot();
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
					if (severity.equalsIgnoreCase(":error")) {
  						sev = IMarker.SEVERITY_ERROR;
  					}
  					
 					Map<String, Object> attr = new HashMap<String, Object>();
 					attr.put(IMarker.CHAR_START, new Integer(offset));
 					attr.put(IMarker.CHAR_END, new Integer(offset+1));
 					
 					attr.put(IMarker.MESSAGE, msg);
 					attr.put(IMarker.SEVERITY, sev);
 					if ( file == null && !fileName.equals("")){
 						IResource fl = wk.findMember(new Path(fileName.replace(wk.getLocation().toString(), "")));
 						if ( fl != null ) {
 							if ( !files.contains(fileName) ){
 								files.add(fileName);
 								try {
 									fl.deleteMarkers(COMPILE_PROBLEM_MARKER, true, IResource.DEPTH_ZERO);
 								} catch (CoreException e1) {
 									e1.printStackTrace();
 								}											
 							}
 							try {
 								MarkerUtilities.createMarker(fl, attr, COMPILE_PROBLEM_MARKER);
 							} catch (CoreException e) {
 								System.out.println(e);
 							}							
  						}
 					} else if (file.getLocation().toString().replace("\\", "/").equals(fileName)
 							|| file.getName().equals(buffer)) {						
  						try {
  							MarkerUtilities.createMarker(file, attr, COMPILE_PROBLEM_MARKER);
  						} catch (CoreException e) {
  							System.out.println(e);
  						}
  					} else {
 						System.out.println(file.getLocation().toString());
  					}
					
				}
			}
			
		}
	}

}
