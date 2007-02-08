/**
 * Created on Mar 27, 2006
 * 
 * Paragent Confidential
 *
 * (C) Copyright Paragent, LLC Mar 27, 2006
 *
 *
 * The source code for this program is not published or otherwise divested
 * of its trade secrets, irrespective of what has been deposited with the
 * U.S. Copyright office.
 *
 * All Rights Reserved. Use, duplication or disclosure restricted unless
 * granted by formal written contract with Paragent, LLC.
 *
 * @author tjasko
 *
 */
package jasko.tim.lisp.editors.actions;

import java.net.MalformedURLException;
import java.net.URL;

import jasko.tim.lisp.*;
import jasko.tim.lisp.util.*;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;
import org.eclipse.ui.texteditor.*;
import org.eclipse.ui.browser.*;

public class CallUrlAction  extends Action implements IEditorActionDelegate {
	private AbstractTextEditor editor;
	private String url;
	
	public CallUrlAction() {
	}
	
	public CallUrlAction(AbstractTextEditor editor, String url) {
		this.editor = editor;
		this.url = url;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (AbstractTextEditor) targetEditor;
	}
	
	public void run() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		String identifier = LispUtil.getCurrentFullWord(
				editor.getDocumentProvider().getDocument(editor.getEditorInput()), offset);
		IWorkbenchBrowserSupport browser = LispPlugin.getDefault().getWorkbench().getBrowserSupport();
		try {
			browser.createBrowser("jasko.tim.lisp.lispdoc").openURL(new URL(
					url.replace("%s", identifier)));
		} catch (PartInitException e) {
			e.printStackTrace();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
		
	}

	public void run(IAction action) {
		run();
	}
	

	public void selectionChanged(IAction action, ISelection selection) {

	}
}
