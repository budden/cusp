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
import jasko.tim.lisp.editors.LispEditor;

import org.eclipse.ui.*;
import org.eclipse.ui.browser.*;

public class CallUrlAction extends LispAction {
	private String url;
	
	public CallUrlAction() {
	}
	
	public CallUrlAction(LispEditor editor, String url) {
		super(editor);
		this.url = url;
	}

	
	public void run() {
		String identifier = getSymbol();
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
}
