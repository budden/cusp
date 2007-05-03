package jasko.tim.lisp.editors.assist;

import jasko.tim.lisp.swank.LispNode;
import jasko.tim.lisp.swank.LispParser;

import org.eclipse.jface.text.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * This class was subclassed in order to enable us to format the text displayed in 
 *  our function info boxes. Currently, we just bold the function parameter info
 *  to distinguish it from the docstring.
 * @author Tim Jasko
 *
 */
public class LispTextHoverControlCreator implements IInformationControlCreator {

	public IInformationControl createInformationControl(Shell parent) {
		return new DefaultInformationControl(parent, new InfoPresenter());
	}
	
	
	public class InfoPresenter implements DefaultInformationControl.IInformationPresenter {
		public String updatePresentation(Display display, String hoverInfo, TextPresentation pres, int maxWidth, int maxHeight) {
			/*int endOfLine = hoverInfo.indexOf('\n');
			if (endOfLine < 0) {
				endOfLine = hoverInfo.length();
			}
			pres.addStyleRange(new StyleRange(0, endOfLine, null, null, SWT.BOLD));*/
			LispNode stuff = LispParser.parse(hoverInfo);
			pres.addStyleRange(new StyleRange(0, stuff.get(0).endOffset, null, null, SWT.BOLD));
			return hoverInfo;
		}
		
	}

}
