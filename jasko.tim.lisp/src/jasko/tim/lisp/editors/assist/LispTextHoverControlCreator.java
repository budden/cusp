package jasko.tim.lisp.editors.assist;

import jasko.tim.lisp.swank.LispNode;
import jasko.tim.lisp.swank.LispParser;

import org.eclipse.jface.text.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;


import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.*;

/**
 * This class was subclassed in order to enable us to format the text displayed in 
 *  our function info boxes. Currently, we just bold the function parameter info
 *  to distinguish it from the docstring.
 * @author Tim Jasko
 *
 */
public class LispTextHoverControlCreator implements IInformationControlCreator {

    public IInformationControl createInformationControl(Shell parent){
    	ICommand fCommand = PlatformUI.getWorkbench().getCommandSupport()
        .getCommandManager().getCommand("jasko.tim.lisp.editors.actions.ContentAssistFocus");
    	if (fCommand != null && !fCommand.isDefined()){
            fCommand.getKeySequenceBindings();
        }
        return new DefaultInformationControl(parent, SWT.NONE,
        		new InfoPresenter(), "Press 'F2' for focus.");
    }

	public class InfoPresenter 
	  implements DefaultInformationControl.IInformationPresenter {
		public String updatePresentation(Display display, String hoverInfo, 
				TextPresentation pres, int maxWidth, int maxHeight) {
			if( hoverInfo.length() > 0 && hoverInfo.startsWith("(")){
				LispNode stuff = LispParser.parse(hoverInfo,true);
				pres.addStyleRange(new StyleRange(0, Math.min(stuff.endOffset+1,hoverInfo.length()), 
						null, null, SWT.BOLD));				
			}
			return hoverInfo;
		}
		
	}

}
