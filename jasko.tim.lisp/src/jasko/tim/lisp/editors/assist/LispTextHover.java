package jasko.tim.lisp.editors.assist;

import jasko.tim.lisp.*;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.util.*;

import org.eclipse.jface.text.*;


/**
 * Pops up info about the symbol the mouse is hovering over.
 * @author Tim Jasko
 * @see ArglistAssistProcessor
 */
public class LispTextHover implements ITextHover, ITextHoverExtension {
	String prev = ")"; // impossible value
	String prevResult = "";
	LispEditor editor;
	
	public LispTextHover(LispEditor editor) {
		this.editor = editor;
	}
	
	public LispTextHover() {
	}

	public String getHoverInfo(ITextViewer textViewer, IRegion hoverRegion) {
		IDocument doc = textViewer.getDocument();
		String function = LispUtil.getCurrentFullWord(doc, hoverRegion.getOffset());
		if (function.equals("")) {
			return null;
		} else if (function.equals(prev)) {
			return prevResult;
		}
		SwankInterface swank = LispPlugin.getDefault().getSwank();
		String result = "";
		if (editor != null) {
			result = swank.getArglist(function,3000, editor.getPackage());
		} else {
			result = swank.getArglist(function,3000);
		}
		
		if (!result.contains("not available") && !result.equals("nil")) {
			String docString = swank.getDocumentation(function, 1000);
			if (!docString.equals("")) {
				result += "\n" + docString;
			}
			
			// Cache the last result, save some swanking
			prev = function;
			prevResult = result;
			
			return result;
		} else {
			return null;
		}
	}

	public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
		return new Region(offset, 0);
	}

	public IInformationControlCreator getHoverControlCreator() {
		return new LispTextHoverControlCreator();
	}

}
