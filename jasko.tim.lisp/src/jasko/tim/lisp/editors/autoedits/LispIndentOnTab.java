package jasko.tim.lisp.editors.autoedits;

import jasko.tim.lisp.util.LispUtil;
import jasko.tim.lisp.editors.actions.IndentAction;

import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;

public class LispIndentOnTab implements IAutoEditStrategy {

	private void cmdEnd(DocumentCommand c, int offset){
		c.shiftsCaret = false;
		c.caretOffset = offset;
		c.doit = false;
		c.text = "";
		return;
	}
	
	public void customizeDocumentCommand(IDocument d,
			DocumentCommand c) {
		if( "\t".equals(c.text) ){
			int topLvlRange[] = LispUtil.getTopLevelRange(d, c.offset);
			if( topLvlRange != null ){
				int offsets[] = 
					IndentAction.doIndent(topLvlRange[0], topLvlRange[1], d,c.offset);
				cmdEnd(c,offsets[2]);				
			} else { // when offset is just next to top-level bracket want to indent
				topLvlRange = LispUtil.getTopLevelRange(d, c.offset+1);
				if( topLvlRange != null ){
					int offsets[] = 
						IndentAction.doIndent(topLvlRange[0], topLvlRange[1], d,c.offset);
					cmdEnd(c,offsets[2]);				
				}
			}
		}
		
	}

}
