package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.ILispEditor;
import jasko.tim.lisp.editors.LispPartitionScanner;
import jasko.tim.lisp.editors.autoedits.LispIndentOnEnter;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.*;
import org.eclipse.ui.IEditorPart;

/**
 * This is probably not the right place to do indentation, but it's a lot easier to do it here than in
 *  IAutoEditStrategy.
 * If somebody has a better understanding of these things, feel free to implement this in a more
 *  proper fashion.
 * @author Tim Jasko
 * @see jasko.tim.lisp.editors.autoedits.LispIndentOnEnter
 */
public class IndentAction extends LispAction {

    private ILispEditor editor;
	    
	public IndentAction() {
	}
	
    public IndentAction(ILispEditor editor) {
        this.editor = editor;
    }
	    
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
        editor = (ILispEditor)targetEditor;
    }    
	
	private static int getIndent(String str){
		int i = 0;
		while( i < str.length() && Character.isWhitespace(str.charAt(i)) ){
			++i;
		}
		return i;
	}

	public static int[] doIndent(int offset, int length, IDocument doc, int i0){
		int newOffset = offset;
		int newOffsetLength = length;
		int newi0Offset = i0;
		int i0length = i0 - offset;
		
		try {
			int firstLine = doc.getLineOfOffset(offset);
			int i0Line = doc.getLineOfOffset(i0);
			int lastLine = doc.getLineOfOffset(offset+length);
			// get first line indent0
			IRegion firstLineInfo = doc.getLineInformation(firstLine);
			int firstIndent = 
				getIndent(doc.get(firstLineInfo.getOffset(),
						firstLineInfo.getLength()));
			// get first line trimmed offset
			int firstTrimedOffset = 
				Math.max(0, offset - firstLineInfo.getOffset() - firstIndent);
			
			// get i0 line indent0
			IRegion i0LineInfo = doc.getLineInformation(i0Line);
			int i0Indent = 
				getIndent(doc.get(i0LineInfo.getOffset(),
						i0LineInfo.getLength()));
			// get last line trimmed position
			int i0TrimedOffset = Math.max(0, 
					offset + i0length - i0LineInfo.getOffset() - i0Indent);

			int newi0LineOffset = i0LineInfo.getOffset();
			
			// get last line indent0
			IRegion lastLineInfo = doc.getLineInformation(lastLine);
			int lastIndent = 
				getIndent(doc.get(lastLineInfo.getOffset(),
						lastLineInfo.getLength()));
			// get last line trimmed position
			int lastTrimedOffset = Math.max(0, 
					offset + length - lastLineInfo.getOffset() - lastIndent);

			int newLastLineOffset = lastLineInfo.getOffset();

			int firstIndentNew = 0;
			int lastIndentNew = 0;
			int i0IndentNew = 0;
			for (int funcLine = firstLine; funcLine <= lastLine; ++funcLine) {
				IRegion lineInfo = doc.getLineInformation(funcLine);
				
                // do not mess with indentation of multiline strings! 
                // todo -- it might be cleaner architecturally to have this check happen in LispUtil,
                //     but the code there is still somewhat opaque to me at the moment, and clobbering spacing in 
                //     multiline strings can be *really* bad, so let expediency overtake purity for the nonce.
                //     - Chas Emerick
                if (doc.getContentType(lineInfo.getOffset()).equals(LispPartitionScanner.LISP_STRING) &&
                        !doc.get(lineInfo.getOffset(), lineInfo.getLength()).trim().startsWith("\"")){
                	continue;                	
                }                
				
				String indent = LispIndentOnEnter.calculateIndent(lineInfo.getOffset(), doc);
				int indentOld = getIndent(doc.get(lineInfo.getOffset(),lineInfo.getLength()));
				if( funcLine == firstLine ){
					firstIndentNew = indent.length();
				}
				if( funcLine == lastLine ){
					lastIndentNew = indent.length();	
				} else {
					newLastLineOffset += indent.length() - indentOld;
				}
				if( funcLine < i0Line ){
					newi0LineOffset += indent.length() - indentOld;
				} else if (funcLine == i0Line ){
					i0IndentNew = indent.length();	
				}
				doc.replace(lineInfo.getOffset(), indentOld, indent);
			}
			newOffset = 
				firstLineInfo.getOffset() + firstIndentNew + firstTrimedOffset;
			newOffsetLength = 
				newLastLineOffset + lastIndentNew + lastTrimedOffset;
			newi0Offset = newi0LineOffset + i0IndentNew + i0TrimedOffset; 
			
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		
		return new int[] {newOffset, newOffsetLength, newi0Offset};
	}
	
	public void run() {
		ITextSelection ts = 
			(ITextSelection) editor.getSelectionProvider().getSelection();
		int newOffset[] = doIndent(ts.getOffset(),ts.getLength(),editor.getDocument(),ts.getOffset());
		editor.getSelectionProvider().setSelection(new TextSelection(newOffset[0],newOffset[1]-newOffset[0]));			
	}

}
