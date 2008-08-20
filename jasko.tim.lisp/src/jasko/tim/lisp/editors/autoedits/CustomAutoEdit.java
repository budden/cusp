package jasko.tim.lisp.editors.autoedits;

import java.util.HashMap;
import jasko.tim.lisp.preferences.PreferenceInitializer;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;

/* autoedit snippets, like (let) (progn) etc */
public class CustomAutoEdit implements IAutoEditStrategy {

	private static String CARET_CHAR = "|";
	private static int MAX_MATCH_LENGTH = 20;

	// ending character -> match keyword -> keywords
	private HashMap<String,HashMap<String,AutoEditData>> autoEditRegistry;
	
	private class AutoEditData{
		public String replace;
		public int caretMove;
		public int offset;

		public AutoEditData(String match, String replace1){
			offset = match.length();
			caretMove = replace1.indexOf(CARET_CHAR) - offset - 1;
			replace = replace1.replace(CARET_CHAR, "");
		}
	}

	private void initAutoEditRegistry(){
		String[] strs = PreferenceInitializer.getCustomAutoEditsPreference2();
		int n = strs.length/2;
		autoEditRegistry = new HashMap<String,HashMap<String,AutoEditData>>();
		for( int i = 0; i < n; ++i ){
			if( strs[2*i] != null && strs[2*i+1] != null && strs[2*i].length()>0
					&& strs[2*i+1].contains(CARET_CHAR)){
				String match = strs[2*i];
				AutoEditData ad = new AutoEditData(match,strs[2*i+1]);
				String ending = match.substring(match.length()- 1);
				if( autoEditRegistry.containsKey(ending) ){
					HashMap<String,AutoEditData> map = 
						autoEditRegistry.get(ending);
					map.put(match, ad);
				} else {
					HashMap<String,AutoEditData> map = 
						new HashMap<String,AutoEditData>();
					map.put(match, ad);
					autoEditRegistry.put(ending, map);
				}
			}
		}
	}
	
	public CustomAutoEdit(){
		initAutoEditRegistry();
	}
	
	private void cmdEnd(IDocument d, DocumentCommand c, AutoEditData data){
		try{
			d.replace(c.offset - data.offset, data.offset, data.replace);			
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		c.shiftsCaret = false;
		c.caretOffset = c.offset + data.caretMove;
		c.doit = false;
		return;
	}

	public void customizeDocumentCommand(IDocument d,
			DocumentCommand c) {
		if( autoEditRegistry.containsKey(c.text) ){ //candidate for autoedit
			// search if there is a valid autoedit
			HashMap<String,AutoEditData> map = 
				autoEditRegistry.get(c.text);
			int max_match_length = 
				MAX_MATCH_LENGTH - Math.max(0, MAX_MATCH_LENGTH - c.offset);
			try{
				String max_match_str = 
					d.get(c.offset - max_match_length, max_match_length);
				for(int i = 1; i < max_match_length; ++i ){
					String match_candidate = 
						max_match_str.substring(max_match_length-i);
					if( map.containsKey(match_candidate) ){
						cmdEnd(d,c,map.get(match_candidate));
						return;
					}
				}
			} catch (BadLocationException e) {
				e.printStackTrace();
			}
		}
	}

}
