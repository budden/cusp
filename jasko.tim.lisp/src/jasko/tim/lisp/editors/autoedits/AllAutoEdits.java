package jasko.tim.lisp.editors.autoedits;

import org.eclipse.jface.text.IAutoEditStrategy;

public class AllAutoEdits {
	
	public static IAutoEditStrategy[] get(){
		return 	new IAutoEditStrategy[] {new LispIndentOnTab(),
				new LispIndentOnEnter(), new PairAutoEdit()};
	}
}
