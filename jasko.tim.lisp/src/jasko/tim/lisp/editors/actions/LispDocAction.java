package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.editors.LispEditor;


public class LispDocAction extends CallUrlAction {
	
	public LispDocAction() {
		super(null, "http://www.lispdoc.com/?q=%s&search=Basic+search");
	}
	
	public LispDocAction(LispEditor editor) {
		super(editor, "http://www.lispdoc.com/?q=%s&search=Basic+search");
	}

}
