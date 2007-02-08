package jasko.tim.lisp.editors.actions;

import org.eclipse.ui.texteditor.*;




public class LispDocAction extends CallUrlAction {
	
	public LispDocAction() {
		super(null, "http://www.lispdoc.com/?q=%s&search=Basic+search");
	}
	
	public LispDocAction(AbstractTextEditor editor) {
		super(editor, "http://www.lispdoc.com/?q=%s&search=Basic+search");
	}

}
