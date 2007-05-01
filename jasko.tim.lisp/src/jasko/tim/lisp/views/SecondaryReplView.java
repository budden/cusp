package jasko.tim.lisp.views;


import jasko.tim.lisp.swank.SwankInterface;

public class SecondaryReplView extends ReplView {
	public static final String ID = "jasko.tim.lisp.views.SecondaryReplView";

	protected SwankInterface getSwank() {
		return new SwankInterface();
	}
	
	public void dispose() {
		swank.disconnect();
		super.dispose();
	}
}
