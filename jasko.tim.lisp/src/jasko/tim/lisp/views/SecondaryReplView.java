package jasko.tim.lisp.views;


import jasko.tim.lisp.swank.SwankInterface;

public class SecondaryReplView extends ReplView {

	protected SwankInterface getSwank() {
		return new SwankInterface();
	}
	
	public void dispose() {
		swank.disconnect();
		super.dispose();
	}
}
