package jasko.tim.lisp.swank;

public abstract class SwankDisplayRunnable extends SwankRunnable {

	protected String presentation = null;
	
	public void startPresentation(String presentation) {
		this.presentation = presentation;
	}
	
	public void endPresentation() {
		presentation = null;
	}
}
