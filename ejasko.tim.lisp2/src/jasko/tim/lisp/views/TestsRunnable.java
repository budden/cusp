package jasko.tim.lisp.views;

import jasko.tim.lisp.swank.LispNode;
import jasko.tim.lisp.swank.SwankRunnable;

public class TestsRunnable extends SwankRunnable {

	public void run() {
		LispNode contents = result.getf(":return").getf(":ok");
		TestsView.showTestsView(contents);
	}

}
