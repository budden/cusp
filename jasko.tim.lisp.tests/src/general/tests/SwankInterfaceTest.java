package general.tests;

import junit.framework.TestCase;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.*;



public class SwankInterfaceTest extends TestCase {

	private SwankInterface getSwank(){
		return LispPlugin.getDefault().getSwank();
	}
	
	public void testSendEvalAndGrab() {
		String res = getSwank().sendEvalAndGrab("(+ 2 3)",1000);
		assertTrue(res.equals("5"));
	}

}
