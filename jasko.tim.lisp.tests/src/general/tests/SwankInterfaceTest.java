package general.tests;

import junit.framework.TestCase;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.*;



public class SwankInterfaceTest extends TestCase {

	private SwankInterface getSwank(){
		return LispPlugin.getDefault().getSwank();
	}
	
	public void test_sendEvalAndGrab() {
		String res = getSwank().sendEvalAndGrab("(+ 2 3)",1000);
		assertTrue(res.equals("5"));
	}

	public void test_haveDefinitions() {
		 // SK: on my system (WinXP, sbcl), I don't have SBCL sources, so
		// res = false. It might be true with SBCL sources.
		boolean res = getSwank().haveDefinitions("*query-io*", ":wiz", 2000);
		assertTrue(!res);
	}
}
