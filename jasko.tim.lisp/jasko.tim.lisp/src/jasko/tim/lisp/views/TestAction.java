/**
 * 
 */
package jasko.tim.lisp.views;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.swank.SwankInterface;
import jasko.tim.lisp.views.repl.PackageDialog;

import java.util.ArrayList;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Shell;

/**
 * @author sk
 *
 */
public class TestAction extends Action {
	private Shell shell;
	public TestAction(Shell shell){
		super("Run Tests");
		this.shell = shell;
	}

	public void run() {
		SwankInterface swank = LispPlugin.getDefault().getSwank();
		String hasunit = swank.sendEvalAndGrab("(some #'(lambda (x) " +
				"(equal \"LISP-UNIT\" (package-name x))) (list-all-packages))", 1000);
		if( swank != null && "T".equalsIgnoreCase(hasunit) && swank.getUseUnitTest() ){
			PackageDialog pd = 
				new PackageDialog(shell,
						swank.getPackagesWithTests(1000), 
						swank.getlastTestPackage(),true);
			if (pd.open() == Dialog.OK) {
				swank.sendRunTests(pd.getPackage(), new TestsRunnable());
			}					
		} else {
			ArrayList<String> strings = new ArrayList<String>(2);
			strings.add("Cannot run tests,");
		}
	}
}
