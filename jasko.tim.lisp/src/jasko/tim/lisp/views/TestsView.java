package jasko.tim.lisp.views;

import java.util.ArrayList;

import jasko.tim.lisp.LispImages;
import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.views.repl.PackageDialog;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.*;

public class TestsView extends ViewPart {
	public static final String ID = "jasko.tim.lisp.views.TestsView";

	private StyledText output;
	private Action runTestsButton;
	
	public static TestsView getTestsView() {
		IWorkbenchPage page = PlatformUI.getWorkbench()
		    .getActiveWorkbenchWindow().getActivePage();
		try {
			IViewPart view = page.showView(TestsView.ID);
			if (view != null && view instanceof TestsView) {
				TestsView me = (TestsView) view;
				return me;
			} else {
				return null;
			}
		} catch (PartInitException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public static void showTestsView(LispNode contents) {
		IWorkbenchPage page = PlatformUI.getWorkbench()
		    .getActiveWorkbenchWindow().getActivePage();
		try {
			IViewPart view = page.showView(TestsView.ID);
			if (view != null && view instanceof TestsView) {
				TestsView me = (TestsView) view;
				me.populate(contents);
			}
		} catch (PartInitException e) {
			e.printStackTrace();
		}
	}
	
	private void populate(LispNode contents){
		output.setRedraw(false);
		output.setText(/*contents.get(1).value*/contents.toString());
		output.setRedraw(true);		
	}

	protected void fillNormalToolBar() {
		if (runTestsButton != null) {
			IToolBarManager tbm = 
				this.getViewSite().getActionBars().getToolBarManager();
			
			tbm.removeAll();
			tbm.add(runTestsButton);
			tbm.update(true);
			SwankInterface swank = LispPlugin.getDefault().getSwank();
			if ( swank != null ){
			//	runTestsButton.setEnabled(swank.useUnitTest);
			// if repl is not first, then 
			}
		}
	}
	
	protected void fillToolBar(Composite parent) {
		
		runTestsButton = new Action("Run Tests") {
			public void run() {
				SwankInterface swank = LispPlugin.getDefault().getSwank();
				if( swank != null && swank.useUnitTest ){
					PackageDialog pd = 
						new PackageDialog(TestsView.this.getSite().getShell(),
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
		};
		runTestsButton.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.RUN_TESTS));
		runTestsButton.setToolTipText("Run tests");
		
		this.fillNormalToolBar();
	}

	@Override
	public void createPartControl(Composite parent) {
		GridLayout layout = new GridLayout(1, false);
		layout.marginLeft = 1;
		layout.marginTop = 1;
		layout.marginRight = 1;
		layout.marginBottom = 1;
		parent.setLayout(layout);
		
		GridData gd;
 		
 		output = new StyledText(parent, SWT.WRAP | SWT.V_SCROLL | SWT.BORDER | SWT.MULTI);
 		gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.verticalAlignment = GridData.FILL;
 		gd.grabExcessHorizontalSpace = true;
 		gd.grabExcessVerticalSpace = true;
 		output.setLayoutData(gd);
 		output.setEditable(false);

 		fillToolBar(parent);
	}

	@Override
	public void setFocus() {
	}

}
