package jasko.tim.lisp.navigator;


import org.eclipse.core.resources.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.views.navigator.*;



/**
 * Displays Resources in a Lispy way.
 *  Currently, that just means filtering out certain files.
 * @author Tim
 *
 */
public class LispNavigator extends ResourceNavigator implements IResourceChangeListener {
	public static final String ID = "jasko.tim.lisp.navigator.LispNavigator";
	
	
	public LispNavigator() {
		super();
		
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this,
				IResourceChangeEvent.POST_BUILD);
		
	}
	
	public void resourceChanged(IResourceChangeEvent event) {
		if (event.getType() == IResourceChangeEvent.POST_BUILD) {
			Display.getDefault().asyncExec( new Runnable() {
					public void run() {
						getViewer().refresh(true);
					}
			});
			
			
		} // if
	}
	
	protected void initFilters(TreeViewer viewer) {
		viewer.addFilter(new LispResourceFilter());
	}
	
	protected void initLabelProvider(TreeViewer viewer) {
		super.initLabelProvider(viewer);
		//viewer.setLabelProvider(WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider());
				/*new DecoratingLabelProvider(new WorkbenchLabelProvider(),
					   LispPlugin.getDefault().getWorkbench()
			         .getDecoratorManager().getLabelDecorator()));*/	
	}
}
