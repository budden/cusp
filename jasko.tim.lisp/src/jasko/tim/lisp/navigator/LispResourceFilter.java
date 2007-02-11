package jasko.tim.lisp.navigator;

import org.eclipse.core.resources.*;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.views.navigator.ResourcePatternFilter;


public class LispResourceFilter extends ResourcePatternFilter {
	
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if (element instanceof IFile) {
			IFile file = (IFile) element;
			
			String fileName = file.getName();
		
			if (fileName.equals(".project")
					|| fileName.equals(".DS_Store")
					|| fileName.endsWith(".fas")
					|| fileName.endsWith(".fasl")
					|| fileName.endsWith("~")
					|| fileName.endsWith(".lib")) {
				return false;
			}
		}
		
		return true;
	}

}
