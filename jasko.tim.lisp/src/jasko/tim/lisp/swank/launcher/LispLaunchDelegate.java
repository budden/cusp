package jasko.tim.lisp.swank.launcher;

import jasko.tim.lisp.LispPlugin;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;

public class LispLaunchDelegate extends LaunchConfigurationDelegate {

	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		if( !LispPlugin.getDefault().startSwank() ){
			abort("Swank is not started", null);
		}
	}

	private void abort(String message, Throwable e) throws CoreException {
		throw new CoreException(new Status(IStatus.ERROR, 
				LispPlugin.getDefault().getDescriptor().getUniqueIdentifier(), 0, message, e));
	}
	
	
}
