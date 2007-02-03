package jasko.tim.lisp;

import java.io.IOException;

import jasko.tim.lisp.swank.*;

import org.eclipse.ui.plugin.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class LispPlugin extends AbstractUIPlugin {
	
	private SwankInterface swank; 

	//The shared instance.
	private static LispPlugin plugin;
	
	/**
	 * The constructor.
	 */
	public LispPlugin() {
		plugin = this;
	}

	/**
	 * This method is called upon plug-in activation
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		
		try {
			swank = new SwankInterface();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public SwankInterface getSwank() {
		return swank;
	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		cm.dispose();
		getSwank().disconnect();
		plugin = null;
	}

	/**
	 * Returns the shared instance.
	 */
	public static LispPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path.
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return AbstractUIPlugin.imageDescriptorFromPlugin("jasko.tim.lisp", path);
	}
	
	
	private ColorManager cm = new ColorManager();
	public ColorManager getColorManager() {
		return cm;
	}
	
	public String getPluginPath() {
		try {
			return FileLocator.resolve(Platform.getBundle("jasko.tim.lisp").getEntry("/") ).getFile();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return "";
	}
	
}
