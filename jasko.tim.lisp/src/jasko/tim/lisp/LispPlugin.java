package jasko.tim.lisp;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Properties;

import jasko.tim.lisp.preferences.PreferenceConstants;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.views.ReplView;

import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.plugin.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.osgi.framework.BundleContext;
import org.eclipse.jface.action.IStatusLineManager;

/**
 * The main plugin class to be used in the desktop.
 */
public class LispPlugin extends AbstractUIPlugin {
	
	private SwankInterface swank = null;
	
	//The shared instance.
	private static LispPlugin plugin;
    
    private static String CUSP_VERSION = "0.0.0"; //$NON-NLS-1$
    private static String RELEASE_DATE = "0000.00.00"; //$NON-NLS-1$
    
    private static String CONSOLE_NAME = "jasko.tim.lisp.console"; //$NON-NLS-1$

    // strings to store configurations
	public static final String ATTR_LISP_EXE = "jasko.tim.lisp" + ".ATTR_LISP_EXE";
	public static final String ATTR_LISP_FLAVOR = "jasko.tim.lisp" + ".ATTR_LISP_FLAVOR";
	public static final String ID_LAUNCH_CONFIGURATION_TYPE = "jasko.tim.lisp.launchType";	
	
	/**
	 * The constructor.
	 */
	public LispPlugin() {
		plugin = this;
	}

	/**
	 * @param msg prints message to Repl's status bar (if repl is available)
	 */
	public void welcomeMessage(String lispVersion, String pkg){
	    IWorkbench workbench= PlatformUI.getWorkbench();
	    IWorkbenchWindow window= workbench.getActiveWorkbenchWindow();
	    IWorkbenchPage activePage = window.getActivePage();
	    IStatusLineManager statusLineManager = null;
		  if (activePage != null) {
		   IWorkbenchPart replPart = activePage.findView(ReplView.ID);
		   if (replPart instanceof ReplView){
			   statusLineManager = 
					((IViewPart)replPart).getViewSite().getActionBars().getStatusLineManager();
			   statusLineManager.setMessage(makeStatusMsg(lispVersion,pkg));
			   ((ReplView)replPart).appendText("You are running "+lispVersion
					   +" via Cusp " 
	 				   + LispPlugin.getVersion());
		   }
		  }
	}
	
	static public String makeStatusMsg(String lispVersion, String pkg){
		   String statusMsg = "CL:"+lispVersion
			+"| Cusp: "+getVersion()
			+"| Current package: " + pkg;
		   return statusMsg;
	}
	
	/**
	 * @param msg prints message to Repl's status bar (if repl is available)
	 */
	public void updateReplStatusLine(String msg){
	    IWorkbench workbench= PlatformUI.getWorkbench();
	    IWorkbenchWindow window= workbench.getActiveWorkbenchWindow();
	    IWorkbenchPage activePage = window.getActivePage();
	    IStatusLineManager statusLineManager = null;
		  if (activePage != null) {
		   IWorkbenchPart replPart = activePage.findView(ReplView.ID);
		   if (replPart instanceof ReplView){
			   statusLineManager = 
					((IViewPart)replPart).getViewSite().getActionBars().getStatusLineManager();
			   statusLineManager.setMessage(msg);
		   }
		  }
	}

	/**
	 * This method is called upon plug-in activation
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		cm =  new ColorManager(this);
		
		try {
			Properties props = new Properties();
			
			InputStream in = LispPlugin.class.getResourceAsStream("/cusp.properties"); //$NON-NLS-1$
			props.load(in);
			in.close();
			
			CUSP_VERSION = props.getProperty("cusp.version"); //$NON-NLS-1$
			RELEASE_DATE = props.getProperty("cusp.release_date"); //$NON-NLS-1$

			startSwank(); //FIXME: do this with launcher rather on startup
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public boolean startSwank(){
		if( swank == null || !swank.isConnected() ){
			swank = new SwankInterface();			
		} else { //disconnect if already running and connect again.
			swank.reconnect();
		}
		return ( swank != null && swank.isConnected() );
	}
	
	public boolean startSwank(String flavor, String command){
		if( swank == null || !swank.isConnected() ){
			swank = new SwankInterface(flavor,command);			
		} else { //disconnect if already running and connect again.
			swank.reconnect();
		}
		return ( swank != null && swank.isConnected() );
	}
	
	public SwankInterface getSwank() {
		return swank;
	}
    
    public static String getVersion () {
        return CUSP_VERSION;
    }
    
    public static String getReleaseDate () {
        return RELEASE_DATE;
    }

	private MessageConsole getConsole() {
		ConsolePlugin plugin = ConsolePlugin.getDefault();
		IConsoleManager conMan = plugin.getConsoleManager();
		IConsole[] existing = conMan.getConsoles();
		for (int i = 0; i < existing.length; i++){
			if (CONSOLE_NAME.equals(existing[i].getName())){
				return (MessageConsole) existing[i];				
			}
		}
		//no console found, so create a new one
		MessageConsole myConsole = new MessageConsole(CONSOLE_NAME, null);
		conMan.addConsoles(new IConsole[]{myConsole});
		return myConsole;
	}
	
	/**
	 * Put string to console.
	 * @param str
	 */
	public void out(String str){
			MessageConsole myConsole = getConsole();
			MessageConsoleStream out = myConsole.newMessageStream();
			out.println(str);					
	}
	
	public void activateConsole(){
		getConsole().activate();
	}
	    
	/**
	 * This method is called when the plug-in is stopped
	 */
	public void stop(BundleContext context) throws Exception {
		cm.dispose();
		getSwank().disconnect();
		plugin = null;
		super.stop(context);
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
		return AbstractUIPlugin.imageDescriptorFromPlugin("jasko.tim.lisp", path); //$NON-NLS-1$
	}
	
	
	private ColorManager cm;
	public ColorManager getColorManager() {
		return cm;
	}
	
	public String getLibsPathRegisterCode(){
		String code = ""; //$NON-NLS-1$
		ArrayList<File> subdirs = getLibsPath();
		if(subdirs.size() > 0){
		//	code = "(mapcar #'com.gigamonkeys.asdf-extensions:register-source-directory '(\n"; // $NON-NLS-1$
			code = "(com.gigamonkeys.asdf-extensions:register-source-directories '(\n"; // $NON-NLS-1$
			for (int i = 0; i < subdirs.size(); i++) {
				File child = subdirs.get(i);
				String name = child.getAbsolutePath().replace("\\", "/"); //$NON-NLS-1$ //$NON-NLS-2$
				if (!name.endsWith("/")) { //$NON-NLS-1$
					name += "/"; //$NON-NLS-1$
				}
				code += "  \"" + name + "\"\n"; //$NON-NLS-1$ //$NON-NLS-2$
			}
			code += "))"; //$NON-NLS-1$
		}
		return code;
	}
	
	public ArrayList<File> getLibsPath(){
		String path = getPluginPath() + "libraries"; //$NON-NLS-1$
	
	    // This filter only returns directories of type jasko.tim.lisp.libs
	    FileFilter libPluginFilter = new FileFilter() {
	        public boolean accept(File file) {
	            return (file.isDirectory()
	            		&& file.toString().matches(".*jasko\\.tim\\.lisp\\.libs.*")); //$NON-NLS-1$
	        }
	    };
	
	    ArrayList<File> topLevelDirs = new ArrayList<File>();
	    topLevelDirs.add(new File(path));

		String sysdirs[] = 
			getPreferenceStore().getString(PreferenceConstants.SYSTEMS_PATH).split(";"); //$NON-NLS-1$
		
		for(String sysdir: sysdirs){
			if( sysdir != null && !sysdir.equals("")){ //$NON-NLS-1$
				topLevelDirs.add(new File(sysdir));			
			}
		}
	    
		File pluginsDir = (new File(LispPlugin.getDefault().getPluginPath())).getParentFile();
	    for( File dir : pluginsDir.listFiles(libPluginFilter)){
	    	topLevelDirs.add(new File(dir.getAbsolutePath()+"/libs")); //$NON-NLS-1$
	    }
		
	    // This filter only returns directories
	    FileFilter dirFilter = new FileFilter() {
	        public boolean accept(File file) {
	            return file.isDirectory();
	        }
	    };
	
		ArrayList<File> subdirs = new ArrayList<File>();
		for( File dir : topLevelDirs){
			if( dir.isDirectory() ){
				subdirs.add(dir);
				for( File subdir : dir.listFiles(dirFilter) ){
					if( subdir != null && !subdirs.contains(subdir) ){
						subdirs.add(subdir);			
					}
				}						
			}
		}
		if (subdirs.size() == 0) {
			// Either dir does not exist or is not a directory
			System.out.println("*libraries dir not found! " + path); //$NON-NLS-1$
		}
		return subdirs;
	}
	
	public String getPluginPath() {
		try {
			String path = FileLocator.resolve(Platform.getBundle("jasko.tim.lisp").getEntry("/") ).getFile(); //$NON-NLS-1$ //$NON-NLS-2$
			if (System.getProperty("os.name").toLowerCase().contains("windows")){ //$NON-NLS-1$ //$NON-NLS-2$
				if(path.matches("/\\w:/.*")){ //$NON-NLS-1$
					path = path.substring(1);
				}
			}
			return path;
		} catch (IOException e) {
			e.printStackTrace();
		}
		return ""; //$NON-NLS-1$
	}
}
