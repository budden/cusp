package jasko.tim.lisp.swank;

import jasko.tim.lisp.LispPlugin;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;

/**
 * This is a class for finding and starting Allegro.
 * Static methods are provided for finding an Allegro implementation on the system.
 * 
 * An instance of this class can be used for spawning Allegro processes and starting
 * them up.
 *  
 * @author Red Daly
 */
public class AllegroImplementation extends LispImplementation {
	/**
	 * @return whether an SBCL implementation can be found on the host computer
	 */
	static public boolean existsOnSystem() {
		return findExecutable() != null;
	}
	
	/**
	 * 
	 * @return the executable SBCL file on the host computer
	 */
	static protected File findExecutable()
	{
		File implementafoltionFolder = findFolder();
		File exec = null;
		if (implementafoltionFolder != null)
		{
			String os = System.getProperty("os.name").toLowerCase();
			String execName;
			if (os.contains("windows"))
				execName = "alisp.exe";
			else
				execName = "alisp";
			File possibleExecutable = new File(implementafoltionFolder.getPath() + File.separator + execName);
			
			if (possibleExecutable.exists())
				exec = possibleExecutable;
			else
				exec = null;
		}
		else
		{
			//TODO: just default to whatever happens when you type `sbcl' into bash
			exec = null;
		}
		return exec;
	}
	
	/**
	 * Attempts to find the path of the implementation on this computer.
	 * @return the directory of the lisp implementation or null if none is found
	 */
	static protected File findFolder()
	{
		File foundPath = null;
		String pluginDir = LispPlugin.getDefault().getPluginPath();
		String lispPath = pluginDir + "acl80/";
		
		File dir = new File(lispPath);
		if (dir.exists())
			foundPath = dir;
		return foundPath;
	}
	
	/**
	 * Attempts to find and return an SBCL implementation on the system.
	 *  
	 * @return SBCL implementation found on the system, or null if none is found
	 */
	static public AllegroImplementation findImplementation()
	{
		File executable = findExecutable();
		File dir = findFolder();
		if (executable != null && dir != null)
		{
			return new AllegroImplementation(executable, dir);
		}
		else
			return null;
	}

	File executable = null;
	File path = null;
	
	/**
	 * Constructs an instance of an AllegroImplementation.  Does NOT start a process
	 * or find an SBCL implementation on the host machine.
	 * 
	 * @see start()
	 * @see 
	 * @param executable
	 * @param sbclDirectory
	 */
	public AllegroImplementation(File executable, File sbclDirectory)
	{
		this.executable = executable;
		this.path = sbclDirectory;
	}
	
	public boolean isValid() { return executable != null && path != null; }
	
	public Process start(String loadPath) throws IOException
	{
		if (isValid())
		{
			String[] commandLine = new String[] {
					executable.getPath()
			};

			ProcessBuilder pb = new ProcessBuilder(commandLine);
			return pb.start();
		}
		else
		{
			return null;
		}
	}
	public String getQuitForm() { return "(exit)"; }
}
