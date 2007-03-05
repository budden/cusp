package jasko.tim.lisp.swank;

import jasko.tim.lisp.LispPlugin;

import java.io.File;
import java.io.IOException;

/**
 * This is a class for finding and starting Steel Bank Common Lisp.
 * Static methods are provided for finding an SBCL implementation on the system.
 * 
 * An instance of this class can be used for spawning SBCL processes and starting
 * them up.
 *  
 * @author Red Daly
 */
public class SBCLImplementation extends LispImplementation {
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
		if (implementafoltionFolder != null)
		{
			String os = System.getProperty("os.name").toLowerCase();
			String execName;
			if (os.contains("windows"))
				execName = "sbcl.exe";
			else
				execName = "sbcl";
			File possibleExecutable = new File(implementafoltionFolder.getPath() + File.separator + execName);
			
			if (possibleExecutable.exists())
				return possibleExecutable;
			else
				return null;
		}
		else
		{
			//TODO: just default to whatever happens when you type `sbcl' into bash
			return null;
		}
	}
	
	/**
	 * 
	 * @return the SBCL directory on the host computer
	 */
	static protected File findFolder()
	{
		String pluginDir = LispPlugin.getDefault().getPluginPath();
		File sbclFolder = new File(pluginDir + "sbcl/");
		if (sbclFolder.exists())
			return sbclFolder;
		else
			return null;
	}
	
	/**
	 * Attempts to find and return an SBCL implementation on the system.
	 *  
	 * @return SBCL implementation found on the system, or null if none is found
	 */
	static public SBCLImplementation findImplementation()
	{
		File executable = findExecutable();
		File dir = findFolder();
		if (executable != null && dir != null)
		{
			return new SBCLImplementation(executable, dir);
		}
		else
			return null;
	}

	File executable = null;
	File path = null;
	
	/**
	 * Constructs an instance of an SBCL implementation.  Does NOT start an SBCL process,
	 * or find an SBCL implementation on the host machine.
	 * 
	 * @see start()
	 * @see 
	 * @param executable
	 * @param sbclDirectory
	 */
	public SBCLImplementation(File executable, File sbclDirectory)
	{
		this.executable = executable;
		this.path = sbclDirectory;
	}

	public boolean isValid() { return executable != null && path != null; }
	
	public Process start(String loadPath) throws IOException
	{
		if (isValid())
		{
			ProcessBuilder pb;
			String[] commandLine = new String[] {
					executable.getPath(),
					"--load", loadPath
			};
			
			pb = new ProcessBuilder(commandLine);
			pb.environment().put("SBCL_HOME", path.getPath());
			return pb.start();
		}
		return null;
	}
}
