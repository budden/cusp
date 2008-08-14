package jasko.tim.lisp.swank;

import jasko.tim.lisp.LispPlugin;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;

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
			File possibleExecutable = new File(implementafoltionFolder.getPath() 
					+ File.separator + execName);
			
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
		if (sbclFolder.exists()){
			System.out.println(">>> Found default sbcl folder: "+sbclFolder.toString());
			return sbclFolder;
		} else {
			
			File pluginsFolder = new File(pluginDir).getParentFile();
			
			sbclFolder = new File(pluginsFolder.getPath() + "/sbcl/");
			
			if (sbclFolder.exists()){
				System.out.println(">>> Found next default sbcl folder: "
						+sbclFolder.toString());
				return sbclFolder;
			} else {
			    // This filter only returns directories that start with sbcl
			    FileFilter dirFilter = new FileFilter() {
			        public boolean accept(File file) {
			            return (file.isDirectory() 
			            		 && file.getName().startsWith("sbcl"));
			        }
			    };
				
			    ArrayList<File> sbclFolders = new ArrayList<File>();
			    
			    for( File dir : pluginsFolder.listFiles(dirFilter)){
			    	System.out.println(">>> Possible sbcl folder: "+dir.toString());
			    	sbclFolders.add(dir);
			    }
			    
			    if( sbclFolders.size() > 0 ){
				    Collections.sort(sbclFolders);
				    
					sbclFolder = new 
						File(sbclFolders.get(sbclFolders.size()-1).toString()+"/sbcl/");
					System.out.println(">>> Found feature sbcl folder: "
							+sbclFolder.toString());
					return sbclFolder;
			    } else {
					String dir = System.getenv("SBCL_HOME");
					if( dir != null ){
						sbclFolder = new File(dir);
						System.out.println(">>> Found SBCL_HOME sbcl folder: "
								+sbclFolder.toString());
						return sbclFolder;
					}		    	
			    }
			    
				System.out.println(">>> Did not find sbcl folder");
				return null;				
			}			
		}
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
	String loadPath = null;
	
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
		flispType = "SBCL";
		if (System.getProperty("os.name").toLowerCase().contains("windows")) {
			hasthreads = false;
		}
	}

	public boolean isValid() { return executable != null && path != null; }
	
	public Process start(String loadPath, int port) throws IOException
	{
		System.out.println("start");
		
		if (isValid())
		{
			ProcessBuilder pb;
			String[] commandLine = new String[] {
					executable.getPath()
					//,"--dynamic-space-size", "50000" // simulate the error some windows users get
					//"--load", loadPath
			};
			
			pb = new ProcessBuilder(commandLine);
			this.loadPath = loadPath;
			pb.environment().put("SBCL_HOME", path.getPath());
			return pb.start();
		}
		return null;
	}
	
	public Process startHarder(String loadPath) throws IOException {
		System.out.println("startHarder");
		ProcessBuilder pb;
		String[] commandLine = new String[] {
				executable.getPath(),
				"--dynamic-space-size", "100"
				//"--load", loadPath
		};
		
		pb = new ProcessBuilder(commandLine);
		this.loadPath = loadPath;
		pb.environment().put("SBCL_HOME", path.getPath());
		return pb.start();
	}
	
	public String getLoadSwankCommand() {
		return "(load \"" + this.loadPath.replace("\\", "\\\\") + "\")\n";
	}
}
