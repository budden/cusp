package jasko.tim.lisp.swank;

import java.io.IOException;

public abstract class LispImplementation {
	protected String flispType = ""; //possible values SBCL, CLISP etc.
	protected boolean hasthreads = true; 
	
	// Probably lisp implementation specific - tested with SBCL
	protected String fatalError = "fatal error";
	public String fatalErrorString(){ return fatalError; }
	
	public String lispType(){ return flispType; }
	public boolean hasThreads(){ return hasthreads; }
	/**
	 * @return whether this instance is valid (ie can roll off a process) 
	 */
	public abstract boolean isValid();
	
	/**
	 * TODO: figure out this method's signature.. ie how to pass SBCL init args
	 * @param loadPath some lisp file to load
	 * @return running SBCL process
	 * @throws IOException 
	 */
	public abstract Process start(String loadPath, int swankPort) throws IOException;
	
	public String getQuitForm() { return "(quit)"; }
	
	/**
	 * This exists to help work around SBCL's windows problem.
	 *  If we find ourselves unable to connect to Slime, and we see that the process has terminated on us,
	 *   call this function, which should have a somewhat better shot at starting.
	 * @param loadPath
	 * @return
	 * @throws IOException
	 */
	public Process startHarder(String loadPath, int swankPort) throws IOException {
		return start(loadPath, swankPort);
	}

	/**
	 * This returns lisp commands neccessary to execute on the console to
	 * load swank file.
	 * 
	 * @return
	 */
	public abstract String getLoadSwankCommand();
	
	/**
	 * This performs any translations necessary on a local file name / directory name
	 * to feed it into the implementation.
	 * 
	 * @param filePath
	 * @return
	 */
	public String translateLocalFilePath(String filePath) {
		return filePath;
	}
	
	/**
	 * This performs any translations necessary on a remote file name /  directory name
	 * received from the implementation.
	 * 
	 * @param filePath
	 * @return
	 */
	public String translateRemoteFilePath(String filePath) {
		return filePath;
	}
}
