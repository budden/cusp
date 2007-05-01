package jasko.tim.lisp.swank;

import java.io.IOException;

public abstract class LispImplementation {
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
	public abstract Process start(String loadPath) throws IOException;
	
	public String getQuitForm() { return "(quit)"; }
	
	/**
	 * This exists to help work around SBCL's windows problem.
	 *  If we find ourselves unable to connect to Slime, and we see that the process has terminated on us,
	 *   call this function, which should have a somewhat better shot at starting.
	 * @param loadPath
	 * @return
	 * @throws IOException
	 */
	public Process startHarder(String loadPath) throws IOException {
		return start(loadPath);
	}

}
