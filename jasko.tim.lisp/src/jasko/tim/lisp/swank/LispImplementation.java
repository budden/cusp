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
	
	

}
