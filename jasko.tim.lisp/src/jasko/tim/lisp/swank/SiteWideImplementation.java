package jasko.tim.lisp.swank;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.preferences.PreferenceConstants;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import org.eclipse.jface.preference.IPreferenceStore;

/**
 * This is a very simple class that starts the lisp that the user
 * chose in the preferences
 *  
 * @author Marc Halbruegge
 */
public class SiteWideImplementation extends LispImplementation {
	
	private File executable = null;
	private String loadPath = null;

	/**
	 * @return Lisp implementation found in the preferences, or null if none is found
	 */
	static public SiteWideImplementation findImplementation() {
		IPreferenceStore prefStore = LispPlugin.getDefault().getPreferenceStore();
		String executable = prefStore.getString(PreferenceConstants.LISP_EXE);
		return new SiteWideImplementation(executable);
	}

	
	/**
	 * Constructs an instance of an Lisp implementation.  Does NOT start an Lisp process,
	 * or find an Lisp implementation on the host machine.
	 * 
	 * @see start()
	 * @param executable full path read from the preferences
	 */
	public SiteWideImplementation(String executable) {
		this.executable = new File(executable);
	}

	public boolean isValid() { 
		return executable != null; 
	}
	
	public Process start(String loadPath, int port) throws IOException {
		if (isValid()) {
			if (!executable.exists()) {
				throw new IllegalArgumentException("Executable file does not exist :'" + executable + "'");
			}
			ProcessBuilder pb;
			String[] commandLine = new String[] {
				executable.getPath(),
				//"--load", loadPath
			};
			
			pb = new ProcessBuilder(commandLine);
			this.loadPath = loadPath;
			return pb.start();
		}
		return null;
	}
	
	public String getLoadSwankCommand() {
		return "(load \"" + this.loadPath.replace("\\", "\\\\") + "\")\n";
	}
}
