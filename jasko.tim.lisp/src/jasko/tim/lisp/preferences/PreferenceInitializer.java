package jasko.tim.lisp.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import jasko.tim.lisp.LispPlugin;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
	 */
	public void initializeDefaultPreferences() {
		IPreferenceStore store = LispPlugin.getDefault().getPreferenceStore();
		//String exe = LispPlugin.getDefault().getPluginPath() + "CLisp-win32/clisp.exe";
		store.setDefault(PreferenceConstants.LISP_EXE, "C:\\Program Files\\LispBox\\CLISP\\clisp-2.33\\full\\lisp.exe");
		store.setDefault(PreferenceConstants.SWANK_LOADER, "C:\\Program Files\\LispBox\\slime\\swank-loader.lisp");

		
		store.setDefault(PreferenceConstants.P_BOOLEAN, true);
		store.setDefault(PreferenceConstants.P_CHOICE, "choice2");
		store.setDefault(PreferenceConstants.P_STRING,
				"Default value");
	}

}
