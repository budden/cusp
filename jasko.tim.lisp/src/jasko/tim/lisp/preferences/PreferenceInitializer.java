package jasko.tim.lisp.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.ColorManager;;

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
		store.setDefault(PreferenceConstants.COLOR_COMMENT, rgbAsPrefString(ColorManager.DEFAULT_COMMENT));
		store.setDefault(PreferenceConstants.COLOR_CONSTANT, rgbAsPrefString(ColorManager.DEFAULT_CONSTANT));
		store.setDefault(PreferenceConstants.COLOR_GLOBAL, rgbAsPrefString(ColorManager.DEFAULT_GLOBAL));
		store.setDefault(PreferenceConstants.COLOR_KEYWORD, rgbAsPrefString(ColorManager.DEFAULT_KEYWORD));
		store.setDefault(PreferenceConstants.COLOR_NUMBER, rgbAsPrefString(ColorManager.DEFAULT_NUMBER));
		store.setDefault(PreferenceConstants.COLOR_PARAMS, rgbAsPrefString(ColorManager.DEFAULT_PARAMS));
		store.setDefault(PreferenceConstants.COLOR_PAREN, rgbAsPrefString(ColorManager.DEFAULT_PAREN));
		store.setDefault(PreferenceConstants.COLOR_SENT_MESSAGE, rgbAsPrefString(ColorManager.DEFAULT_SENT_MESSAGE));
		store.setDefault(PreferenceConstants.COLOR_STRINGS, rgbAsPrefString(ColorManager.DEFAULT_STRING));
		store.setDefault(PreferenceConstants.COLOR_SYMBOL, rgbAsPrefString(ColorManager.DEFAULT_SYMBOL));
	}
	
	
	protected static String rgbAsPrefString(RGB rgb) { 
		return "" + rgb.red + "," + rgb.green + "," + rgb.blue + "";
	}

}
