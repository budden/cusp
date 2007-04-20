package jasko.tim.lisp.preferences;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.ColorManager;;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

    private static final HashMap<String, String> DEFAULT_COLOR_MAP = new HashMap<String, String>();
    
    static {
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_COMMENT, rgbAsPrefString(ColorManager.DEFAULT_COMMENT));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_CONSTANT, rgbAsPrefString(ColorManager.DEFAULT_CONSTANT));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_GLOBAL, rgbAsPrefString(ColorManager.DEFAULT_GLOBAL));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_KEYWORD, rgbAsPrefString(ColorManager.DEFAULT_KEYWORD));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_NUMBER, rgbAsPrefString(ColorManager.DEFAULT_NUMBER));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_PARAMS, rgbAsPrefString(ColorManager.DEFAULT_PARAMS));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_PAREN, rgbAsPrefString(ColorManager.DEFAULT_PAREN));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_SENT_MESSAGE, rgbAsPrefString(ColorManager.DEFAULT_SENT_MESSAGE));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_STRINGS, rgbAsPrefString(ColorManager.DEFAULT_STRING));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_SYMBOL, rgbAsPrefString(ColorManager.DEFAULT_SYMBOL));
        DEFAULT_COLOR_MAP.put(PreferenceConstants.COLOR_UCW, rgbAsPrefString(ColorManager.DEFAULT_UCW));
    }
    
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
	 */
	public void initializeDefaultPreferences() {
		IPreferenceStore store = LispPlugin.getDefault().getPreferenceStore();
		for (Map.Entry e: DEFAULT_COLOR_MAP.entrySet()) {
			store.setDefault((String)e.getKey(), (String)e.getValue());
		}
	}

	/**
	 * Returns the default rgb preference string for the given color id.
	 * @param colorId - one of the color constants specified in {@link PreferenceConstants}.
	 */
	public static String getDefaultColorFor (String colorId) {
		return DEFAULT_COLOR_MAP.get(colorId);
	}

	
	protected static String rgbAsPrefString(RGB rgb) { 
		return "" + rgb.red + "," + rgb.green + "," + rgb.blue + "";
	}

}
