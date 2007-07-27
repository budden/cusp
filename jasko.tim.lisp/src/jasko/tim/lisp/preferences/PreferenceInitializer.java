package jasko.tim.lisp.preferences;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.ColorManager;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer implements PreferenceConstants {

    private static final HashMap<String, String> DEFAULT_COLOR_MAP = new HashMap<String, String>();
    
    static {
        DEFAULT_COLOR_MAP.put(COLOR_COMMENT, rgbAsPrefString(ColorManager.DEFAULT_COMMENT));
        DEFAULT_COLOR_MAP.put(COLOR_CONSTANT, rgbAsPrefString(ColorManager.DEFAULT_CONSTANT));
        DEFAULT_COLOR_MAP.put(COLOR_GLOBAL, rgbAsPrefString(ColorManager.DEFAULT_GLOBAL));
        DEFAULT_COLOR_MAP.put(COLOR_KEYWORD, rgbAsPrefString(ColorManager.DEFAULT_KEYWORD));
        DEFAULT_COLOR_MAP.put(COLOR_NUMBER, rgbAsPrefString(ColorManager.DEFAULT_NUMBER));
        DEFAULT_COLOR_MAP.put(COLOR_PARAMS, rgbAsPrefString(ColorManager.DEFAULT_PARAMS));
        DEFAULT_COLOR_MAP.put(COLOR_PAREN, rgbAsPrefString(ColorManager.DEFAULT_PAREN));
        DEFAULT_COLOR_MAP.put(COLOR_SENT_MESSAGE, rgbAsPrefString(ColorManager.DEFAULT_SENT_MESSAGE));
        DEFAULT_COLOR_MAP.put(COLOR_STRINGS, rgbAsPrefString(ColorManager.DEFAULT_STRING));
        DEFAULT_COLOR_MAP.put(COLOR_SYMBOL, rgbAsPrefString(ColorManager.DEFAULT_SYMBOL));
        DEFAULT_COLOR_MAP.put(COLOR_UCW, rgbAsPrefString(ColorManager.DEFAULT_UCW));
        
        DEFAULT_COLOR_MAP.put(REPL_INSPECTABLE_BG_COLOR, rgbAsPrefString(ColorManager.DEFAULT_REPL_INSPECTABLE_BG_COLOR));
        DEFAULT_COLOR_MAP.put(REPL_INSPECTABLE_FG_COLOR, rgbAsPrefString(ColorManager.DEFAULT_REPL_INSPECTABLE_FG_COLOR));
        DEFAULT_COLOR_MAP.put(REPL_INPUT_BG_COLOR, rgbAsPrefString(ColorManager.DEFAULT_REPL_INPUT_BG_COLOR));
        DEFAULT_COLOR_MAP.put(REPL_INPUT_FG_COLOR, rgbAsPrefString(ColorManager.DEFAULT_REPL_INPUT_FG_COLOR));
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
        
		store.setDefault(MANAGE_PACKAGES, true);
		store.setDefault(BUILD_TYPE, USE_ECLIPSE_BUILD);
		
		store.setDefault(SHOW_OUTLINE_HINT, true);
		
		store.setDefault(SHOW_EVAL_IN_REPL, false);
		store.setDefault(USE_CTRL_ENTER, false);
        store.setDefault(PAIR_EDIT_BRACKETS, false);
        store.setDefault(PAIR_SMART_BRACKETS, false);
        store.setDefault(PAIR_EDIT_BRACES, false);
        store.setDefault(PAIR_EDIT_QUOTES, false);
        store.setDefault(PAIR_EDIT_COMMENTS, false);
        store.setDefault(AUTO_POPUP_COMPLETIONS, true);
        store.setDefault(AUTO_POPUP_COMPLETIONS_DELAY, 700);
        store.setDefault(AUTO_INSERT_COMPLETIONS, false);
        store.setDefault(AUTO_FUZZY_COMPLETIONS, false);
        store.setDefault(DOCS_IN_COMPLETIONS, false);
        store.setDefault(MAX_HINT_LINES, 10);
        store.setDefault(ARGLIST_BELOW, false);
        
        store.setDefault(DECORATE_REPL_INSPECTABLES, true);
        store.setDefault(REPL_INSPECTABLE_UNDERLINE, true);
        store.setDefault(REPL_FONT_SIZE, 9);
        
        store.setDefault(OUTLINE_SORT, 0);
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
