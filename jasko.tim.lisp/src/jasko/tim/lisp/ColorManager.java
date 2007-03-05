package jasko.tim.lisp;

import jasko.tim.lisp.preferences.PreferenceConstants;

import java.awt.Event;
import java.util.Collection;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.Preferences.IPropertyChangeListener;
import org.eclipse.core.runtime.Preferences.PropertyChangeEvent;
import org.eclipse.jface.resource.DataFormatException;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.Display;

public class ColorManager {

	public static enum TokenType 
	  { STRING, NUMBER, PAREN, KEYWORD, SYMBOL, PARAMS,
		COMMENT, DEFAULT, GLOBAL, CONSTANT, SENT_MESSAGE };
	

	public static RGB DEFAULT_STRING = new RGB(200, 128, 0);
	public static RGB DEFAULT_NUMBER = new RGB(0, 128, 128);
	public static RGB DEFAULT_PAREN = new RGB(128, 0, 0);
	public static RGB DEFAULT_KEYWORD = new RGB(0, 0, 128);
	public static RGB DEFAULT_SYMBOL = new RGB(0, 0, 255);
	public static RGB DEFAULT_PARAMS = new RGB(128, 128, 0);
	public static RGB DEFAULT_COMMENT = new RGB(0, 128, 0);
	public static RGB DEFAULT_DEFAULT = new RGB(0, 0, 128);
	public static RGB DEFAULT_GLOBAL = new RGB(128, 0, 255);
	public static RGB DEFAULT_CONSTANT = new RGB(128, 0, 128);
	public static RGB DEFAULT_SENT_MESSAGE = new RGB(220,220,220);
	
	protected static Map<String, TokenType> prefTokenTypeMap;
	public static TokenType preferenceStringToTokenType(String str)
	{
		return getPrefTokenTypeMap().get(str);
	}
	
	protected static Map<String, TokenType> getPrefTokenTypeMap()
	{
		if (prefTokenTypeMap == null)
		{
			prefTokenTypeMap = new HashMap<String, TokenType>();
			prefTokenTypeMap.put(PreferenceConstants.COLOR_COMMENT,      TokenType.COMMENT);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_CONSTANT,     TokenType.CONSTANT);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_DEFAULT,      TokenType.DEFAULT);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_KEYWORD,      TokenType.KEYWORD);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_GLOBAL,       TokenType.GLOBAL);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_NUMBER,       TokenType.NUMBER);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_PAREN,        TokenType.PAREN);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_PARAMS,       TokenType.PARAMS);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_SENT_MESSAGE, TokenType.SENT_MESSAGE);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_STRINGS,       TokenType.STRING);
			prefTokenTypeMap.put(PreferenceConstants.COLOR_SYMBOL,       TokenType.SYMBOL);
		}
		return prefTokenTypeMap;
	}

	protected Map<RGB, Color> fColorTable = new HashMap<RGB, Color>(10);
	protected Map<TokenType, Color> tokenColorTable = new HashMap<TokenType, Color>(10);
	protected LispPlugin plugin;
	

	protected LispPlugin getPlugin() { return plugin; }
	
	protected IPropertyChangeListener colorPrefsChangeListener = new IPropertyChangeListener()
	{
		public void propertyChange(PropertyChangeEvent event) {
			//get the token type that corresponds to the modified property
			TokenType toktype = preferenceStringToTokenType(event.getProperty());
			//get the new color assigned to the token type
			String newval = (String)event.getNewValue();
			RGB rgb = StringConverter.asRGB(newval);
			Color color = setRGBForTokenType(toktype, rgb);
			//notify all listeners of the change
			for (ChangeEventListener listener : getChangeListeners())
				listener.colorPreferenceChanged(new ColorChangeEvent(null, toktype, color));
		}
	};
	
	protected Collection<ChangeEventListener> colorChangeListeners;
	protected Collection<ChangeEventListener> getChangeListeners() {
		return colorChangeListeners != null ? colorChangeListeners : new LinkedList<ChangeEventListener>();
	}
	public void addChangeEventListener(ChangeEventListener listener)
	{
		getChangeListeners().add(listener);
	}
	public void removeChangeEventListener(ChangeEventListener listener)
	{
		getChangeListeners().remove(listener);
	}
	
	public class ColorChangeEvent extends EventObject {
		protected Color newValue;
		protected Color oldValue;
		protected TokenType tokenType;
		public ColorChangeEvent(ColorManager source, TokenType type, Color newVal) {
			super(source);
			this.newValue = newVal;
			this.tokenType = type;
			this.oldValue = null;
		}
		/**
		 * 
		 */
		private static final long serialVersionUID = 6644641096793018547L;
	}
	
	public interface ChangeEventListener {
		public void colorPreferenceChanged(ColorChangeEvent event);
	}
	
	/**
	 * Loads the saved color preferences into the color manager.
	 *
	 */
	protected void loadColorPreferences()
	{
		Map<String, TokenType> prefMap = getPrefTokenTypeMap();
		try {
			for (String pref : prefMap.keySet())
			{
				TokenType toktype = preferenceStringToTokenType(pref);
				RGB prefVal;
				try {
					prefVal = StringConverter.asRGB(getPlugin().getPreferenceStore().getString(pref));
				} catch (DataFormatException e)
				{
					prefVal = DEFAULT_DEFAULT;
				}
				setRGBForTokenType(toktype, prefVal);
			}
		} 
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	
	public ColorManager(LispPlugin plugin)
	{
		this.plugin = plugin;
		beginObservingPreferences();
		loadColorPreferences();
	}

	/**
	 * Sets the color that 
	 * @param type token type to set the color for
	 * @param rgb rgb color of the token
	 */
	protected Color setRGBForTokenType(TokenType type, RGB rgb)
	{
		Color color = new Color(Display.getCurrent(), rgb);
		tokenColorTable.put(type, color);
		return color;
	}
	
	protected void beginObservingPreferences()
	{
		Preferences prefs = getPlugin().getPluginPreferences();
		prefs.addPropertyChangeListener(colorPrefsChangeListener);
	}
	protected void stopObservingPreferences()
	{
		getPlugin().getPluginPreferences().removePropertyChangeListener(colorPrefsChangeListener);
	}

	public void dispose() {
		Iterator e = fColorTable.values().iterator();
		
		while (e.hasNext())
			 ((Color) e.next()).dispose();
		
		stopObservingPreferences();
	}	

	/*
	public Color getColor(RGB rgb) {
		Color color = (Color) fColorTable.get(rgb);
		if (color == null) {
			color = new Color(Display.getCurrent(), rgb);
			fColorTable.put(rgb, color);
		}
		return color;
	}
	*/
	

	public Color getColor(TokenType type) {
		Color color = (Color) tokenColorTable.get(type);
		if (color == null) {
			
		}
		return color;
	}
	
	public Color getDefaultColor(RGB rgb)
	{
		return new Color(Display.getCurrent(), rgb);
	}
}
