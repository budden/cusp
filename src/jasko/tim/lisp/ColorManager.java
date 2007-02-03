package jasko.tim.lisp;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.Display;

public class ColorManager {
	
	public static RGB STRING = new RGB(200, 128, 0);
	public static RGB NUMBER = new RGB(0, 128, 128);
	public static RGB PAREN = new RGB(128, 0, 0);
	public static RGB KEYWORD = new RGB(0, 0, 128);
	public static RGB SYMBOL = new RGB(0, 0, 255);
	public static RGB PARAMS = new RGB(128, 128, 0);
	public static RGB COMMENT = new RGB(0, 128, 0);
	public static RGB DEFAULT = new RGB(0, 0, 128);
	public static RGB GLOBAL = new RGB(128, 0, 255);
	public static RGB CONSTANT = new RGB(128, 0, 128);
	public static RGB SENT_MESSAGE = new RGB(220,220,220);
	

	protected Map<RGB, Color> fColorTable = new HashMap<RGB, Color>(10);

	public void dispose() {
		Iterator e = fColorTable.values().iterator();
		while (e.hasNext())
			 ((Color) e.next()).dispose();
	}
	public Color getColor(RGB rgb) {
		Color color = (Color) fColorTable.get(rgb);
		if (color == null) {
			color = new Color(Display.getCurrent(), rgb);
			fColorTable.put(rgb, color);
		}
		return color;
	}
}
