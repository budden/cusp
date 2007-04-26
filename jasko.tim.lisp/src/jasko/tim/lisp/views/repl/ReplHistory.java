package jasko.tim.lisp.views.repl;

import java.util.ArrayList;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.editors.LispConfiguration;
import jasko.tim.lisp.inspector.InspectorRunnable;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;

public class ReplHistory extends SourceViewer
	implements MouseMoveListener, DisposeListener, MouseListener {
	
	private ArrayList<Region> regions = new ArrayList<Region>();
	private ArrayList<String> ids = new ArrayList<String>();
	private Cursor hand;
	private Color inspectableBack;
	private Color inspectableFore;
	
	public ReplHistory(Composite parent) {
		super(parent, new VerticalRuler(10), SWT.V_SCROLL | SWT.MULTI | SWT.LEFT | SWT.BORDER);
		
		setEditable(false);
		configure(new LispConfiguration(null, LispPlugin.getDefault().getColorManager()));
 		IDocument doc = new Document();
 		ReplPartitionScanner.connectPartitioner(doc);
 		setDocument(doc);
 		
 		showAnnotations(false);
 		showAnnotationsOverview(false);
 		
 		getTextWidget().addMouseMoveListener(this);
 		getTextWidget().addMouseListener(this);
 		getTextWidget().addDisposeListener(this);
 		
 		hand = new Cursor(parent.getDisplay(), SWT.CURSOR_HAND);
 		
 		inspectableBack = new Color(parent.getDisplay(), new RGB(230, 230, 255));
 		inspectableFore = new Color(parent.getDisplay(), new RGB(0, 0, 128));
	}
	
	
	public void clear() {
		IDocument doc = getDocument();
		doc.set("");
		
		regions.clear();
		ids.clear();
	}
	
	public void appendText(String text) {
		IDocument doc = getDocument();
		try {
			doc.replace(doc.getLength(), 0, text);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}
	
	public void appendInspectable(String text, String id) {
		IDocument doc = getDocument();
		try {
			int start = doc.getLength();
			regions.add(new Region(start, start + text.length()));
			ids.add(id);
			doc.replace(start, 0, text);
			
			StyleRange style = new StyleRange();
			style.underline = true;
			style.start = start;
			style.length = text.length();
			style.background = inspectableBack;
			style.foreground = inspectableFore;
			getTextWidget().setStyleRange(style);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}
	
	private int getRegion(int offset) {
		for (int i=regions.size()-1; i>=0; --i) {
			Region r = regions.get(i);
			if (r.getLength() >= offset) {
				if (r.getOffset() <= offset) {
					return i;
				}
			} else {
				return -1;
			}
		}
		
		return -1;
	}

	public void mouseMove(MouseEvent e) {
		Point p = new Point(e.x, e.y);
		int offset = getTextWidget().getOffsetAtLocation(p);
		if (getRegion(offset)  != -1) {
			getTextWidget().setCursor(hand);
		} else {
			getTextWidget().setCursor(null);
		}
	}

	public void mouseUp(MouseEvent e) {
		Point p = new Point(e.x, e.y);
		int offset = getTextWidget().getOffsetAtLocation(p);
		int item = getRegion(offset);
		if (item  != -1) {
			String id = ids.get(item);
			sendInspect(id);
		}
	}
	
	protected void sendInspect(String id) {
		LispPlugin.getDefault().getSwank().sendInspectReplResult(id, new InspectorRunnable());
	}

	public void widgetDisposed(DisposeEvent e) {
		hand.dispose();
		inspectableFore.dispose();
		inspectableBack.dispose();
	}


	public void mouseDoubleClick(MouseEvent e) {
	}


	public void mouseDown(MouseEvent e) {
	}



}
