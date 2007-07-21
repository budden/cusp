package jasko.tim.lisp.views.repl;

import java.util.ArrayList;

import jasko.tim.lisp.ColorManager;
import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.ColorManager.ChangeEventListener;
import jasko.tim.lisp.ColorManager.ColorChangeEvent;
import jasko.tim.lisp.editors.LispConfiguration;
import jasko.tim.lisp.inspector.InspectorRunnable;
import jasko.tim.lisp.preferences.PreferenceConstants;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.*;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;

public class ReplHistory extends SourceViewer
	implements MouseMoveListener, DisposeListener, MouseListener {
	
	private ArrayList<InspectableRegion> regions = new ArrayList<InspectableRegion>();
	private Cursor hand;
	private Color inspectableBack;
	private Color inspectableFore;
	private Color inputBack;
	private Color inputFore;
    
    private boolean applyInspectableStyles;
    private boolean underlineInspectables;
	
	public ReplHistory (Composite parent) {
		super(parent, new VerticalRuler(10), SWT.V_SCROLL | SWT.MULTI | SWT.LEFT | SWT.BORDER);
		
		setEditable(false);
		//configure(new LispConfiguration(null, LispPlugin.getDefault().getColorManager()));
 		IDocument doc = new Document();
 		//ReplPartitionScanner.connectPartitioner(doc);
 		setDocument(doc);
 		
 		showAnnotations(false);
 		showAnnotationsOverview(false);
 		
 		getTextWidget().addMouseMoveListener(this);
 		getTextWidget().addMouseListener(this);
 		getTextWidget().addDisposeListener(this);
 		
 		hand = new Cursor(parent.getDisplay(), SWT.CURSOR_HAND);
 		
 		inputBack = LispPlugin.getDefault().getColorManager().getColor(ColorManager.TokenType.REPL_INP_BG);
 		inputFore = LispPlugin.getDefault().getColorManager().getColor(ColorManager.TokenType.REPL_INP_FG);
        
 		inspectableBack = LispPlugin.getDefault().getColorManager().getColor(ColorManager.TokenType.REPL_INSP_BG);
 		inspectableFore = LispPlugin.getDefault().getColorManager().getColor(ColorManager.TokenType.REPL_INSP_FG);
        
        IPreferenceStore ps = LispPlugin.getDefault().getPreferenceStore();
        applyInspectableStyles = ps.getBoolean(PreferenceConstants.DECORATE_REPL_INSPECTABLES);
        underlineInspectables = ps.getBoolean(PreferenceConstants.REPL_INSPECTABLE_UNDERLINE);
        ps.addPropertyChangeListener(new IPropertyChangeListener () {
            public void propertyChange (PropertyChangeEvent event) {
                if (event.getProperty().equals(PreferenceConstants.DECORATE_REPL_INSPECTABLES)) {
                    applyInspectableStyles = (Boolean)event.getNewValue();
                } else if (event.getProperty().equals(PreferenceConstants.REPL_INSPECTABLE_UNDERLINE)) {
                    underlineInspectables = (Boolean)event.getNewValue();
                } else {
                    return;
                }
                
                // should refresh already-displayed inspectables, but not sure how to get the currently-applied
                // StyleRanges back to the "normal" syntax highlighted baseline so that the new styles could be applied
                // as necessary
            }
        });
        LispPlugin.getDefault().getColorManager().addChangeEventListener(new ChangeEventListener () {
            public void colorPreferenceChanged (ColorChangeEvent event) {
                if (event.tokenType.equals(ColorManager.TokenType.REPL_INSP_BG)) {
                    inspectableBack = event.newValue;
                } else if (event.tokenType.equals(ColorManager.TokenType.REPL_INSP_FG)) {
                    inspectableFore = event.newValue;
                } else if (event.tokenType.equals(ColorManager.TokenType.REPL_INP_FG)) {
                    inputFore = event.newValue;
                } else if (event.tokenType.equals(ColorManager.TokenType.REPL_INP_BG)) {
                    inputBack = event.newValue;
                } else {
                    return;
                }
                
                // should refresh already-displayed inspectables, but not sure how to get the currently-applied
                // StyleRanges back to the "normal" syntax highlighted baseline so that the new styles could be applied
                // as necessary
            }
        });
	}
	
	
	public void clear() {
		IDocument doc = getDocument();
		doc.set("");
		
		regions.clear();
	}
	
	public void appendText(String text) {
		IDocument doc = getDocument();
		try {
			doc.replace(doc.getLength(), 0, text);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}

	public void appendInput(String text){
		IDocument doc = getDocument();
		try {
			int start = doc.getLength();
			doc.replace(doc.getLength(), 0, text);
            applyInputStyle(start, text.length());
		} catch (BadLocationException e) {
			e.printStackTrace();
		}		
	}
	
    private void applyInputStyle (int start, int length) {
        StyleRange style = new StyleRange(start, length, inputFore, inputBack);
        getTextWidget().setStyleRange(style);
    }
    
	public void appendInspectable(String text, String id) {
		IDocument doc = getDocument();
		try {
			int start = doc.getLength();
			regions.add(new InspectableRegion(start, text.length(), id));
			doc.replace(start, 0, text);
			
            applyInspectableStyle(start, text.length());
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}
	
    private void applyInspectableStyle (int start, int length) {
        if (!applyInspectableStyles) return;
        
        StyleRange style = new StyleRange(start, length, inspectableFore, inspectableBack);
        style.underline = underlineInspectables;
        getTextWidget().setStyleRange(style);
    }
    
    private void applyInspectableHoverStyle (int start, int length) {
        // not currently used -- might be nice to allow users to define a :hover style
        if (!applyInspectableStyles) return;
        
        StyleRange style = new StyleRange(start, length, inspectableFore, inspectableBack);
        style.underline = underlineInspectables;
        getTextWidget().setStyleRange(style);
    }
	
	private InspectableRegion getRegion (int offset) {
		for (int i=regions.size()-1; i>=0; --i) {
            InspectableRegion r = regions.get(i);
			if (r.getOffset() + r.getLength() >= offset) {
				if (r.getOffset() <= offset) {
					return r;
				}
			} else {
				return null;
			}
		}
		
		return null;
	}
    
	public void mouseMove(MouseEvent e) {
		Point p = new Point(e.x, e.y);
        InspectableRegion region = null;
        try { 
            region = getRegion(getTextWidget().getOffsetAtLocation(p));
        } catch (IllegalArgumentException ex) {
            // no active region
        }
         
		if (region != null) {
			getTextWidget().setCursor(hand);
		} else {
			getTextWidget().setCursor(null);
		}
	}

	public void mouseUp(MouseEvent e) {
		Point p = new Point(e.x, e.y);
        int offset;
        try {
    		offset = getTextWidget().getOffsetAtLocation(p);
        } catch (IllegalArgumentException ex) {
            return;
        }
		InspectableRegion region = getRegion(offset);
		if (region != null) sendInspect(region.id);
	}
	
	protected void sendInspect(String id) {
		LispPlugin.getDefault().getSwank().sendInspectReplResult(id, new InspectorRunnable());
	}

	public void widgetDisposed(DisposeEvent e) {
		hand.dispose();
	}
    
	public void mouseDoubleClick(MouseEvent e) {}
	public void mouseDown(MouseEvent e) {}
    
    private class InspectableRegion extends Region {
        public final String id;
        
        public InspectableRegion(int offset, int length, String id) {
            super(offset, length);
            this.id = id;
        }
    }

}
