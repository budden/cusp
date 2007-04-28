package jasko.tim.lisp.editors;

import jasko.tim.lisp.util.LispUtil;

import java.util.Arrays;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;

public class CurrentExpressionHighlightingListener implements KeyListener, MouseListener {
    private final Annotation highlightAnnotation = new Annotation("jasko.tim.lisp.editors.LispEditor.current-sexp",
    																    false, "");
    private int[] currentHighlightRange;
    
    private ISourceViewer installedOn;
    
    private void removeHighlight () {
        installedOn.getAnnotationModel().removeAnnotation(highlightAnnotation);
    }
    
    public void install (ISourceViewer viewer) {
        viewer.getTextWidget().addKeyListener(this);
        viewer.getTextWidget().addMouseListener(this);
        installedOn = viewer;
    }
    
    public void uninstall (ISourceViewer viewer) {
        viewer.getTextWidget().removeKeyListener(this);
        viewer.getTextWidget().removeMouseListener(this);
    }

    private void updateHighlighting () {
        ITextSelection ts = (ITextSelection)installedOn.getSelectionProvider().getSelection();
        try {
            int[] range = LispUtil.getCurrentExpressionRange(installedOn.getDocument(), ts.getOffset());
            try {
                if (range == null) {
                    removeHighlight();
                } else if (currentHighlightRange != null && Arrays.equals(currentHighlightRange, range)) {
                    // leave current highlight in place, still valid
                } else {
                    removeHighlight();
                    installedOn.getAnnotationModel().addAnnotation(highlightAnnotation, new Position(range[0], range[1]));
                }
            } finally {
                currentHighlightRange = range;
            }
        } catch (BadLocationException ex) {
            ex.printStackTrace();
        }
    }
    
    public void keyPressed (KeyEvent e) {// only need to update highlighting for key events that might move us into a different s-expression scope
        switch (e.character) {
            case '(':
            case ')':
            case '\b':
            case '\n':
            case '\r':
                updateHighlighting();
                return;
        }
        switch (e.keyCode) {
            case SWT.KEYPAD_CR:
            case SWT.ARROW_RIGHT:
            case SWT.ARROW_LEFT:
            case SWT.ARROW_DOWN:
            case SWT.ARROW_UP:
            case SWT.HOME:
            case SWT.END:
            case SWT.PAGE_DOWN:
            case SWT.PAGE_UP:
            case SWT.DEL:
                updateHighlighting();
        }
    }

    public void keyReleased (KeyEvent e) {
    }

    public void mouseDoubleClick (MouseEvent e) {
    }

    public void mouseDown (MouseEvent e) {
        updateHighlighting();
    }

    public void mouseUp (MouseEvent e) {
    }
    
}