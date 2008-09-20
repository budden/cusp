package jasko.tim.lisp.editors.assist;

import jasko.tim.lisp.editors.*;

import org.eclipse.jface.text.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * This class exists for the purposes of rendering that yellow information box we 
 *  pop up for macro expansions and similar fun.
 * @author Tim Jasko
 *
 */
public class LispInformationControlManager extends AbstractInformationControlManager {
	LispEditor editor;

	public LispInformationControlManager(LispEditor editor) {
		super(new LispInformationControlCreator());
		this.takesFocusWhenVisible(true);
		
		this.editor = editor;
		this.setCloser(new InfoControlCloser());
	}
	
	private String text;
	public void setText(String txt) {
		text = txt;
	}
	
	private Point location;
	public void setLocation(Point loc) {
		location = loc;
	}
	
	@Override
	protected void computeInformation() {
		
		setInformation(text, new Rectangle(location.x, location.y, 10, 10));

	}

	
	
	/**
	 * Closes the Tooltip when it loses focus,
	 *  and returns that focus back to the editor
	 * @author Tim Jasko
	 *
	 */
	public static class InfoControlCloser implements IInformationControlCloser,
		DisposeListener, FocusListener {
		
		IInformationControl control;
		Control parent;

		public void setSubjectControl(Control subject) {
			parent = subject;
		}

		public void setInformationControl(IInformationControl control) {
			this.control = control;
		}

		public void start(Rectangle subjectArea) {
			control.addDisposeListener(this);
			control.addFocusListener(this);
		}

		public void stop() {
		}
		
		public void widgetDisposed(DisposeEvent e) {
			parent.setFocus();
		}

		public void focusGained(FocusEvent e) {
		}

		public void focusLost(FocusEvent e) {
			control.dispose();
		}
	}
	
	
	/**
	 * Creates the InformationControl.
	 *  I don't know why the interface calls for this class, but it does.
	 * @author Tim Jasko
	 *
	 */
	public static class LispInformationControlCreator implements IInformationControlCreator {
		public IInformationControl createInformationControl(Shell parent) {
			return new DefaultInformationControl(parent);
		}

	}
}
