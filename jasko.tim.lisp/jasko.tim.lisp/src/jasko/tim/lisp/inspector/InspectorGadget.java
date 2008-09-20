package jasko.tim.lisp.inspector;

import org.eclipse.swt.widgets.Composite;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.views.repl.ReplHistory;

/**
 * So named because it's funnier than InspectorWidget.
 */
public class InspectorGadget extends ReplHistory {

	public InspectorGadget(Composite parent) {
		super(parent);
	}
	
	protected void sendInspect(String id) {
		LispPlugin.getDefault().getSwank().sendInspectInspectedPart(id, new InspectorRunnable());
	}
	
}
