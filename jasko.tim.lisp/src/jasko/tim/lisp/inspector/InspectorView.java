package jasko.tim.lisp.inspector;

import java.util.*;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import jasko.tim.lisp.swank.*;

public class InspectorView extends ViewPart {
	public static final String ID = "jasko.tim.lisp.inspector.InspectorView";
	
	private InspectorGadget output;
	String title;
	String type;
	LispNode content;
	
	ArrayList<Integer> offsets = new ArrayList<Integer>();
	ArrayList<Control> controls = new ArrayList<Control>();
	

	
	public static void showInspector(String title, 
			String type, LispNode content) {
		IWorkbenchPage page = PlatformUI.getWorkbench()
		    .getActiveWorkbenchWindow().getActivePage();
		try {
			IViewPart view = page.showView(InspectorView.ID);
			if (view != null && view instanceof InspectorView) {
				InspectorView me = (InspectorView) view;
				me.populate(title, type, content);
			}
		} catch (PartInitException e) {
			e.printStackTrace();
		}
	}
	
	public void createPartControl(Composite comp) {
		
		GridData gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.verticalAlignment = GridData.FILL;
 		gd.grabExcessHorizontalSpace = true;
 		gd.grabExcessVerticalSpace = true;
 		
 		GridLayout layout = new GridLayout(1, false);
		layout.marginLeft = 1;
		layout.marginTop = 1;
		layout.marginRight = 1;
		layout.marginBottom = 1;
		
		comp.setLayout(layout);
		
		output = new InspectorGadget(comp);
		output.setEditable(false);
		output.getControl().setLayoutData(gd);
		
		output.appendText("You're not currently inspecting anything.\n");
		output.appendText("Click on underlined objects in the REPL to inspect them.\n");
	}
	
	
	public void populate(String title, String type, LispNode content) {
		output.clear();
		this.title = title;
		this.type = type;
		this.content = content;
		this.setPartName(title + " (" + type + ")");
		
		for (LispNode item : content.params) {
			if (item.isString) {
				output.appendTextForInspector(item.value);
			} else {
				output.appendInspectable(item.getf(":value").value, item.get(2).value);
			}
		}
	}


	public void setFocus() {
	}
	
	

}
