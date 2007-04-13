package jasko.tim.lisp.views;

import jasko.tim.lisp.*;
import jasko.tim.lisp.swank.*;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.part.ViewPart;

public class ThreadView extends ViewPart {
	public static final String ID = "jasko.tim.lisp.views.ThreadView";
	
	private Tree threadTree;

	public void createPartControl(Composite parent) {
		GridLayout layout = new GridLayout(1, false);
		layout.marginLeft = 1;
		layout.marginTop = 1;
		layout.marginRight = 1;
		layout.marginBottom = 1;
		parent.setLayout(layout);
		
		GridData gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.verticalAlignment = GridData.FILL;
 		gd.grabExcessHorizontalSpace = true;
 		gd.grabExcessVerticalSpace = true;
 		
 		threadTree = new Tree(parent, SWT.BORDER);
 		threadTree.setLayoutData(gd);
 		threadTree.setHeaderVisible(true);
 		
 		TreeColumn col1 = new TreeColumn(threadTree, SWT.LEFT);
 		col1.setText("Thread");
 		col1.setWidth(200);
 		TreeColumn col2 = new TreeColumn(threadTree, SWT.RIGHT);
 		col2.setText("Status");
 		col2.setWidth(200);
	}

	public void setFocus() {
		SwankInterface swank = LispPlugin.getDefault().getSwank();
		
		swank.sendListThreads(new SwankRunnable() {
			public void run() {
				threadTree.removeAll();
				LispNode threads = result.getf(":return").getf(":ok");
				for (LispNode thread : threads.params) {
					String name = thread.get(0).value;
					String status = thread.get(1).value;
					String num = thread.get(2).value;
					
					TreeItem item = new TreeItem(threadTree, 0);
					item.setText(new String[] {name, status});
					item.setData(num);
				}
			}
		});
	}

}
