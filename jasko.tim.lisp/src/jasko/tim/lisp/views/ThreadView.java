package jasko.tim.lisp.views;

import jasko.tim.lisp.*;
import jasko.tim.lisp.swank.*;

import org.eclipse.jface.action.*;
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
 		
 		threadTree = new Tree(parent, SWT.BORDER | SWT.FULL_SELECTION);
 		threadTree.setLayoutData(gd);
 		threadTree.setHeaderVisible(true);
 		
 		TreeColumn col0 = new TreeColumn(threadTree, SWT.LEFT);
 		col0.setText("Num");
 		col0.setWidth(50);
 		TreeColumn col1 = new TreeColumn(threadTree, SWT.LEFT);
 		col1.setText("Thread");
 		col1.setWidth(200);
 		TreeColumn col2 = new TreeColumn(threadTree, SWT.RIGHT);
 		col2.setText("Status");
 		col2.setWidth(200);
 		
 		fillToolBar(parent);
	}
	private Action killBtn;
	private Action debugBtn;
	private Action refreshBtn;
	
	private String getSelectedThread() {
		TreeItem[] sel = threadTree.getSelection();
		if (sel.length > 0) {
			if (sel[0].getData() != null) {
				return sel[0].getData().toString();
			}
		}
		return null;
	}
	
	protected void fillToolBar(Composite parent) {
		IToolBarManager tbm = this.getViewSite().getActionBars().getToolBarManager();
		
		debugBtn = new Action("Debug thread") {
			public void run() {
				String thread = getSelectedThread();
				if (thread != null) {
					SwankInterface swank = LispPlugin.getDefault().getSwank();
					swank.sendDebugThread(thread, new SwankRunnable() {
						public void run() {
							ReplView.switchToRepl();
						}
					});
				}
			}
		};
		debugBtn.setImageDescriptor(LispImages.getImageDescriptor(LispImages.THREAD_DEBUG));
		debugBtn.setToolTipText("Debug thread");
		
		tbm.add(debugBtn);
		
		
		killBtn = new Action("Kill thread") {
			public void run() {
				String thread = getSelectedThread();
				if (thread != null) {
					SwankInterface swank = LispPlugin.getDefault().getSwank();
					swank.sendKillThread(thread, new SwankRunnable() {
						public void run() {
							refreshThreads();
						}
					});
				}
			}
		};
		killBtn.setImageDescriptor(LispImages.getImageDescriptor(LispImages.THREAD_KILL));
		killBtn.setToolTipText("Kill thread");
		
		tbm.add(killBtn);
		
		
		refreshBtn = new Action("Refresh") {
			public void run() {
				refreshThreads();
			}
		};
		refreshBtn.setImageDescriptor(LispImages.getImageDescriptor(LispImages.REFRESH));
		refreshBtn.setToolTipText("Refresh");
		
		tbm.add(refreshBtn);
	}

	public void setFocus() {
		refreshThreads();
	}
	
	private void refreshThreads() {
		SwankInterface swank = LispPlugin.getDefault().getSwank();
		
		swank.sendListThreads(new SwankRunnable() {
			public void run() {
				threadTree.removeAll();
				LispNode threads = result.getf(":return").getf(":ok");
				for (int i=0; i < threads.params.size(); ++i) {
					LispNode thread = threads.get(i);
					
					String name = thread.get(0).value;
					String status = thread.get(1).value;
					String num = thread.get(2).value;
					
					TreeItem item = new TreeItem(threadTree, 0);
					item.setText(new String[] {num, name, status});
					item.setData(i);
				}
			}
		});
	}

}
