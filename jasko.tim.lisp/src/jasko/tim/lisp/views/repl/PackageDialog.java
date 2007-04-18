package jasko.tim.lisp.views.repl;


import java.util.*;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.widgets.List;


public class PackageDialog extends Dialog {
	ArrayList<String> packages;
	String result = "";
	String currPackage;
	
	public PackageDialog(Shell parentShell, ArrayList<String> packages, String currPackage) {
		super(parentShell);
		this.currPackage = currPackage;
		this.packages = packages;
		Collections.sort(this.packages);
	}
	
	List lstEnums;
	
	public String getPackage() {
		return result;
	}
	
	protected Control createDialogArea(Composite parent) {
		Composite comp = (Composite)super.createDialogArea(parent);
	
		comp.setLayout(new GridLayout());
	
		GridData gridData;

		Group grpEnum = new Group(comp, SWT.SHADOW_ETCHED_IN);
		grpEnum.setText("Current Package: " + currPackage);
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		
		grpEnum.setLayout(layout);
		gridData = new GridData();
		gridData.grabExcessHorizontalSpace = true;
		gridData.horizontalAlignment = GridData.FILL;
		grpEnum.setLayoutData(gridData);
	
		lstEnums = new List(grpEnum, SWT.LEFT | SWT.BORDER | SWT.V_SCROLL | SWT.SINGLE);
		for(String p: packages) {
			lstEnums.add(p);
		}
		lstEnums.setSelection(new String[] { currPackage });
		gridData = new GridData();
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalAlignment = GridData.FILL;
		gridData.verticalAlignment = GridData.FILL;
		gridData.heightHint = 100;
		lstEnums.setLayoutData(gridData);
		lstEnums.addMouseListener(new MouseListener() {

			public void mouseDoubleClick(MouseEvent e) {
				okPressed();
			}

			public void mouseDown(MouseEvent e) {
			}
			public void mouseUp(MouseEvent e) {
			}
		});
		lstEnums.addKeyListener(new KeyListener() {

			private String acc = "";
			
			public void keyPressed(KeyEvent e) {
				System.out.println(e);

				System.out.println(new Character(e.character).toString());
				String[] items = lstEnums.getItems();
				for (int i=0; i<items.length; ++i) {
					String item = items[i];
					
					acc += new Character(e.character).toString().toUpperCase();
					if (item.startsWith(acc)) {
						lstEnums.select(i);
						lstEnums.showSelection();
						return;
					}
					acc = "";
				}
			}

			public void keyReleased(KeyEvent e) {
			}
			
		});
		
		Composite grpButtons = new Composite(grpEnum, SWT.SHADOW_NONE);
		GridLayout layButtons = new GridLayout();
		layButtons.numColumns = 2;
		grpButtons.setLayout(layButtons);
		gridData = new GridData();
		gridData.grabExcessHorizontalSpace = true;
		gridData.horizontalAlignment = GridData.FILL;
		grpButtons.setLayoutData(gridData);
		
		
		
		return comp;
	}
	
	protected void okPressed() {
		result = lstEnums.getSelection()[0];
		super.okPressed();
	}
	
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Change Package");
	}
}
