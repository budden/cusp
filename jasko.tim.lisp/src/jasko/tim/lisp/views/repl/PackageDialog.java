package jasko.tim.lisp.views.repl;


import java.util.*;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.widgets.List;


public class PackageDialog extends Dialog implements KeyListener {
	ArrayList<String> packages;
	String result = "";
	String currPackage;
	String title = "";
	String groupTitle = "";

	private List lstEnums;
	private Label lblSearch;
	private String search = "";
	
	public PackageDialog(Shell parentShell, ArrayList<String> packages,
			ArrayList<String> infos, String currPackage, boolean loadDialog) {
		super(parentShell);
		this.currPackage = currPackage;
		this.packages = packages;
		if(loadDialog){
			title = "Load package";
			groupTitle = "Installed packages";
		} else {
			groupTitle = "Current Package: " + currPackage;
			title = "Change Package";
		}
		Collections.sort(this.packages);
	}
	
	public PackageDialog(Shell parentShell, ArrayList<String> packages, 
			String currPackage, boolean loadDialog) {
		super(parentShell);
		this.currPackage = currPackage;
		this.packages = packages;
		if(loadDialog){
			title = "Load package";
			groupTitle = "Installed packages";
		} else {
			groupTitle = "Current Package: " + currPackage;
			title = "Change Package";
		}
		Collections.sort(this.packages);
	}
	
	
	public String getPackage() {
		return result;
	}
	
	protected Control createDialogArea(Composite parent) {
		Composite comp = (Composite)super.createDialogArea(parent);
	
		
		comp.setLayout(new GridLayout());
	
		GridData gridData;

		Group grpEnum = new Group(comp, SWT.SHADOW_ETCHED_IN);
		grpEnum.setText(groupTitle);
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
		gridData.heightHint = 200;
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
		lstEnums.addKeyListener(this);
		
		lblSearch = new Label(grpEnum, SWT.SHADOW_IN);
		gridData = new GridData();
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = false;
		gridData.horizontalAlignment = GridData.FILL;
		lblSearch.setLayoutData(gridData);
		lblSearch.setVisible(false);
		
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
		newShell.setText(title);
	}
	
	private boolean isSearchable(char c) {
		if ("1234567890qwertyuiopasdfghjklzxcvbnm!@#$%^&*()_-=+{}|[]\\:;\"\'<>?,./`~"
				.indexOf(Character.toLowerCase(c)) >= 0) {
			return true;
		} else {
			return false;
		}
	}
	
	public void keyPressed(KeyEvent e) {
		if (e.keyCode == SWT.ESC) {
			if (search.equals("")) {
				this.cancelPressed();
				return;
			} else {
				search = "";
				lblSearch.setText(search);
				lblSearch.setVisible(false);
				return;
			}
		} else if (e.keyCode == SWT.TRAVERSE_RETURN) {
			this.okPressed();
			return;
		} else if (e.character == SWT.BS) {
			search = search.substring(0, search.length() - 1);
		} else if (isSearchable(e.character)) {
			search += Character.toLowerCase(e.character);
		} else {
			return;
		}
		lblSearch.setText(search);
		lblSearch.setVisible(true);
		for (String option: lstEnums.getItems()) {
			if (option.toLowerCase().startsWith(search)) {
				lstEnums.setSelection(new String[] { option });
				return;
			}
		}
		this.getShell().getDisplay().beep();
	}

	public void keyReleased(KeyEvent e) {
		
	}
}
