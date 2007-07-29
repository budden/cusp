package jasko.tim.lisp.views.repl;


import java.util.*;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.custom.*;


public class PackageDialog extends Dialog implements KeyListener {
	ArrayList<String> loadedPkgs;
	ArrayList<String> packages;
	private HashMap<String,String> infoMap = new HashMap<String,String>();
	String result = "";
	String currPackage;
	String title = "";
	String groupTitle = "";

	private List lstEnums;
	private Label lblSearch;
	private Label lblLoaded;
	private StyledText txtDoc;
	private String search = "";
	boolean loadDialog;
	
	//load package dialog
	public PackageDialog(Shell parentShell, ArrayList<String> loadedPkgs,
			ArrayList<String> packages,	ArrayList<String> infos,
			String currPackage) {
		super(parentShell);
		this.currPackage = currPackage;
		this.packages = packages;
		this.loadedPkgs = loadedPkgs;
		title = "Load package";
		groupTitle = "Installed packages";
		if( packages.size() == infos.size() ){
			for(int i = 0; i < packages.size(); ++i){
				infoMap.put(packages.get(i), infos.get(i));
			}
		} else {
			infoMap.clear();
		}
		loadDialog = true;
		Collections.sort(this.packages);
	}
	
	//change package dialog
	public PackageDialog(Shell parentShell, ArrayList<String> packages, 
			String currPackage) {
		super(parentShell);
		this.currPackage = currPackage;
		this.packages = packages;
		groupTitle = "Current Package: " + currPackage;
		title = "Change Package";
		loadDialog = false;
		Collections.sort(this.packages);
	}
	
	
	public String getPackage() {
		return result;
	}
	
	protected Control createDialogArea(Composite parent) {
		Composite comp = (Composite)super.createDialogArea(parent);
	
		GridLayout compLayout = new GridLayout();
		comp.setLayout(compLayout);
	
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
	
		lstEnums = new List(grpEnum, SWT.LEFT | SWT.BORDER 
				| SWT.V_SCROLL | SWT.SINGLE);
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
		lstEnums.addKeyListener(this);
		
		lblSearch = new Label(grpEnum, SWT.SHADOW_IN);
		gridData = new GridData();
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = false;
		gridData.horizontalAlignment = GridData.FILL;
		lblSearch.setLayoutData(gridData);
		lblSearch.setVisible(false);
		
		if( loadDialog ){
			lstEnums.addSelectionListener(new SelectionListener(){
				public void widgetDefaultSelected(SelectionEvent e){
					widgetSelected(e);
				};
				
				public void widgetSelected(SelectionEvent e){
					displayInfo();
				};
			});

			lblLoaded = new Label(grpEnum, SWT.SHADOW_IN);
			gridData = new GridData();
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = false;
			gridData.horizontalAlignment = GridData.FILL;
			lblLoaded.setLayoutData(gridData);
			lblLoaded.setVisible(false);
			
			txtDoc = new StyledText(grpEnum,
					SWT.BORDER | SWT.V_SCROLL );
			gridData = new GridData();
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = true;
			gridData.horizontalAlignment = GridData.FILL;
			gridData.verticalAlignment = GridData.FILL;
			gridData.heightHint = 70;
			gridData.widthHint = 400;
			txtDoc.setLayoutData(gridData);
			txtDoc.setEditable(false);
			txtDoc.setWordWrap(true);			
		}
		
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
		String pkg = lstEnums.getSelection()[0];
		if( loadDialog && loadedPkgs.contains(pkg.toUpperCase()) ){
			lblLoaded.setText("Package " + pkg 
					+ " is loaded. Press Esc to cancel.");
			lblLoaded.setVisible(true);
			return;
		} else {
			result = lstEnums.getSelection()[0];
			super.okPressed();			
		}
	}
	
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(title);
	}
	
	private void displayInfo(){
		int sel = lstEnums.getSelectionIndex(); 
		if( sel < 0 ){
			lblLoaded.setVisible(false);
			txtDoc.setText("");
			return;
		}
		if( infoMap.size() == packages.size() ){
			String pkg = packages.get(sel);
			String txt = infoMap.get(pkg);
			txtDoc.setText(txt);
			txtDoc.setToolTipText(txt);
			if( loadedPkgs.contains(pkg.toUpperCase()) ){
				lblLoaded.setText("Package "+pkg + " is loaded");
			} else {
				lblLoaded.setText("Package "+pkg + " is NOT loaded");
			}
			lblLoaded.setVisible(true);
		}
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
