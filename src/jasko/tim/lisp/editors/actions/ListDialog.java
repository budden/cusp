package jasko.tim.lisp.editors.actions;

import java.util.ArrayList;


import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

public class ListDialog<T>  extends Dialog {
	//ArrayList<String> options;
	String result = "";
	int resultIndex = 0;
	
	ArrayList<String> options;
	ArrayList<T> data;
	ArrayList<String> tips = null;
	
	public ListDialog(Shell parentShell, ArrayList<String> options, ArrayList<T> data) {
		super(parentShell);
		this.options = new ArrayList<String>(options);
		this.data = data;
	}
	
	public ListDialog(Shell parentShell, ArrayList<String> options, ArrayList<T> data, ArrayList<String> tips) {
		super(parentShell);
		this.options = new ArrayList<String>(options);
		this.data = data;
		this.tips = tips;
	}
	
	public void setTitle(String title) {
		this.getShell().setText(title);
	}
	
	
	private List lstEnums;
	private Label lblTip;
	
	public String getResult() {
		return result;
	}
	
	public T getData() {
		return data.get(resultIndex);
	}
	
	public void setSelection(int index) {
		lstEnums.setSelection(index);
		if (tips != null) {
			lblTip.setText(tips.get(index));
		}
	}
	
	protected Control createDialogArea(Composite parent) {
		Composite comp = (Composite)super.createDialogArea(parent);
	
		comp.setLayout(new GridLayout());
	
		GridData gridData;

		Group grpEnum = new Group(comp, SWT.SHADOW_ETCHED_IN);
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		
		grpEnum.setLayout(layout);
		gridData = new GridData();
		gridData.grabExcessHorizontalSpace = true;
		gridData.horizontalAlignment = GridData.FILL;
		grpEnum.setLayoutData(gridData);
	
		lstEnums = new List(grpEnum, SWT.LEFT | SWT.BORDER | SWT.V_SCROLL | SWT.SINGLE);
		for(String p: options) {
			lstEnums.add(p);
		}
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
		
		if (tips != null) { 
			lblTip = new Label(grpEnum, SWT.NONE);
			lblTip.setText(tips.get(0));
			
			gridData = new GridData();
			gridData.grabExcessHorizontalSpace = true;
			gridData.grabExcessVerticalSpace = false;
			gridData.horizontalAlignment = GridData.FILL;
			lblTip.setLayoutData(gridData);
		}
		
		
		lstEnums.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				if (tips != null) {
					int index = lstEnums.getSelectionIndex();
					if (index >= 0) {
						lblTip.setText(tips.get(index));
					} else {
						lblTip.setText("");
					}
				}
			}
			
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		lstEnums.select(0);
		
		
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
		resultIndex = lstEnums.getSelectionIndex();
		super.okPressed();
	}
	
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Select one");
	}
}
