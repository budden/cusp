package jasko.tim.lisp.views;

import jasko.tim.lisp.*;
import jasko.tim.lisp.util.*;
import jasko.tim.lisp.views.repl.*;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.editors.*;

import java.util.*;

import org.eclipse.swt.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.ui.part.ViewPart;


public class ReplView extends ViewPart {
	
	protected ArrayList<String> prevCommands = new ArrayList<String>();

	public static final String ID = "jasko.tim.lisp.views.ReplView";
	protected enum State {
		Eval,
		ReadString,
		Debug
	}
	
	protected State state = State.Eval;
	
	/**
	 * Used when the Repl is returning input from ReadString mode.
	 */
	String stringNum1;
	String stringNum2;
	
	/**
	 * Tells us what options we actually have in Debug mode; prevents bad debug answers
	 */
	int numDebugOptions;

	protected SwankInterface swank;
	
	protected Composite parentControl;
	protected Sash replComposite;
	protected SourceViewer history;
	protected SourceViewer in;
	
	protected Button btn;
	
	public void setFocus() {
		in.getControl().setFocus();
	}
	
	protected SwankInterface getSwank() {
		return LispPlugin.getDefault().getSwank();
	}
	
	public void createPartControl(Composite parent) {
		parentControl = parent;
		swank = getSwank();
		
		if (swank == null) {
			// We weren't able to connect to Lisp. If you don't catch this,
			// Eclipse shows the backtrace where the Repl should be, and then people
			// email you asking what's going on. This is the much better solution.
			Label lbl = new Label(parent, SWT.BORDER);
			lbl.setText("Cusp was unable to connect to your lisp instance. Please try restarting Eclipse.");
			
			return;
		}

		GridLayout layout = new GridLayout(1, false);
		layout.marginLeft = 1;
		layout.marginTop = 1;
		layout.marginRight = 1;
		layout.marginBottom = 1;
		parent.setLayout(layout);
		
		GridData gd;
		gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.verticalAlignment = GridData.FILL;
 		gd.grabExcessHorizontalSpace = true;
 		gd.grabExcessVerticalSpace = true;
 		
 		
 		final Composite notButtons = new Composite(parent, SWT.NONE);
 		notButtons.setLayoutData(gd);
 		
 		final Sash sash = new Sash(notButtons, SWT.HORIZONTAL);
 		replComposite = sash;
 		
 		// Put a border around our text viewer
 		Composite comp = new Composite(notButtons, SWT.BORDER);
 		layout = new GridLayout(1, false);
		layout.marginLeft = 0;
		layout.marginTop = 0;
		layout.marginRight = 0;
		layout.marginBottom = 0;
		layout.horizontalSpacing = 0;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		comp.setLayout(layout);
 		//comp.setLayoutData(gd);
 		
 		Font baseFont = JFaceResources.getTextFont();
 		FontData fd = baseFont.getFontData()[0];
 		fd.setHeight(9);
 		Font newFont = new Font(Display.getDefault(), fd);
 		
 		history = new SourceViewer(comp, new VerticalRuler(10), SWT.V_SCROLL | SWT.MULTI | SWT.LEFT | SWT.BORDER);
 		history.setEditable(false);
 		//history.configure(new ReplConfiguration(LispPlugin.getDefault().getColorManager()));
 		history.configure(new LispConfiguration(null, LispPlugin.getDefault().getColorManager()));
 		IDocument doc = new Document();
 		ReplPartitionScanner.connectPartitioner(doc);
 		history.setDocument(doc);
 		
 		history.showAnnotations(false);
 		history.showAnnotationsOverview(false);
 		history.getControl().setLayoutData(gd);
 		history.getTextWidget().setFont(newFont);
		
 		Composite comp2 = new Composite(notButtons, SWT.BORDER);
 		comp2.setLayout(layout);
 		
 		in = new SourceViewer(comp2, new VerticalRuler(10), SWT.V_SCROLL | SWT.MULTI | SWT.LEFT | SWT.BORDER);
 		in.setEditable(true);
 		in.configure(new LispConfiguration(null, LispPlugin.getDefault().getColorManager()));
 		doc = new Document();
 		LispDocumentProvider.connectPartitioner(doc);
 		in.setDocument(doc);
 		in.showAnnotations(false);
 		in.showAnnotationsOverview(false);
 		in.getControl().setLayoutData(gd);
 		in.getTextWidget().setFont(newFont);
 		in.appendVerifyKeyListener(new PrevCommandsShortcuts());
 		/*in.addTextListener(new ITextListener() {
			public void textChanged(TextEvent event) {
				try {
				if (event != null) {
					System.out.println("*" + event.getText() + ":" + event.getText().length());
					if (event.getText().startsWith("\n") && LispUtil.doParensBalance(in.getDocument())) {
						eval();
					}
				}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});*/
		
		/*gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.heightHint = 50;
 		gd.grabExcessHorizontalSpace = true;
 		comp2.setLayoutData(gd);*/
 		
 		Composite buttonRow = new Composite(parent, SWT.NONE);
 		buttonRow.setLayout(new RowLayout(SWT.HORIZONTAL));

 		btn = new Button(buttonRow, SWT.PUSH);
 		btn.setText("Send");
 		btn.addSelectionListener( new OkListener());

 		Button prev = new Button(buttonRow, SWT.PUSH);
 		prev.setText("History");
 		prev.addSelectionListener( new PrevListener(parent));
 		
 		// layout stuff
 		
 		final FormLayout form = new FormLayout ();
 		notButtons.setLayout (form);
 		
 		FormData topData = new FormData ();
 		topData.left = new FormAttachment (0, 0);
 		topData.right = new FormAttachment (100, 0);
 		topData.top = new FormAttachment (0, 0);
 		topData.bottom = new FormAttachment (sash, 0);
 		comp.setLayoutData (topData);
 		
 		final int limit = 20, percent = 60;
 		final FormData sashData = new FormData ();
 		sashData.left = new FormAttachment (0, 0);
 		sashData.right = new FormAttachment (100, 0);
 		sashData.top = new FormAttachment (percent, 0);
 		sashData.bottom = new FormAttachment (percent, 5);
 		sash.setLayoutData (sashData);
 		// I have to handle this manually? Worst. Splitter. Ever.
 		sash.addListener (SWT.Selection, new Listener () {
 			public void handleEvent (Event e) {
 				Rectangle sashRect = sash.getBounds ();
 				Rectangle shellRect = notButtons.getClientArea ();
 				int top = shellRect.height - sashRect.height - limit;
 				e.y = Math.max (Math.min (e.y, top), limit);
 				if (e.y != sashRect.y)  {
 					sashData.top = new FormAttachment (0, e.y);
 					sashData.bottom = new FormAttachment (0, e.y + 5);
 					notButtons.layout ();
 				}
 			}
 		});
 		
 		FormData bottomData = new FormData ();
 		bottomData.left = new FormAttachment (0, 0);
 		bottomData.right = new FormAttachment (100, 0);
 		bottomData.top = new FormAttachment (sash, 0);
 		bottomData.bottom = new FormAttachment (100, 0);
 		comp2.setLayoutData (bottomData);
 		
 		parent.getShell().setDefaultButton(btn);
 		
 		registerSwankListeners();
 		
 		fillToolBar(parent);
 		fillMenu(parent);
 		
 		swank.sendEval("(format nil \"You are running ~a ~a\" (lisp-implementation-type) (lisp-implementation-version))\n", null);
	}
	
	protected void fillMenu(Composite parent) {
		IMenuManager menuMgr = this.getViewSite().getActionBars().getMenuManager();
		menuMgr.add(new Action("Increase Text Size") {
			public void run() {
				StyledText history = ReplView.this.history.getTextWidget();
				
				Font baseFont = history.getFont();
		 		FontData fd = baseFont.getFontData()[0];
		 		fd.setHeight(fd.getHeight() + 1);
		 		Font newFont = new Font(Display.getDefault(), fd);
		 		
		 		history.setFont(newFont);
		 		in.getTextWidget().setFont(newFont);
		 		
		 		baseFont.dispose();
			}
		});
		
		menuMgr.add(new Action("Decrease Text Size") {
			public void run() {
				StyledText history = ReplView.this.history.getTextWidget();
				
				Font baseFont = history.getFont();
		 		FontData fd = baseFont.getFontData()[0];
		 		fd.setHeight(fd.getHeight() - 1);
		 		Font newFont = new Font(Display.getDefault(), fd);
		 		
		 		history.setFont(newFont);
		 		in.getTextWidget().setFont(newFont);
		 		
		 		baseFont.dispose();
			}
		});
	}
	
	private Action connectButton;
	
	protected void fillToolBar(Composite parent) {
		IToolBarManager tbm = this.getViewSite().getActionBars().getToolBarManager();
		
		
		connectButton = new Action("Reconnect") {
			public void run() {
				if (MessageDialog.openQuestion(ReplView.this.getSite().getShell(),
						"Reconnect", "Are you sure you want to restart your Lisp session?")) {
					appendText("Reconnecting...");
					swank.reconnect();
					appendText("done.\n");
					scrollDown();
					
					this.setImageDescriptor(
							LispImages.getImageDescriptor(LispImages.RECONNECT));
				}
			}
		};
		connectButton.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.RECONNECT));
		connectButton.setToolTipText("Reconnect");
		
		Action packageButton = new Action("Change Package") {
			public void run() {
				swank.sendGetPackages(new SwankRunnable() {
					public void run() {
						LispNode packages = result.getf(":return").getf(":ok");
						PackageDialog pd = new PackageDialog(ReplView.this.getSite().getShell(),
								packages.params, swank.getPackage());
						if (pd.open() == Dialog.OK) {
							String p = pd.getPackage();
							swank.setPackage(p);
							appendText(";Package changed to " + p + "\n");
							scrollDown();
						}
					}
				});
			}
		};
		packageButton.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.DEFPACKAGE));
		packageButton.setToolTipText("Change Package");
		
		Action clearButton = new Action("Clear Console") {
			public void run() {
				IDocument doc = history.getDocument();
				doc.set("");
			}
		};
		clearButton.setImageDescriptor(LispImages.getImageDescriptor(LispImages.CLEAR));
		clearButton.setToolTipText("Clear Console");
		
		tbm.add(clearButton);
		tbm.add(packageButton);
		tbm.add(connectButton);
		
		
	}
	
	
	protected void appendText(String text) {
		IDocument doc = history.getDocument();
		try {
			doc.replace(doc.getLength(), 0, text);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}
	
	protected class DisplayRunnable extends SwankRunnable {
		public ReplView rv;
		
		public void run() {
			rv.appendText(resultString + "\n");
			scrollDown();
		}
		
		public SwankRunnable clone() {
			DisplayRunnable re = new DisplayRunnable();
			re.result = this.result;
			re.resultString = this.resultString;
			re.rv = this.rv;
			
			return re;
		}
	}
	
	protected void registerSwankListeners() {
		DisplayRunnable dr = new DisplayRunnable();
		dr.rv = this;
		swank.addDisplayCallback(dr);
		
		swank.addDisconnectCallback(new SwankRunnable() {
			public void run() {
				connectButton.setImageDescriptor(
						LispImages.getImageDescriptor(LispImages.DISCONNECTED));
			}
		});
		
		swank.addReadListener(new SwankRunnable() {
			public void run() {
				ReplView.this.stringNum1 = result.get(1).value;
				ReplView.this.stringNum2 = result.get(2).value;
				setState(State.ReadString);
				appendText(swank.fetchDisplayText() + "\n");
				scrollDown();
			}
		});
		
		swank.addDebugListener(new SwankRunnable() {
			public void run() {
				LispNode desc = result.get(3);
				appendText(desc.car().value + "\n" + desc.cadr().value + "\n");
				
				LispNode options = result.get(4);
				numDebugOptions = options.params.size(); 
				for (int i=0; i<options.params.size(); ++i) {
					LispNode option = options.get(i);
					appendText("\t" + i + ": [" + option.car().value + "] "
							+ option.cadr().value + "\n");
				} // for
				
				scrollDown();
				
				LispNode backtrace = result.get(5);
				appendText("BACKTRACE:\n");
				for (int i=0; i<backtrace.params.size(); ++i) {
					LispNode trace = backtrace.get(i);
					appendText("\t[" + trace.car().value + "] " + trace.cadr().value + "\n");
				} // for
				
				
				setState(State.Debug);
			} // run()
		});
		
	}
	
	protected void scrollDown() {
		history.revealRange(history.getDocument().getLength(), 0);
	}
	
	
	private Color backgroundColor;
	
	protected void setState(State state) {
		this.state = state;
		Display display = this.getSite().getShell().getDisplay();
		Color newColor = null;
		
		switch (state) {
		case Eval:
			in.getControl().setBackground(null);
			in.getControl().setForeground(null);
			replComposite.setBackground(null);
			break;
		case ReadString:
			newColor = new Color(display, 0, 255, 0);
			in.getControl().setBackground(newColor);
			in.getControl().setForeground(newColor);
			replComposite.setBackground(newColor);
			break;
		case Debug:
			newColor = new Color(display, 255, 0, 0);
			in.getControl().setBackground(newColor);
			in.getControl().setForeground(newColor);
			replComposite.setBackground(newColor);
			break;
		}
		if (backgroundColor != null) {
			backgroundColor.dispose();
		}
		backgroundColor = newColor;
	}
	
	protected class ReturnHandler extends SwankRunnable {
		public void run() {
			System.out.println(result);
			LispNode returns = result.getf(":return").getf(":ok").getf(":present");
			for (LispNode r: returns.params) {
				String res = r.get(0).value;
				appendText(res + "\n");
			}
			//String res = .get(0).get(0).value;
			
			scrollDown();
		}
	}
	
	protected void eval() {
		String cmd = in.getDocument().get();
		if (!cmd.endsWith("\n")) {
			cmd = cmd + "\n"; 
		}
		String cleanCmd = cmd.replace("\r", "");
		switch (state) {
		case Eval: {
			System.out.println(cleanCmd);
			prevCommands.add(cmd.trim());
			currPrevCommand = prevCommands.size();
			appendText(swank.getPackage() + "> " + cmd);
			scrollDown();
			
			swank.sendEval(cleanCmd, new ReturnHandler());
			break;
		}
		case Debug: {
			try {
				int choice = Integer.parseInt(cleanCmd.trim());
				if (choice >=0 && choice < numDebugOptions) {
					swank.sendDebug(cleanCmd, null);
					appendText("]> " + cmd);
					scrollDown();
					setState(State.Eval);
				} else {
					appendText("; You must choose a debug option between 0 and " + (numDebugOptions - 1) + "\n");
				}
			} catch (Exception e) {
				appendText("; You must choose a debug option between 0 and " + (numDebugOptions - 1) + "\n");
			}
			break;
		}
		case ReadString: {
			swank.sendReadString(cleanCmd, null, stringNum1, stringNum2);
			appendText(">> " + cmd);
			scrollDown();
			setState(State.Eval);
			break;
		}
		default:
			System.out.println("Repl is in a theoretically unreachable state");
		} // switch
		
		
		// When this was called from the TextChanged event, exceptions got thrown
		//  and the control became unresponsive. So, we execute elsewhere.
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				try {
					in.getTextWidget().setText("");
					in.getTextWidget().setFocus();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}
	
	protected class OkListener implements SelectionListener {

		public void widgetSelected(SelectionEvent e) {
			eval();
		}


		public void widgetDefaultSelected(SelectionEvent e) {
		}
		
	}
	
	int currPrevCommand=0;
	
	protected class PrevCommandsShortcuts implements VerifyKeyListener {
		public void verifyKey(VerifyEvent event) {
			//System.out.println("*" + event.keyCode);
			if ((event.stateMask & SWT.CTRL) == SWT.CTRL) {
				
				if (event.keyCode == 'p' || event.keyCode == 'P') {
					event.doit = false;
					--currPrevCommand;
					if (currPrevCommand < 0) {
						currPrevCommand = prevCommands.size() - 1;
					}
					if (currPrevCommand >=0 ) {
						in.getDocument().set(prevCommands.get(currPrevCommand));
						in.setSelectedRange(in.getDocument().getLength(), 0);
					}
				} else if (event.keyCode == 'n' || event.keyCode == 'N') {
					event.doit = false;
					++currPrevCommand;
					if (currPrevCommand >= prevCommands.size()) {
						currPrevCommand = 0;
					}
					if (prevCommands.size() > 0) {
						in.getDocument().set(prevCommands.get(currPrevCommand));
						in.setSelectedRange(in.getDocument().getLength(), 0);
					}
				}
			} else if ((event.keyCode == '\r' || event.keyCode == '\n')
					&& LispUtil.doParensBalance(in.getDocument())) {
				//System.out.println("*" + event.text + ":" + event.text.length());
				eval();
				event.doit = false;
			}
		}
	}
	
	protected class PrevListener implements SelectionListener {
		Control parent;
		
		public PrevListener(Control parent) {
			this.parent = parent;
		}

		public void widgetSelected(SelectionEvent e) {
			Menu mnu = new Menu(parent);
			
			for (String command: prevCommands) {
				MenuItem mnuCom = new MenuItem(mnu, SWT.PUSH);
				if (command.length() > 50) {
					mnuCom.setText(command.substring(0, 47) + "...");
				} else {
					mnuCom.setText(command);
				}
				
				mnuCom.addSelectionListener(new PrevCommandListener(command));
			}
			mnu.setVisible(true);
		}

		public void widgetDefaultSelected(SelectionEvent e) {
		}
	}
	
	protected class PrevCommandListener implements SelectionListener {
		String command;

		public PrevCommandListener(String command) {
			this.command = command;
		}
		
		public void widgetSelected(SelectionEvent e) {
			in.getDocument().set(command);
		}

		public void widgetDefaultSelected(SelectionEvent e) {
		}
	
	}
}
