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
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.ui.IKeyBindingService;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;


/**
 * @author Tim Jasko
 */

public class ReplView extends ViewPart {
	public static final String ID = "jasko.tim.lisp.views.ReplView";
	
	protected ArrayList<String> prevCommands = new ArrayList<String>();

	protected Stack<State> states = new Stack<State>();

	protected SwankInterface swank;
	
	protected Sash replComposite;
	protected SourceViewer history;
	protected SourceViewer in;
	
	protected Composite parentControl;
	protected Composite mainView;
	protected Composite debugView;
	protected Tree debugTree;
	protected Label debugLabel;
	
	protected Button btn;
	
	public static void switchToRepl() {
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		try {
			page.showView(ReplView.ID);
		} catch (PartInitException e) {
			e.printStackTrace();
		}
	}
	
	public void setFocus() {
		if (currState() instanceof DebugState) {
			debugTree.setFocus();
		} else {
			in.getControl().setFocus();
		}
	}
	
	protected SwankInterface getSwank() {
		return LispPlugin.getDefault().getSwank();
	}
	
	public void createPartControl(Composite parent) {
		parentControl = parent;
		parent.setLayout(new GridLayout(1, false));
		mainView = new Composite(parent, SWT.NONE);
		debugView = new Composite(parent, SWT.BORDER);
		
		hideFrame(debugView);
		showFrame(mainView);
		
		swank = getSwank();
		
		if (swank == null) {
			// We weren't able to connect to Lisp. If you don't catch this,
			// Eclipse shows the backtrace where the Repl should be, and then people
			// email you asking what's going on. This is the much better solution.
			Label lbl = new Label(parent, SWT.BORDER);
			lbl.setText("Cusp was unable to connect to your lisp instance. Please try restarting Eclipse.");
			
			return;
		}
		// layout controllers we need in a few places
		GridData gd;
		gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.verticalAlignment = GridData.FILL;
 		gd.grabExcessHorizontalSpace = true;
 		gd.grabExcessVerticalSpace = true;
 		
 		GridLayout layout = new GridLayout(1, false);
		layout.marginLeft = 1;
		layout.marginTop = 1;
		layout.marginRight = 1;
		layout.marginBottom = 1;
		
		// Create the debug view
		RowLayout rl = new RowLayout();
		rl.type = SWT.VERTICAL;
		rl.fill = true;
		debugView.setLayout(layout);
		
		debugLabel = new Label(debugView, SWT.BORDER);
		debugLabel.setText("Debugging");
		
		RowData rd = new RowData();
		rd.width = SWT.FILL;
		rd.height = SWT.FILL;
		debugTree = new Tree(debugView, SWT.SINGLE);
		debugTree.setLayoutData(gd);
		debugTree.setLayout(layout);
		
		
		// Create the main repl view
		
		parent = mainView;
		
		parent.setLayout(layout);
		
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
 		//in.appendVerifyKeyListener(new PrevCommandsShortcuts());
 		in.appendVerifyKeyListener(new CheckEvalListener());
        in.appendVerifyKeyListener(new SelectAllListener());
        
 		IKeyBindingService keys = this.getSite().getKeyBindingService();
 		keys.setScopes(new String[] { "jasko.tim.lisp.context1" });
 		PreviousREPLCommandAction prevCmdAction = new PreviousREPLCommandAction(this);
 		prevCmdAction.setActionDefinitionId("jasko.tim.lisp.actions.PreviousREPLCommandAction");
 		keys.registerAction(prevCmdAction);
 		NextREPLCommandAction nextCmdAction = new NextREPLCommandAction(this);
 		nextCmdAction.setActionDefinitionId("jasko.tim.lisp.actions.NextREPLCommandAction");
 		keys.registerAction(nextCmdAction);
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
 		
 		pushEvalState();
 		
 		registerSwankListeners();
 		
 		fillToolBar(parent);
 		fillMenu(parent);
 		
 		if (swank != null) {
 			swank.sendEval("(format nil \"You are running ~a ~a via Cusp v" + LispPlugin.getVersion() +
                    "\" (lisp-implementation-type) (lisp-implementation-version))\n", null);
 		}
 		
 		parentControl.layout(false);
	}
	
	private void hideFrame(Composite control) {
		GridData hider = new GridData();
		hider.exclude = true;
		control.setLayoutData(hider);
		control.setVisible(false);
		
		parentControl.layout(false);
	}
	
	private void showFrame(Composite control) {
		GridData shower = new GridData();
		shower.horizontalAlignment = SWT.FILL;
		shower.verticalAlignment = GridData.FILL;
		shower.grabExcessHorizontalSpace = true;
		shower.grabExcessVerticalSpace = true;
		shower.exclude = false;
		control.setLayoutData(shower);
		control.setVisible(true);
		
		parentControl.layout(false);
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
				PackageDialog pd = new PackageDialog(ReplView.this.getSite().getShell(),
						swank.getAvailablePackages(5000), swank.getPackage());
				if (pd.open() == Dialog.OK) {
					switchPackage(pd.getPackage());
				}
			}
		};
		packageButton.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.DEFPACKAGE));
		packageButton.setToolTipText("Change Package");
		
		Action pauseButton = new Action("Interrupt execution") {
			public void run() {
				getSwank().sendInterrupt(null);
			}
		};
		pauseButton.setImageDescriptor(LispImages.getImageDescriptor(LispImages.THREAD_DEBUG));
		pauseButton.setToolTipText("Interrupt execution");
		
		
		Action clearButton = new Action("Clear Console") {
			public void run() {
				IDocument doc = history.getDocument();
				doc.set("");
			}
		};
		clearButton.setImageDescriptor(LispImages.getImageDescriptor(LispImages.CLEAR));
		clearButton.setToolTipText("Clear Console");
		
		tbm.add(clearButton);
		tbm.add(pauseButton);
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
	
	
	/**
	 * This is a real class rather than an anonymous one so that it can be cloned properly.
	 *  If it weren't, you'd sometimes get results printed twice on the repl
	 */
	protected class DisplayRunnable extends SwankRunnable {
		public ReplView rv;
		
		public void run() {
			rv.appendText(result.value + "\n");
			scrollDown();
		}
		
		public SwankRunnable clone() {
			DisplayRunnable re = new DisplayRunnable();
			re.result = this.result;
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
				pushReadState(result.get(1).value, result.get(2).value);
				appendText(swank.fetchDisplayText() + "\n");
				scrollDown();
			}
		});
		
		swank.addDebugListener(new SwankRunnable() {
			public void run() {
				
				pushDebugState(result);
			} // run()
		});
		
	}
	
	protected void scrollDown() {
		history.revealRange(history.getDocument().getLength(), 0);
	}
	
	
	private Color backgroundColor;
	
	
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

	/**
	 * Checks the next repl command to be issued for a valid in-package form.
	 * If one is found, the name specified in the form is checked against the list
	 * of available packages.  If the specified package name is available, then
	 * the swank connection's current package is changed as requested.
	 */
	private void checkSwitchPackage (String replCmd) {
		LispNode n = LispParser.parse(replCmd);
		if (n.car().car().value.toUpperCase().equals("IN-PACKAGE")) {
			String packageName = n.car().cadr().value.toUpperCase();
			if (packageName.startsWith(":")) packageName = packageName.substring(1);
			if (!packageName.equals(swank.getPackage())) {
				ArrayList<String> packages = swank.getAvailablePackages(5000);
				if (packages.contains(packageName.toUpperCase())) switchPackage(packageName);
			}
		}
	}

	/**
	 * Switches the swank connection's current package to the given package name,
	 * prints an appropriate commented message, and forces the repl to scroll to the
	 * bottom.
	 */
	private void switchPackage (String packageName) {
		swank.setPackage(packageName);
		appendText(";Package changed to " + packageName + "\n");
		scrollDown();
	}

	protected void eval() {
		String cmd = in.getDocument().get();
		if (!cmd.endsWith("\n")) {
			cmd = cmd + "\n"; 
		}
		String cleanCmd = cmd.replace("\r", "");
		State state = currState();
		
		if (state.handle(cmd, cleanCmd)) {
			popState();
		}
		
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
    public void showPreviousCommandFromHistory () {
    	 --currPrevCommand;
    	if (currPrevCommand < 0) {
    		currPrevCommand = prevCommands.size() - 1;
    	}
    	if (currPrevCommand >=0 ) {
    		in.getDocument().set(prevCommands.get(currPrevCommand));
    		in.setSelectedRange(in.getDocument().getLength(), 0);
    	}
    }
    	    
    public void showNextCommandFromHistory () {
    	++currPrevCommand;
    	if (currPrevCommand >= prevCommands.size()) {
    		currPrevCommand = 0;
    	}
    	if (prevCommands.size() > 0) {
    		in.getDocument().set(prevCommands.get(currPrevCommand));
    		in.setSelectedRange(in.getDocument().getLength(), 0);
    	}
    }
    
    protected class SelectAllListener implements VerifyKeyListener {
        public void verifyKey (VerifyEvent evt) {
            if (evt.keyCode == 'a') {
                if (evt.stateMask == SWT.CONTROL ||
                        (evt.stateMask == SWT.COMMAND && Platform.getOS().equals(Platform.OS_MACOSX))) {
                    in.setSelectedRange(0, in.getDocument().getLength());
                }
            }
        }
    }
	
	protected class CheckEvalListener implements VerifyKeyListener {
		public void verifyKey(VerifyEvent event) {
			if ((event.keyCode == '\r' || event.keyCode == '\n')
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
	
	
	
	
	//******************************
	//      State handling
	//******************************
	
	protected void pushState(State s) {
		states.push(s);
		applyCurrentState();
	}
	
	protected void popState() {
		states.pop();
		applyCurrentState();
	}
	
	protected State currState() {
		return states.peek();
	}
	
	private void applyCurrentState() {
		State state = currState();
		Display display = this.getSite().getShell().getDisplay();
		Color newColor = state.getColor(display);
	
		in.getControl().setBackground(newColor);
		in.getControl().setForeground(newColor);
		replComposite.setBackground(newColor);
		
		if (backgroundColor != null) {
			backgroundColor.dispose();
		}
		backgroundColor = newColor;
		
		state.activate();
	}
	
	
	protected void pushEvalState() {
		pushState(new EvalState());
	}
	
	protected void pushDebugState(LispNode debugInfo) {
		pushState(new DebugState(debugInfo));
	}
	
	protected void pushReadState(String s1, String s2) {
		pushState(new ReadState(s1, s2));
	}
	
	
	
	
	
	
	protected class EvalState implements State {

		public Color getColor(Display display) {
			return null;
		}

		public boolean handle(String command, String cleanCommand) {
			System.out.println(cleanCommand);
			prevCommands.add(command.trim());
			currPrevCommand = prevCommands.size();
			appendText(swank.getPackage() + "> " + command);
			scrollDown();
			
			swank.sendEval(cleanCommand, new ReturnHandler());
			
			checkSwitchPackage(command);
			
			return false;
		}

		public void activate() {
		}
	}
	
	
	
	protected class ReadState implements State {
		private String stringNum1, stringNum2;
		
		public ReadState(String s1, String s2) {
			stringNum1 = s1;
			stringNum2 = s2;
		}

		public Color getColor(Display display) {
			return new Color(display, 0, 255, 0);
		}

		public boolean handle(String command, String cleanCommand) {
			swank.sendReadString(cleanCommand, null, stringNum1, stringNum2);
			appendText(">> " + command);
			scrollDown();
			return true;
		}

		public void activate() {
		}
	
	}
	
	
	
	
	

	protected class DebugState implements State, 
		MouseListener, KeyListener, TreeListener, SelectionListener {
		
		private int numDebugOptions;
		private LispNode desc, options, backtrace;
		
		public DebugState(LispNode debugInfo) {
			desc = debugInfo.get(3);
			options = debugInfo.get(4);
			backtrace = debugInfo.get(5);
			
			numDebugOptions = options.params.size();
		}

		public Color getColor(Display display) {
			return new Color(display, 255, 0, 0);
		}

		public boolean handle(String command, String cleanCommand) {
			return false;
		}

		public void activate() {
			debugLabel.setText(desc.car().value + "\n" + desc.cadr().value);
			debugTree.removeAll();
			
			appendText(desc.car().value + "\n" + desc.cadr().value + "\n");
			
			for (int i=0; i<options.params.size(); ++i) {
				LispNode option = options.get(i);
				appendText("\t" + i + ": [" + option.car().value + "] "
						+ option.cadr().value + "\n");
				TreeItem item = new TreeItem(debugTree, 0);
				item.setText(i + ": [" + option.car().value + "] " + option.cadr().value);
				item.setData(i);
			} // for
			
			scrollDown();
			
			TreeItem bt = new TreeItem(debugTree, SWT.NONE);
			bt.setText("==Backtrace==");
			bt.setData(null);
			
			for (int i=0; i<backtrace.params.size(); ++i) {
				LispNode trace = backtrace.get(i);
				//appendText("\t[" + trace.car().value + "] " + trace.cadr().value + "\n");
				
				TreeItem item = new TreeItem(bt, 0);
				item.setText(trace.car().value + "] " + trace.cadr().value);
				item.setData(null);
				item.setData("frame", i);
				
				TreeItem tmp = new TreeItem(item, 0);
				tmp.setText("Getting data...");
			} // for
			bt.setExpanded(true);
			
			debugTree.addSelectionListener(this);
			debugTree.addTreeListener(this);
			debugTree.addMouseListener(this);
			debugTree.addKeyListener(this);
			debugView.addKeyListener(this);
			
			hideFrame(mainView);
			showFrame(debugView);
			debugTree.setFocus();
		}
		
		public void choose(Integer choice) {
			swank.sendDebug(choice.toString(), null);
			
			appendText("]> " + choice + "\n");
			scrollDown();
			
			debugTree.removeSelectionListener(this);
			debugTree.removeTreeListener(this);
			debugTree.removeMouseListener(this);
			debugTree.removeKeyListener(this);
			debugView.removeKeyListener(this);
			
			hideFrame(debugView);
			showFrame(mainView);
			popState();
			in.getControl().setFocus();
		}
		
		public void treeExpanded(TreeEvent e) {
			final TreeItem sel = (TreeItem)e.item;
			
			if (sel.getData("frame") != null) {
				Object frame = sel.getData("frame");
				
				getSwank().sendGetFrameLocals(frame.toString(), new SwankRunnable() {
					public void run() {
						sel.removeAll();
						LispNode vars = result.getf(":return").getf(":ok");
						if (vars.params.size() <= 0) {
							TreeItem tmp = new TreeItem(sel, 0);
							tmp.setText("[No Locals]");
						} else {
							for (LispNode var : vars.params) { 
								String name = var.getf(":name").value;
								String val = var.getf(":value").value;
								TreeItem varItem = new TreeItem(sel, 0);
								varItem.setText(name + " = " + val);
							}
						}
					}
				});
			}
		}
		
		public void widgetSelected(SelectionEvent e) {
			final TreeItem sel = (TreeItem)e.item;
			
			if (sel.getData("frame") != null) {
				Object frame = sel.getData("frame");
				getSwank().sendGetFrameSourceLocation(frame.toString(), new SwankRunnable() {
					public void run() {
						LispNode res = result.getf(":return").getf(":ok");
						if (!res.car().value.equals(":error")) {
							String file = res.getf(":file").value;
							int pos = res.getf(":position").asInt();
							String snippet = res.getf(":snippet").value;
							LispEditor.jumpToDefinition(file, pos, snippet);
							setFocus();
						}
					}
				});
			}
		}

		
		public void mouseDoubleClick(MouseEvent e) {
			TreeItem item = debugTree.getItem(new Point(e.x, e.y));
			if (item != null && item.getData() != null && item.getData() instanceof Integer) {
				Integer choice = (Integer) item.getData();
				choose(choice);
			}
		}
		
		public void mouseDown(MouseEvent e) {
			/*TreeItem item = debugTree.getItem(new Point(e.x, e.y));
			if (item != null && item.getData("frame") != null) {
				Object frame = item.getData("frame");
				getSwank().sendGetFrameSourceLocation(frame.toString(), new SwankRunnable() {
					public void run() {
						LispNode res = result.getf(":return").getf(":ok");
						if (!res.car().value.equals(":error")) {
							String file = res.getf(":file").value;
							int pos = res.getf(":position").asInt();
							String snippet = res.getf(":snippet").value;
							LispEditor.jumpToDefinition(file, pos, snippet);
						}
					}
				});
			}*/
		}
		
		public void keyPressed(KeyEvent e) {
			if (e.character == '\r' || e.character == '\n') {
				TreeItem[] sels = debugTree.getSelection();
				if (sels.length > 0) {
					TreeItem item = sels[0];
					if (item.getData() != null && item.getData() instanceof Integer) {
						Integer choice = (Integer) item.getData();
						choose(choice);
					}
				}
			} else {
				Character c = new Character(e.character);
				try {
					int choice = Integer.parseInt(c.toString());
					if (choice >=0 && choice < numDebugOptions) {
						choose(choice);
					}
				} catch (NumberFormatException ex) {
				}
			}
		}


		
		public void mouseUp(MouseEvent e) {
		}
		public void keyReleased(KeyEvent e) {
		}
		public void treeCollapsed(TreeEvent e) {
		}
		public void widgetDefaultSelected(SelectionEvent e) {
		}
	}
}
