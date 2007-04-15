package jasko.tim.lisp.swank;

import jasko.tim.lisp.LispPlugin;

import java.io.*;
import java.util.*;
import java.net.*;

import org.eclipse.swt.widgets.Display;

/*
C:\sbcl\bin\sbcl.exe --load "C:/slime/swank-loader.lisp" --eval "(swank:create-swank-server 4005 nil)"
*/

/**
 * The core guts of the plugin. All traffic to and from our lisp implementation goes through here.
 * Cusp operates off of the swank server that comes with slime. Thus, to your lisp
 *  implementation, it's really no different from slime. Swank makes our lives easier, as
 *  we don't have to worry about implementing all of that stuff that the smart
 *  Swank developers did for us (in a cross-implementation fashion, even).
 * Now, a lot of this could probably be cleaned up, and wisened up by somebody more familiar with
 *  Slime/Swank development. I have learned the protocol almost entirely through packet sniffing.
 *  My methods seem to work, but I could be rather naive in some places.
 * It should also be noted that the Swank developers are not above changing the protocol
 *  for no good reason. Have fun, maintainers!
 * 
 * Rough overview:
 *  Requests/commands sent to Lisp are associated with a SwankRunnable subclass, whose run() method processes
 *   the result once it comes in. Pass <code>null</code> if you don't care about the result.
 *  Through here, we also register various listeners for the various events that Lisp might
 *   invoke (the debugger, for example). Probably 90% of these are subscribed to by the Repl. 
 *  
 * TODO:
 * -Indentation is very dumb right now, and needs to be made smarter.
 *  
 * 
 *  @see SwankRunnable
 *  @author Tim Jasko
 */
public class SwankInterface {
	
	public LispImplementation implementation;
	 	
	/** Port of the Swank server */
	private static Integer port = 4004;
	
	private Socket echoSocket = null;
	private Socket secondary = null;
	private DataOutputStream out = null;
	DataOutputStream commandInterface = null;
	private int messageNum = 1;
	
	/** Holds whether we are connected to Swank. */
	private boolean connected = false;
	private String currPackage = "COMMON-LISP-USER";
	
	private ListenerThread listener;
	private DisplayListenerThread displayListener;
	
	private DisplayListenerThread stdOut;
	private DisplayListenerThread stdErr;
	
	/**
	 * Holds all outstanding jobs that we're waiting for Lisp to finish processing,
	 *  except those that are being executed synchronously.
	 */
	private Hashtable<String, SwankRunnable> jobs = new Hashtable<String, SwankRunnable>();
	/**
	 * Holds jobs that are being executed synchronously. We handle them slightly differently.
	 */
	private Hashtable<String, SyncCallback> syncJobs = new Hashtable<String, SyncCallback>();
	
	/**
	 * Listeners to be notified when the debugger is activated.
	 */
	private List<SwankRunnable> debugListeners;
	
	/**
	 * Listeners to be notified of anything to be output.
	 */
	private List<SwankRunnable> displayListeners;
	
	/**
	 * Listeners to be notified whenever Lisp is trying to read something from the user.
	 */
	private List<SwankRunnable> readListeners;
	
	/**
	 * For those who want to be informed of the death of our Lisp.
	 */
	private List<SwankRunnable> disconnectListeners;
	
	/**
	 * Anybody who wants to know that indentation has been updated.
	 */
	private List<SwankRunnable> indentationListeners;
	
	Process lispEngine;
	
	public SwankInterface() {
		debugListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
		displayListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
		readListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
		disconnectListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
		indentationListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
	
		initIndents();
		connect();
	}
	
	public Hashtable<String, String> indents;
	public Hashtable<String, String> fletIndents;

	/**
	 * TODO: Indentation is very dumb right now, and needs to be made smarter.
	 *
	 */
	private void initIndents() {
		fletIndents = new Hashtable<String, String>();
		fletIndents.put("flet",			"  ");
		fletIndents.put("labels",		"  ");
		fletIndents.put("macrolet",		"  ");
		
		indents = new Hashtable<String, String>();
		
		indents.put("",				" ");
		indents.put("if",			"    ");
		indents.put("when",			"  ");
		indents.put("unless",		"  ");
		indents.put("let",			"  ");
		indents.put("let*",			"  ");
		indents.put("flet",			"  ");
		indents.put("labels",		"  ");
		indents.put("macrolet",		"  ");
		indents.put("do",			"  ");
		indents.put("dolist",		"  ");
		indents.put("dotimes",		"  ");
		indents.put("lambda",		"  ");
		indents.put("defun",		"  ");
		indents.put("defvar",		"  ");
		indents.put("defparameter",	"  ");
		indents.put("eval-when",	"  ");
		indents.put("multiple-value-bind", "  ");
		indents.put("unwind-protect", "  ");
		indents.put("block", "  ");
		
		addIndentationListener(new SwankRunnable() {
			public void run() {
				LispNode updates = result.get(1);
				for (LispNode update : updates.params) {
					String symbol = update.get(0).value;
					indents.put(symbol, "  ");
				}
			
			}
		});
	}
	
	
	
	public boolean isConnected() {
		return connected;
	}
	
	/**
	 * 
	 * @return
	protected Process startAppropriateLispProcess()
	{
		Process lispProcess = null;
		LispImplementation impl = null;
		//the pecking order of lisps:
		if (impl == null) impl = SBCLImplementation.findImplementation();
		if (impl == null) impl = AllegroImplementation.findImplementation();
		
		if (impl != null)
			lispProcess = impl.start("");
		
		return lispProcess;
	}
	*/
	  	
	/** 
	 * Connects to the swank server.
	 * 
	 * @return whether connecting was successful
	*/
	public boolean connect() {
		currPackage = "COMMON-LISP-USER";
		//IPreferenceStore store = LispPlugin.getDefault().getPreferenceStore();
		
		synchronized(port) {
			++port;
			
			// Find an implementation and start a lisp process
			// the pecking order of lisps:
			if (implementation == null) {
				implementation = SiteWideImplementation.findImplementation();
 			}
			if (implementation == null) {
				implementation = SBCLImplementation.findImplementation();
			}
			if (implementation == null) {
				implementation = AllegroImplementation.findImplementation();
			}

			String pluginDir = LispPlugin.getDefault().getPluginPath();
			String slimePath = pluginDir + "slime/swank-loader.lisp";
			if (implementation != null) {
				try {
					lispEngine = implementation.start(slimePath);
				} catch (IOException e3) {
					e3.printStackTrace();
					return false;
				}
			} else {
				try {
					ProcessBuilder pb = new ProcessBuilder(new String[] {
							"sbcl", "--load", slimePath });
					lispEngine = pb.start();
				} catch (IOException e) {
					return false;
				}
			}
			
			// it's not happy unless we hook up to clear out the output
			stdOut = new DisplayListenerThread(lispEngine.getInputStream(), true);
			stdErr = new DisplayListenerThread(lispEngine.getErrorStream(), true);
			
			stdOut.start();
			stdErr.start();
			
			try {
				commandInterface = new DataOutputStream(lispEngine.getOutputStream());
				//commandInterface.writeBytes("(progn (swank:create-swank-server " + port + " nil) (quit))\n");
				commandInterface.writeBytes("(load \"" + slimePath.replace("\\", "\\\\") + "\")\n");
				commandInterface.writeBytes("(swank:create-server :port " + port + ")\n");
				commandInterface.flush();
				
			} catch (IOException e2) {
				e2.printStackTrace();
			}
			
			
			int tries = 7;
			do {
				try {
					echoSocket = new Socket("localhost", port);
					out = new DataOutputStream(echoSocket.getOutputStream());
					
					listener = new ListenerThread(echoSocket.getInputStream());
					listener.start();
					tries = 0;
					
				} catch (UnknownHostException e) {
					return false;
				} catch (IOException e) {
					System.err.println("Couldn't connect to swank (" + tries + " more tries).");
					if (tries > 0) {
						try {
							Thread.sleep(7000);
							--tries;
						} catch (InterruptedException e1) {
							e1.printStackTrace();
						}
					} else {
						return false;
					}
				}
			} while (tries > 0);
		} // synchronized
		
		//sendRaw("(:emacs-rex (swank:connection-info) nil t 1)");
		connected = true;
		return true;
	}
	
	public void reconnect() {
		disconnect();
		connect();
	}
	
	public void disconnect() {
		
		try {
			commandInterface.writeBytes(implementation.getQuitForm() + "\n");
			commandInterface.flush();
		} catch (IOException e) {
			e.printStackTrace();
		}
		try {
			if (displayListener != null) {
				displayListener.running = false;
			}
			if (stdOut != null) {
				stdOut.running = false;
			}
			if (stdErr != null) {
				stdErr.running = false;
			}
			
			
			listener.running = false;
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		try {
			if (lispEngine != null) {
				currPackage = "COMMON-LISP-USER";
				//sendEval("(quit)", null);
				
				
				//lispEngine.destroy();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public void addReadListener(SwankRunnable callBack) {
		readListeners.add(callBack);
	}
	
	public void addDebugListener(SwankRunnable callBack) {
		debugListeners.add(callBack);
	}
	
	public void addDisplayCallback(SwankRunnable callBack) {
		displayListeners.add(callBack);
	}
	
	public void addDisconnectCallback(SwankRunnable callBack) {
		disconnectListeners.add(callBack);
	}
	
	public void addIndentationListener(SwankRunnable callBack) {
		indentationListeners.add(callBack);
	}
	
	private void registerCallback(SwankRunnable callBack) {
		++messageNum;
		if (callBack != null) {
			jobs.put(new Integer(messageNum).toString(), callBack);
		}
	}
	
	public String getPackage() {
		return currPackage;
	}
	
	public synchronized void setPackage(String p) {
		++messageNum;
		String newPackage = formatCode(p);
		emacsRex("(swank:set-package \"" + newPackage + "\")", currPackage);
		
		currPackage = newPackage;
	}
	
	public synchronized String[] getCompletions(String start, long timeout) {
		return getCompletions(start, currPackage, timeout);
	}
	
	public synchronized String[] getCompletions(String start, String pkg, long timeout) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);
		
		String msg = "(swank:completions \"" + start + "\" " + cleanPackage(pkg) + ")";
		
		try {
			synchronized (callBack) {
				if (emacsRex(msg)) {
					callBack.wait(timeout);
					LispNode results = callBack.result.cadr().cadr().car();
					String[] ret = new String[results.params.size()];
					for (int i=0; i<results.params.size(); ++i) {
						ret[i] = results.get(i).value;
					} // for
					return ret;
				} else {
					return null;
				}
			} // sync
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		} // catch
	}
	
	public synchronized String getArglist(String function, long timeout) {
		return getArglist(function, timeout, currPackage);
	}
	
	
	public synchronized String getArglist(String function, long timeout, String currPackage) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);
		
		String msg = "(swank:arglist-for-echo-area (quote (\"" + formatCode(function) + "\")))";
		
		try {
			synchronized (callBack) {
				if (emacsRex(msg)) {
		
					callBack.wait(timeout);
					return callBack.result.cadr().cadr().value;
				} else {
					return "";
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
			return "";
		}
	}
	
	public synchronized String getMakeInstanceArglist(String className, long timeout) {
		return getMakeInstanceArglist(className, currPackage, timeout);
	}
	
	public synchronized String getMakeInstanceArglist(String className, String pkg, long timeout) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);
		//(swank:arglist-for-echo-area (quote ((:make-instance "some-class" "make-instance"))))
		String msg = "(swank:arglist-for-echo-area (quote ((:make-instance \""
			+ formatCode(className) + "\" \"make-instance\"))))";
		
		try {
			synchronized (callBack) {
				if (emacsRex(msg, pkg)) {
		
					callBack.wait(timeout);
					return callBack.result.getf(":return").getf(":ok").value;
				} else {
					return "";
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
			return "";
		}
	}
	
	public synchronized String getDocumentation(String function, long timeout) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);
		
		String msg = "(swank:documentation-symbol \"" + formatCode(function) + "\")";
		
		try {
			synchronized (callBack) {
				if (emacsRex(msg)) {
		
					callBack.wait(timeout);
					String result = callBack.result.getf(":return").getf(":ok").value;
					if (result.equalsIgnoreCase("nil")) {
						return "";
					} else {
						return result;
					}
				} else {
					return "";
				}
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
			return "";
		}
	}
	
	private class SyncCallback {
		public LispNode result = new LispNode();
	}
	
	public synchronized void sendEval(String message, SwankRunnable callBack) {
		registerCallback(callBack);
		message = message + "\n";
		String msg = "(swank:listener-eval \"" + formatCode(message) + "\")";
		
		emacsRex(msg);
	}
	
	public synchronized void sendDebug(String commandNum, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:invoke-nth-restart-for-emacs 1 "
			+ commandNum + ")";
		
		emacsRex(msg);
	}
	
	// Debug related
	
	public synchronized void sendGetFrameLocals(String frameNum, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:frame-locals-for-emacs " + frameNum + ")";
		
		emacsRex(msg);
	}
	
	public synchronized void sendGetFrameSourceLocation(String frameNum, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:frame-source-location-for-emacs " + frameNum + ")";
		
		emacsRex(msg);
	}
	
	public synchronized void sendDisassemble(String symbol, String pkg, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:disassemble-symbol \"" + symbol + "\")";
		
		emacsRex(msg, pkg);
	}
	
	public synchronized void sendReadString(String input, SwankRunnable callBack, String num1, String num2) {
		registerCallback(callBack);
		String msg = "(:emacs-return-string " + num1 + " " + num2 + " \"" + formatCode(input) + "\")";
		
		sendRaw(msg);
	}
	
	public synchronized void sendFindDefinitions(String symbol, String pkg, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:find-definitions-for-emacs \"" + formatCode(symbol) + "\")";
		
		emacsRex(msg, pkg);
	}
	
	public synchronized void sendUndefine(String symbol, String pkg, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:undefine-function \"" + formatCode(symbol) + "\")";
		
		emacsRex(msg, pkg);
	}
	
	// Threads
	
	public synchronized void sendListThreads(SwankRunnable callBack) {
		registerCallback(callBack);
		emacsRex("(swank:list-threads)");
	}
	
	public synchronized void sendDebugThread(String threadNum, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:debug-nth-thread " + threadNum + ")";
		
		emacsRex(msg);
	}
	
	public synchronized void sendKillThread(String threadNum, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:kill-nth-thread " + threadNum + ")";
		
		emacsRex(msg);
	}
	
	
	
	public synchronized void sendApropos(String regex, SwankRunnable callBack) {
		registerCallback(callBack);
		// Need to protect against these, as they send lisp into an endless loop.
		// Probably others do as well, but these I know about.
		if (regex.equals("+")) {
			regex = "\\+";
		} else if (regex.equals("*")) {
			regex = "\\*";
		}
		String msg = "(swank:apropos-list-for-emacs \""
			+ formatCode(regex) + "\" t nil)";
		
		emacsRex(msg);
	}
	
	
	
	public synchronized void sendMacroExpand(String code, SwankRunnable callBack, boolean all, String pckg) {
		registerCallback(callBack);
		String msg;
		if (all) {
			msg = "(swank:swank-macroexpand-all \"" + formatCode(code) + "\")";
		} else {
			msg = "(swank:swank-macroexpand-1 \"" + formatCode(code) + "\")";
		}
		
		emacsRex(msg, pckg);
	}
	
	// Compiling
	
	public synchronized void sendCompileString(String expr, String file, String dir, int offset, String pckg, SwankRunnable callBack) {
		registerCallback(new CompileRunnable(callBack));
		System.out.println(file);
		System.out.println(dir);
		String msg = "(swank:compile-string-for-emacs \""
			+ formatCode(expr) + "\" \""
			+ formatCode(dir + file) + "\" " + (offset+1) + " \"" + formatCode(dir)
			+ "\")";
		if (pckg.equals("nil")) {
			emacsRex(msg);
		} else {
			emacsRex(msg, pckg);
		}
	}
	
	public synchronized void sendCompileFile(String filePath, SwankRunnable callBack) {
		registerCallback(new CompileRunnable(callBack));
		filePath = filePath.replace('\\', '/');
		String msg = "(swank:compile-file-for-emacs \""
			+ filePath + "\" t)";
		
		emacsRex(msg);
	}
	
	
	
	private class CompileRunnable extends SwankRunnable {
		private SwankRunnable callBack;
		
		public CompileRunnable(SwankRunnable callBack) {
			this.callBack = callBack;
		}
		
		public void run() {
			sendCompileCheck(callBack);
		}
	}
	
	protected synchronized void sendCompileCheck(SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:compiler-notes-for-emacs)";
		
		emacsRex(msg);
	}
	
	public synchronized void sendGetPackages(SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:list-all-package-names t)";
		
		emacsRex(msg);
	}
	
	
	// X-ref
	
	public synchronized void sendGetCallers(String functionName, String pkg, SwankRunnable callBack) {
		registerCallback(callBack);
		
		emacsRex("(swank:xref (quote :callers) (quote \"" + formatCode(functionName) + "\"))", pkg);
	}
	
	public synchronized void sendGetCallees(String functionName, String pkg, SwankRunnable callBack) {
		registerCallback(callBack);
		
		emacsRex("(swank:xref (quote :callees) (quote \"" + formatCode(functionName) + "\"))", pkg);
	}
	
	// Profiling
	
	public synchronized void sendToggleProfileFunction(String functionName, String pkg, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:toggle-profile-fdefinition \"" + formatCode(functionName) + "\")"; 
		emacsRex(msg, pkg);
	}
	
	public synchronized void sendReportProfile(SwankRunnable callBack) {
		registerCallback(callBack);
		emacsRex("(swank:profile-report)");
	}
	
	
	
	private void signalResponse(LispNode reply) {
		String jobNum = reply.get(reply.params.size() - 1).value;
		Object r = jobs.get(jobNum);
		if (r != null) {
			SwankRunnable runnable = (SwankRunnable) r;
			runnable.result = reply;
			Display.getDefault().asyncExec(runnable);
			jobs.remove(jobNum);
		} else {
			SyncCallback sync = syncJobs.get(jobNum);
			if (sync != null) {
				sync.result = reply;
				syncJobs.remove(jobNum);
				synchronized (sync) {
					sync.notifyAll();
				}
			} // if
		} // else
	}
	
	private String formatCode(String code) {
		return code.replace("\\", "\\\\").replace("\"", "\\\"").replace("\r", "");
	}
	
	public String fetchDisplayText() {
		return displayListener.fetchText();
	}
	
	
	private String cleanPackage(String pkg) {
		if (pkg.equals("") || pkg.equals("nil")) {
			return "nil";
		} else {
			return "\"" + formatCode(pkg) + "\"";
		}
	}
	
	public synchronized boolean emacsRex(String message) {
		String msg = "(:emacs-rex " + message + " nil :repl-thread " + messageNum + ")";
		
		return sendRaw(msg);
	}
	
	public synchronized boolean emacsRex(String message, String pkg) {
		String msg = "(:emacs-rex " + message + " " + cleanPackage(pkg) + " :repl-thread " + messageNum + ")";
		
		return sendRaw(msg);
	}
	
	public synchronized boolean sendRaw(String message) {
		//message = message + "\n";
		System.out.println("-->" + message);
		try {
			//Messages are prepending by their length, given as a 6-char string
			// which is a hexadecimal number. Not sure why they do it this way.
			String hexLen = Integer.toHexString(message.length());
			
			while (hexLen.length() < 6) {
				hexLen = "0" + hexLen;
			}
			
			//out.writeByte(0);
			//out.writeShort(message.length());
			out.writeBytes(hexLen);
			out.writeBytes(message);
			out.flush();
		} catch (IOException e) {
			signalListeners(disconnectListeners, new LispNode());
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	private void signalListeners(List<SwankRunnable> listeners, LispNode result) {
		synchronized (listeners) {
			for (int i=0; i<listeners.size(); ++i) {
				SwankRunnable runnable = listeners.get(i).clone();
				runnable.result = result;
				runnable.resultString = result.value;
				Display.getDefault().asyncExec(runnable);
			}
		}
	}
	
	private class ListenerThread extends Thread {
		private BufferedReader in;
		public boolean running = true;
		
		public ListenerThread(InputStream stream) {
			super ("Swank Listener");
			
			in = new BufferedReader(new InputStreamReader(stream));
		}
		
		
		private void handle(LispNode node) {
			try {
				if (node.car().value.equals(":debug")) {
					signalListeners(debugListeners, node);
				} else if (node.car().value.equals(":read-string")) {
					signalListeners(readListeners, node);
				} else if (node.car().value.equals(":write-string")) {
					if (! node.get(1).value.equals("\n")) {
						signalListeners(displayListeners, node.get(1));
					}
				} else if (node.car().value.equals(":indentation-update")) {
					signalListeners(indentationListeners, node);
				} else {
					signalResponse(node);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		public void run() {
			while (running) {
				try {
					if (true) {
						StringBuilder sb = new StringBuilder();
						for (int i=0; i<6; ++i) {
							sb.append((char)in.read());
						}
						int length = Integer.parseInt(sb.toString(), 16);
						sb = new StringBuilder();
						for (int i=0; i<length; ++i) {
							sb.append((char) in.read());
						}
						String reply = sb.toString();
					/*String reply = in.readLine();
					
					if (reply != null && !reply.equals("")) {
						//make sure we don't end early because of newlines in the length
						reply += "\n";
						while (!checkParens(reply) || reply.length() <= 3) {
							String more = in.readLine();
							if (more != null && !more.equals("")) {
								reply += more + "\n";
							}
							if (!running) return; // break out of here if we're done.
						} // while*/
						System.out.println("<--" + reply);
						System.out.flush();
						if (reply.contains("open-dedicated-output-stream")) {
							StringTokenizer tokens = new StringTokenizer(reply, " )");
							while (tokens.hasMoreTokens()) {
								try {
									String token = tokens.nextToken();
									System.out.println(token);
									int tmp = Integer.parseInt(token);
									System.out.println("secondary:" + tmp);
									secondary = new Socket("localhost", tmp);
									displayListener 
										= new DisplayListenerThread(secondary.getInputStream(), true);
									displayListener.start();
								} catch (Exception e){
									//e.printStackTrace();
								} // catch
							} // while
						} else {
							//System.out.println("parsing");
							handle(LispParser.parse(reply.substring(1)));
						}
					} // if
					
				} catch (IOException e) {
					e.printStackTrace();
					signalListeners(disconnectListeners, new LispNode());
				}
			} // while
			System.out.println("Done listening");
		}
		
	} // class
	
	
	
	private class DisplayListenerThread extends Thread {
		private BufferedReader in;
		public boolean running = true;
		//private String curr = "";
		private StringBuffer acc = new StringBuffer();
		private boolean echo;
		
		public DisplayListenerThread(InputStream stream, boolean echo) {
			super ("Secondary Swank Listener");
			this.echo = echo;
			this.in = new BufferedReader(new InputStreamReader(stream));
		}
		
		public String fetchText() {
			synchronized(acc) {
				String ret = acc.toString();
				acc = new StringBuffer();
				return ret;
			}
		}
		
		
		public void run() {
			int lines = 0;
			while (running) {
				try {
					char c = (char)in.read();
					if (c == '\n') {
						// Things are much faster display-wise if we grab several lines at a time
						if (lines > 5 || !in.ready()) {
							synchronized(acc) {
								String curr = acc.toString();
								if (echo) {
									for (int i=0; i<displayListeners.size(); ++i) {
										SwankRunnable runnable = displayListeners.get(i).clone();
										runnable.resultString = curr;
										Display.getDefault().asyncExec(runnable);
									}
								}
								System.out.print("]");
								System.out.println(curr);
						
								acc = new StringBuffer();
							}
							lines = 0;
						} else {
							acc.append(c);
							lines += 1;
						}
						
					} else {
						synchronized(acc) {
							acc.append(c);
						}
					}
					
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		
	} // class
} // class
	

