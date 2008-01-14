package jasko.tim.lisp.swank;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.inspector.InspectorView;
import jasko.tim.lisp.preferences.PreferenceConstants;

import java.io.*;
import java.util.*;
import java.net.*;

import org.eclipse.jface.preference.IPreferenceStore;
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
	
	public String getCurrPackage() {
		return currPackage;
	}
	
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
	 * Listeners to be given debug info, usually right before a :debug-activate.
	 */
	private List<SwankRunnable> debugInfoListeners;
	
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
		debugInfoListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
		displayListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
		readListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
		disconnectListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
		indentationListeners = Collections.synchronizedList(new ArrayList<SwankRunnable>(1));
	
		initIndents();
		connect();
	}
	
	public Hashtable<String, String> specialIndents;
	public Hashtable<String, String> fletIndents;
	public Hashtable<String, Integer> indents;
	public Hashtable<String, String> handlerCaseIndents;

	private void initIndents() {
		// for forms that get indented like flet
		fletIndents = new Hashtable<String, String>();
		fletIndents.put("flet",			"  ");
		fletIndents.put("labels",		"  ");
		fletIndents.put("macrolet",		"  ");
		
		// for forms that get indented like handler-case
		handlerCaseIndents = new Hashtable<String, String>();
		handlerCaseIndents.put("handler-case", "  ");
		
		// forms that always get indented a certain number of spaces
		// Why are flet, labels, etc here as well as in fletIndents?
		//  specialIndents controls how you get indented as a result of your parent form
		//  The indenting we do for fletIndents happens when those forms are the great-grandparents
		specialIndents = new Hashtable<String, String>();
		specialIndents.put("",				" ");
		specialIndents.put("progn",			"  ");
		specialIndents.put("if",			"    ");
		specialIndents.put("cond",			"  ");
		specialIndents.put("when",			"  ");
		specialIndents.put("unless",		"  ");
		specialIndents.put("let",			"  ");
		specialIndents.put("let*",			"  ");
		specialIndents.put("dolist",		"  ");
		specialIndents.put("flet",			"  ");
		specialIndents.put("labels",		"  ");
		specialIndents.put("macrolet",		"  ");
		specialIndents.put("dotimes",		"  ");
		specialIndents.put("lambda",		"  ");
		specialIndents.put("defun",			"  ");
		specialIndents.put("defvar",		"  ");
		specialIndents.put("defparameter",	"  ");
		specialIndents.put("eval-when",		"  ");
		specialIndents.put("multiple-value-bind", "  ");
		specialIndents.put("unwind-protect","  ");
		specialIndents.put("block",			"  ");
		
		// All additional custom indents will go here
		indents = new Hashtable<String, Integer>();
		indents.put("do", 2);
		
		addIndentationListener(new SwankRunnable() {
			public void run() {
				LispNode updates = result.get(1);
				for (LispNode update : updates.params) {
					String symbol = update.get(0).value;
					// The LispParser doesn't understand dotted lists,
					//  and just sees the dot as another element
					int paramNum = update.get(2).asInt();
					indents.put(symbol, paramNum);
				}
			
			}
		});
	}
	
	
	
	public boolean isConnected() {
		return connected;
	}
	

	public boolean managePackages = false;

	public String translateRemoteFilePath(String path) {
		if (implementation != null) {
			return implementation.translateRemoteFilePath(path);
		} else {
			return path;
		}
	}

	public void runAfterLispStart() {
		if( isConnected() ){
			
			String contribs = "(progn (swank:swank-require :swank-fuzzy)"
				+ "(swank:swank-require :swank-asdf)"
				+ "(swank:swank-require :swank-presentations)"
				+ "(swank:swank-require :swank-fancy-inspector)"
				+ "(swank:swank-require :swank-presentations)"
				+ "(swank:swank-require :swank-arglists)"
				+ ")";
			sendEval(contribs, null);
			//sendEval("(swank:fancy-inspector-init)", null);
			
			IPreferenceStore prefs = 
				LispPlugin.getDefault().getPreferenceStore();
			managePackages = 
				prefs.getBoolean(PreferenceConstants.MANAGE_PACKAGES);
			if( managePackages){
				
				String asdfext = LispPlugin.getDefault().getPluginPath() 
					+ "asdf-extensions/asdf-extensions.lisp";
				System.out.printf("asdf path: %s\n", asdfext);
				sendEvalAndGrab("(load \"" + asdfext + "\")", 3000);
				
				String path = 
					LispPlugin.getDefault().getPluginPath() + "libraries";

			    // This filter only returns directories of type jasko.tim.lisp.libs
			    FileFilter libPluginFilter = new FileFilter() {
			        public boolean accept(File file) {
			            return (file.isDirectory()
			            		&& file.toString().matches(".*jasko\\.tim\\.lisp\\.libs.*"));
			        }
			    };
			    			    
			    ArrayList<File> topLevelDirs = new ArrayList<File>();
			    topLevelDirs.add(new File(path));
			    
				String sysdirs[] = 
					prefs.getString(PreferenceConstants.SYSTEMS_PATH).split(";");
				
				for(String sysdir: sysdirs){
					if( sysdir != null && !sysdir.equals("")){
						topLevelDirs.add(new File(sysdir));			
					}
				}
			    
				File pluginsDir = (new File(LispPlugin.getDefault().getPluginPath())).getParentFile();
			    for( File dir : pluginsDir.listFiles(libPluginFilter)){
			    	topLevelDirs.add(new File(dir.getAbsolutePath()+"/libs"));
			    }
				
			    // This filter only returns directories
			    FileFilter dirFilter = new FileFilter() {
			        public boolean accept(File file) {
			            return file.isDirectory();
			        }
			    };

				ArrayList<File> subdirs = new ArrayList<File>();
				for( File dir : topLevelDirs){
					if( dir.isDirectory() ){
						subdirs.add(dir);
						for( File subdir : dir.listFiles(dirFilter) ){
							if( subdir != null && !subdirs.contains(subdir) ){
								subdirs.add(subdir);			
							}
						}						
					}
				}
			    
				if (subdirs.size() == 0) {
					// Either dir does not exist or is not a directory
					System.out.println("*libraries dir not found! " + path);
				} else {
					String code = "(progn\n";
					for (int i = 0; i < subdirs.size(); i++) {
						File child = subdirs.get(i);
						String name = child.getAbsolutePath().replace("\\", "/");
						if (!name.endsWith("/")) {
							name += "/";
						}
						code += "  (com.gigamonkeys.asdf-extensions:register-source-directory\"" + name + "\")\n"; 
					}
					code += ")";
					System.out.println("code: " + code);
					System.out.println(sendEvalAndGrab(code, 1000));
				}
			}
			
			String str = prefs.getString(PreferenceConstants.LISP_INI);
			if( str != "")	{
				str = str.replaceAll("\\\\", "/");
				sendEvalAndGrab("(when (probe-file \""+str+"\") (load \""+str+"\"))\n", 3000);
			}
			sendEval("(format nil \"You are running ~a ~a via Cusp v" + LispPlugin.getVersion() +
					"\" (lisp-implementation-type) (lisp-implementation-version))\n", null);
		}
	}
	
	/** 
	 * Connects to the swank server.
	 * 
	 * @return whether connecting was successful
	*/
	public boolean connect() {
		connected = false;
		currPackage = "COMMON-LISP-USER";
		//IPreferenceStore store = LispPlugin.getDefault().getPreferenceStore();
		
		synchronized(port) {
			++port;
			implementation = null;
			
			// Find an implementation and start a lisp process
			// the pecking order of lisps:
			if (implementation == null) {
				implementation = RemoteImplementation.findImplementation();
			}
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
					lispEngine = implementation.start(slimePath, port);
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
			
			connectStreams(slimePath);
			
			
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
					
					try {
						int val = lispEngine.exitValue();
						System.out.println("exit: " + val);
						
						lispEngine = implementation.startHarder(slimePath, port);
						connectStreams(slimePath);
					} catch (IllegalThreadStateException e2) {
						System.out.println("lisp instance still loading...");
					} catch (IOException e2) {
						e.printStackTrace();
					}
					
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
		if (echoSocket != null && echoSocket.isConnected()) {
			connected = true;
		} else {
			connected = false;
		}
		return connected;
	}
	
	private void connectStreams(String slimePath) {
		if (stdOut != null) { // never cross the streams
			stdOut.running = false;
		}
		if (stdErr != null) {
			stdErr.running = false;
		}
		// it's not happy unless we hook up to clear out the output
		stdOut = new DisplayListenerThread(lispEngine.getInputStream(), true);
		stdErr = new DisplayListenerThread(lispEngine.getErrorStream(), true);
		
		stdOut.start();
		stdErr.start();
		
		try {
			commandInterface = new DataOutputStream(lispEngine.getOutputStream());
			//commandInterface.writeBytes("(progn (swank:create-swank-server " + port + " nil) (quit))\n");
			//commandInterface.writeBytes("(load \"" + slimePath.replace("\\", "\\\\") + "\")\n");
			String slimeLoadCmd = implementation.getLoadSwankCommand();
			if (slimeLoadCmd != null) {
				commandInterface.writeBytes(slimeLoadCmd);
			}
			commandInterface.writeBytes("(swank:create-server :coding-system \"utf-8\" :port " + port + ")\n");
			commandInterface.flush();
			
			// FIXME: at this point we should wait for "Swank started" message on standard output
			// for now, simply sleep a bit. This is only necessary with RemoteLispImplementation
			// because SSH tunnel will allow a successful connection for echoSocket, but will
			// send a reset later.
			try {
				Thread.sleep(1000);
			} catch(Exception e) {
				// ignore
			}
			
		} catch (IOException e2) {
			e2.printStackTrace();
		}
	}
	
	public void reconnect() {
		disconnect();
		connect();
	}
	
	public void disconnect() {
		System.out.println("*disconnect");
		if (System.getProperty("os.name").toLowerCase().contains("windows")) {
			if (lispEngine != null) {
				lispEngine.destroy();
				lispEngine = null;
			}
		}
		
		/*try {
			commandInterface.writeBytes(implementation.getQuitForm() + "\n");
			commandInterface.flush();
		} catch (IOException e) {
			e.printStackTrace();
		}*/
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
	
	public void addDebugInfoListener(SwankRunnable callBack) {
		debugInfoListeners.add(callBack);
	}
	
	public void addDisplayCallback(SwankDisplayRunnable callBack) {
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
	
	//finds definitions in package pkg
	private synchronized boolean haveDefinitionsPkg(String symbol, String pkg, long timeout) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);

		String code = formatCode(symbol);
		String pkgstring = pkg;
		if( !pkg.equals("") && pkg.startsWith(":")) {
			pkgstring = pkg.substring(1);
		}
		if( !pkgstring.equals("") ) {
			pkgstring += "::";
		}
		String msg = "(handler-case (swank:find-definitions-for-emacs \""+pkgstring+code
			+"\") (simple-type-error () nil))";
		if( implementation.lispType().equalsIgnoreCase("SBCL") ) { //quiet compilation warnings
			msg = "(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) "
				+ msg + ")";
		}
		return (!sendEvalAndGrab(msg,2000).equalsIgnoreCase("nil"));
	}

	//finds definitions in package pkg or global context
	public synchronized boolean haveDefinitions(String symbol, String pkg, long timeout) {
		return (haveDefinitionsPkg(symbol,pkg,timeout) || haveDefinitionsPkg(symbol,"",timeout));
	}
	
	/**
	 * 
	 * @param start of the string
	 * @param pkg current package (if null, then swank.currPackage)
	 * @param timeout timeout for swank call
	 * @param n number of completions to get: 0 if no limit (limit works only for fuzzy completions)
	 * @return string[2][n] : string[0] - completions, string[1] - arglists + docs 
	 */
	public synchronized String[][] getCompletionsAndDocs(String start, String pkg, long timeout, int n){
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);

		IPreferenceStore prefs = LispPlugin.getDefault().getPreferenceStore();
		boolean usefuzzy = prefs.getBoolean(PreferenceConstants.AUTO_FUZZY_COMPLETIONS);

		String msg = "";
		String usepkg = currPackage;

		if(usefuzzy){
			msg = "(let ((lst (mapcar #'first (let ((x (swank:fuzzy-completions ";
		} else {
			msg = "(let ((lst (first (swank:simple-completions ";
		}
		msg += "\"" + start + "\" ";
		if( pkg == null ){
			msg += cleanPackage(currPackage)+" ";				
		} else {
			msg += cleanPackage(pkg)+" ";
			usepkg = pkg;
		}
		if( usefuzzy  ){
			if( n > 0 ){
				msg += ":limit " + n;
			}
			msg += "))) (if (listp (first (first x))) (first x) x)";
		}
		msg += "))))";
		msg += "(list lst (mapcar #'(lambda (x) (swank:operator-arglist x " + cleanPackage(pkg) + ")) lst)" +
				" (mapcar #'(lambda (x) (swank:documentation-symbol x)) lst)))";
		LispNode resNode = LispParser.parse(sendEvalAndGrab(msg, usepkg, timeout));
		LispNode compl = resNode.car().get(0);
		LispNode args = resNode.car().get(1);
		LispNode docs = resNode.car().get(2);
		int nn = compl.params.size();
		String[][] res = new String[2][nn];
		if (false ){
			return res;			
		}

		for( int i = 0; i < nn; ++i ){
			String info = args.get(i).value;
			if (info.equalsIgnoreCase("nil") || info.contains("not available")){
				info = "";
			}
			String docString = docs.get(i).value;
			if (!docString.equals("") && !docString.equalsIgnoreCase("nil")) {
				String[] lines = docString.split("\n");
				int maxlines = prefs.getInt(PreferenceConstants.MAX_HINT_LINES);
				if (maxlines > 0 && lines.length > maxlines) {
					for (int j=0; j<maxlines; ++j) {
						info += "\n" + lines[j];
					}
					info += "...";
				} else {
					info += "\n" + docString;
				}
			}
			res[0][i] = compl.get(i).value;
			res[1][i] = info;
		}
		return res;
		
	}
	
	public synchronized String[] getCompletions(String start, long timeout) {
		return getCompletions(start, currPackage, timeout);
	}
	
	public synchronized String[] getCompletions(String start, String pkg, long timeout) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);

		IPreferenceStore prefs = LispPlugin.getDefault().getPreferenceStore();
		String msg = "";
		int nlim = 0;
		
		boolean usefuzzy = prefs.getBoolean(PreferenceConstants.AUTO_FUZZY_COMPLETIONS);
		if ( usefuzzy ){
			String tlimit = "50";
			String nlimit = prefs.getString(PreferenceConstants.AUTO_COMPLETIONS_NLIMIT);
			if (!nlimit.matches("\\d+")) {
				nlimit = "0";
			} else {
				nlim = prefs.getInt(PreferenceConstants.AUTO_COMPLETIONS_NLIMIT);
			}
			msg = "(swank:fuzzy-completions \"" + start + "\" " + cleanPackage(pkg) 
				+ " :limit " + nlimit + " :time-limit-in-msec "+ tlimit + ")";			
		} else {
			msg = "(swank:simple-completions \"" + start + "\" " + cleanPackage(pkg) + ")";			
		}
		
		try {
			synchronized (callBack) {
				if (emacsRex(msg)) {
					callBack.wait(timeout);
					LispNode results = callBack.result.cadr().cadr();
					// for some reasons sometimes it is in car() and at other times just at results
					if( results.cadr().value.equalsIgnoreCase("nil")){
						results = results.car();
					}
					String[] ret = new String[results.params.size()];
					if ( usefuzzy ) {
						int nn = nlim;
						if( nn == 0 ){
							nn = results.params.size();
						} else {
							nn = Math.min(results.params.size(),nn);
						}
						for (int i=0; i<nn; ++i) {
							ret[i] = results.get(i).car().value;
						} // for												
					} else {
						for (int i=0; i<results.params.size(); ++i) {
							ret[i] = results.get(i).value;
						} // for						
					}
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
		
		String msg = "(swank:operator-arglist \"" + formatCode(function) + "\" " + cleanPackage(currPackage) + " )";
		
		try {
			synchronized (callBack) {
				if (emacsRex(msg,currPackage)) {
		
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
	
	public synchronized void sendGetArglist(String function, String currPackage, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:operator-arglist \"" + formatCode(function) + "\" + " + cleanPackage(currPackage) + ")";
		
		emacsRex(msg, currPackage);
	}
	
	public synchronized String getMakeInstanceArglist(String className, long timeout) {
		return getMakeInstanceArglist(className, currPackage, timeout);
	}
	
	public synchronized String getMakeInstanceArglist(String className, String pkg, long timeout) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);
		//(swank:arglist-for-echo-area (quote ((:make-instance "some-class" "make-instance"))))
		String msg = "(swank:arglist-for-echo-area (quote ((\"MAKE-INSTANCE\" \"'"
			+ formatCode(className) + "\"))))";
		
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
	
	public synchronized String getSpecialArglist(String function, String arg0, long timeout) {
		return getSpecialArglist(function, arg0, currPackage, timeout);
	}
	
	public synchronized String getSpecialArglist(String function, String arg0, String pkg, long timeout) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);
		//(swank:arglist-for-echo-area (quote ((:make-instance "some-class" "make-instance"))))
		String msg = "(swank:arglist-for-echo-area (quote ((\"" + formatCode(function) + "\" \""
			+ formatCode(arg0) + "\" ))))";
		
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
	
	public synchronized void sendGetDocumentation(String function, String pkg, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:documentation-symbol \"" + formatCode(function) + "\")";
		
		emacsRex(msg, pkg);
	}
	
	public synchronized String getDocumentation(String function, long timeout) {
 		return getDocumentation(function,currPackage,timeout);
 	}
 	
 	public synchronized String getDocumentation(String function, String pkg, long timeout) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);
		
		String msg = "(swank:documentation-symbol \"" + formatCode(function) + "\")";
		
		try {
			synchronized (callBack) {
				if (emacsRex(msg, pkg)) {
		
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

	public synchronized String sendEvalAndGrab(String message, long timeout) {
		return sendEvalAndGrab(message,"nil", timeout);
	}
	
	public synchronized String sendEvalAndGrab(String message,  String pkg, long timeout) {
		SyncCallback callBack = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callBack);

		String msg = "(swank:eval-and-grab-output \"" + formatCode(message) + "\")";

		try {
			synchronized (callBack) {
				if (emacsRex(msg,pkg)) {
					callBack.wait(timeout);
					LispNode res = callBack.result.getf(":return").getf(":ok");
					if ( res.params.size() > 0 ) {
						return (res.params.get(1).value);						
					} else {
						return "";
					}
				} else {
					return "";
				}
			} // sync
		} catch (Exception e) {
			e.printStackTrace();
			return "";
		} // catch
	}
	
		
	public synchronized void sendDebug(String commandNum, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:invoke-nth-restart-for-emacs 1 "
			+ commandNum + ")";
		
		emacsRex(msg);
	}
	
	public synchronized void sendAbortDebug(SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:sdbl-abort)";
		
		emacsRex(msg);
	}
	
	public synchronized void sendContinueDebug(SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:sdbl-continue)";
		
		emacsRex(msg);
	}
	
	public synchronized void sendQuitDebug(SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:throw-to-toplevel)";
		
		emacsRex(msg);
	}
	
	// Inspection related
	
	public synchronized void sendInspectReplResult(String num, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:init-inspector \"(swank:get-repl-result #10r" + num 
			+ ")\" )";
		
		emacsRex(msg);
	}
	
	public synchronized void sendInspectInspectedPart(String partNum, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:inspect-nth-part " + partNum + ")";
		
		emacsRex(msg);
	}
	
	public synchronized void sendInspectorPop(SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:inspector-pop)";
		emacsRex(msg);
	}
	
	public synchronized void sendInspectorNext(SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:inspector-next)";
		emacsRex(msg);
	}
	
	public synchronized void sendInspectFrameLocal(String threadNum, String frameNum, String varNum, SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:init-inspector \"(swank:get-repl-result '(:frame-var " 
			+ threadNum + " " + frameNum + " " + varNum + "))\" :reset t :eval t :dwim-mode nil)";
		
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
	
	public synchronized void sendInterrupt(SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(:emacs-interrupt :repl-thread)\n";
		
		sendRaw(msg);
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
		if (pckg.equalsIgnoreCase("nil")) {
			emacsRex(msg);
		} else {
			emacsRex(msg, pckg);
		}
	}
	
	public synchronized void sendCompileFile(String filePath, SwankRunnable callBack) {
		registerCallback(new CompileRunnable(callBack));
		filePath = filePath.replace('\\', '/');
		filePath = implementation.translateLocalFilePath(filePath);
		String msg = "(swank:compile-file-for-emacs \""
			+ filePath + "\" t)";
		
		emacsRex(msg);
	}
	
	public synchronized void sendLoadASDF(String fileFullPath, SwankRunnable callBack) {
 		fileFullPath = fileFullPath.replace('\\', '/');
 		fileFullPath = implementation.translateLocalFilePath(fileFullPath);
 		String[] fpathparts = fileFullPath.split("/");
 		if( fpathparts.length > 0 && fpathparts[fpathparts.length-1].matches(".+\\.asd") ){
 			/*String tmp = */sendEvalAndGrab("(load \"" + fileFullPath + "\")",2000);
 			String asdName = fpathparts[fpathparts.length-1].replace(".asd", "");
 			registerCallback(new CompileRunnable(callBack));
 			String msg = "(swank:operate-on-system-for-emacs \"" + asdName + "\" \"LOAD-OP\")";
 			emacsRex(msg);			
 		}
 	}
		
	public synchronized void sendLoadPackage(String pkg) {
		sendEvalAndGrab("(asdf:operate 'asdf:load-op :"+pkg+")",3000);
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
	
	public synchronized void sendGetAvailablePackages(SwankRunnable callBack) {
		registerCallback(callBack);
		String msg = "(swank:list-all-package-names t)";
		
		emacsRex(msg);
	}
	
	public synchronized ArrayList<String> getAvailablePackages(long timeout) {
		SyncCallback callback = new SyncCallback();
		++messageNum;
		syncJobs.put(new Integer(messageNum).toString(), callback);

		java.util.ArrayList<String> packageNames = new java.util.ArrayList<String>();

		try {
			synchronized (callback) {
				if (emacsRex("(swank:list-all-package-names t)")) {
					callback.wait(timeout);
					LispNode packages = callback.result.getf(":return").getf(
							":ok");
					for (LispNode p : packages.params) {
						packageNames.add(p.value);
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		return packageNames;
	}
	
	public synchronized void sendGetInstalledPackages(SwankRunnable callBack) {
		if (managePackages) {
			registerCallback(callBack);
			String msg = "(swank:eval-and-grab-output \"" 
				+ formatCode("(get-installed-packages)") + "\")";
			emacsRex(msg,"com.gigamonkeys.asdf-extensions");
		} else {
			callBack.result = null;
			callBack.run();
		}
	}
	
	public synchronized LispNode getInstalledPackages(long timeout) {
		if ( managePackages ){
			
			SyncCallback callback = new SyncCallback();
			++messageNum;
			syncJobs.put(new Integer(messageNum).toString(), callback);

			String res = sendEvalAndGrab("(get-installed-packages)",
					"com.gigamonkeys.asdf-extensions", timeout);
			LispNode resnode = LispParser.parse(res);
			return resnode;
		} else {
			return null;
		}
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
	
	public synchronized void sendProfileReset(SwankRunnable callBack) {
		registerCallback(callBack);
		emacsRex("(swank:profile-reset)");
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
		if (pkg == null || pkg.equals("") || pkg.equalsIgnoreCase("nil")) {
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
			if (out != null) {
				//Messages are prepending by their length, given as a 6-char string
				// which is a hexadecimal number. Not sure why they do it this way.
				String hexLen = Integer.toHexString(message.length());
				
	            switch (hexLen.length()) {
	                case 1: out.write('0');
	                case 2: out.write('0');
	                case 3: out.write('0');
	                case 4: out.write('0');
	                case 5: out.write('0');
				}
				
				out.writeBytes(hexLen);
				out.write(message.getBytes("UTF-8"));
				out.flush();
			} else {
				return false;
			}
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
				//runnable.resultString = result.value;
				Display.getDefault().asyncExec(runnable);
			}
		}
	}
	
	private class ListenerThread extends Thread {
		private BufferedReader in;
		public boolean running = true;
		
		public ListenerThread(InputStream stream) {
			super("Swank Listener");
			
			try {
                in = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
            } catch (UnsupportedEncodingException e) {
            	System.out.println("Could not load UTF-8 character set -- something seriously wrong....");
            	e.printStackTrace();
            	throw new IllegalStateException("Could not initialize swank listener -- UTF-8 character set not available.", e);
            }
		}
		
		
		private void handle(LispNode node) {
			try {
				if (node.car().value.equalsIgnoreCase(":debug-activate")) {
					signalListeners(debugListeners, node);
				} else if (node.car().value.equalsIgnoreCase(":debug")) {
					signalListeners(debugInfoListeners, node);
				} else if (node.car().value.equalsIgnoreCase(":read-string")) {
					signalListeners(readListeners, node);
				} else if (node.car().value.equalsIgnoreCase(":presentation-start")) {
					for (int i=0; i<displayListeners.size(); ++i) {
						SwankDisplayRunnable runnable = (SwankDisplayRunnable) displayListeners.get(i);
						runnable.startPresentation(node.get(1).value);
					}
				} else if (node.car().value.equalsIgnoreCase(":presentation-end")) {
					for (int i=0; i<displayListeners.size(); ++i) {
						SwankDisplayRunnable runnable = (SwankDisplayRunnable) displayListeners.get(i);
						runnable.endPresentation();
					}
				} else if (node.car().value.equalsIgnoreCase(":write-string")) {
					signalListeners(displayListeners, node);
				} else if (node.car().value.equalsIgnoreCase(":indentation-update")) {
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
							int c = in.read();
							if (c >= 0) {
								sb.append((char)c);
							} else {
								System.out.println("Connection closed");
								signalListeners(disconnectListeners, new LispNode());
								return;
							}
						}
						int length = Integer.parseInt(sb.toString(), 16);
						sb = new StringBuilder();
						for (int i=0; i<length; ++i) {
							int c = in.read();
							if (c >= 0) {
								sb.append((char)c);
							} else {
								System.out.println("Connection closed");
								signalListeners(disconnectListeners, new LispNode());
								return;
							}
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
					return;
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
					int cint = in.read();
					char c = (char)cint;
					if (cint < 0) {
						System.out.println("Display input pipe closed.");
						return;
					} else if (c == '\n') {
						acc.append(c);
						// Things are much faster display-wise if we grab several lines at a time
						if (lines > 5 || !in.ready()) {
							synchronized(acc) {
								String curr = acc.toString();
								if (echo) {
									for (int i=0; i<displayListeners.size(); ++i) {
										SwankRunnable runnable = displayListeners.get(i).clone();
										LispNode result = new LispNode();
										result.params.add(new LispNode(":write-string"));
										result.params.add(new LispNode(curr));
										runnable.result = result;
										Display.getDefault().asyncExec(runnable);
									}
								}
								System.out.print("]");
								System.out.println(curr);
						
								acc = new StringBuffer();
							}
							lines = 0;
						} else {
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
	

