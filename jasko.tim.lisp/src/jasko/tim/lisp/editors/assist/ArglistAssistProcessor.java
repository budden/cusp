package jasko.tim.lisp.editors.assist;

import java.util.*;

import jasko.tim.lisp.*;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.util.*;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;

/**
 * One of the more important parts of an IDE, IMO.
 * This class provides both symbol completion and context info.
 * @author Tim Jasko
 *
 */
public class ArglistAssistProcessor implements IContentAssistProcessor {
	
	private static final int TIMEOUT = 2000;
	
	private String lastCompletionVariable = ")"; // impossible variable name
	private String[] lastCompletionResults;
	private LispEditor editor;
	
	public ArglistAssistProcessor(LispEditor editor) {
		this.editor = editor;
	}

	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer,
			int offset) {
		
		String variable = LispUtil.getCurrentWord(viewer.getDocument(), offset);
		
		if (variable.equals("") || variable.contains("\"")) {
			return null;
		}
		
		// Optimization! Save us lots of swanking
		if (variable.startsWith(lastCompletionVariable) && !variable.contains(":")) {
			ArrayList<ICompletionProposal> temp
				= new ArrayList<ICompletionProposal>(lastCompletionResults.length);
			int rStart = offset - variable.length();
			for (String comp: lastCompletionResults) {
				if (comp.startsWith(variable)) {
					temp.add(new CompletionProposal(comp, rStart,
							variable.length(), comp.length()));
				}
			}
			
			// Displaying a completion for something that is already complete is dumb.
			if (temp.size() == 1) {
				if (temp.get(0).getDisplayString().equals((variable))) {
					return null;
				}
			}
			
			ICompletionProposal[] ret = new ICompletionProposal[temp.size()];
			for (int i=0; i<temp.size(); ++i) {
				ret[i] = temp.get(i);
			}
			return ret;
		} else {
			//System.out.println("*asking swank");
			String[] results = (editor != null ?
					LispPlugin.getDefault().getSwank().getCompletions(variable, editor.getPackage(), TIMEOUT) :
					LispPlugin.getDefault().getSwank().getCompletions(variable, TIMEOUT));
			//System.out.println("*results received");
			// Displaying a completion for something that is already complete is dumb.
			if (results.length == 1) {
				if (results[0].equals(variable)) {
					return null;
				}
			}
			ICompletionProposal[] ret = new ICompletionProposal[results.length];
			int rStart = offset - variable.length();
			for (int j=0; j<results.length; ++j) {
				ret[j] = new CompletionProposal(results[j].toLowerCase(), rStart,
						variable.length(), results[j].length());
			}
			//System.out.println("*proposals made");
			
			lastCompletionVariable = variable;
			lastCompletionResults = results;
			return ret;
		}
	}
	
	
	public static final char[] completionActivators = "qwertyuiopasdfghjklzxcvbnm*!-:".toCharArray();
	
	public char[] getCompletionProposalAutoActivationCharacters() {
		return completionActivators;
	}
	
	
	
	//*****************************************
	//         Context Info
	
	/**
	 * Lets our ContextValidator know whether or not it needs to dismiss the
	 *  make-instance special tooltip
	 */
	protected boolean makeInstanceInfoFound = false;
	protected boolean defmethodInfoFound = false;

	public IContextInformation[] computeContextInformation(ITextViewer viewer,
			int offset) {
		
		String function = LispUtil.getCurrentFunction(viewer.getDocument(), offset);
		
		SwankInterface swank = LispPlugin.getDefault().getSwank();
		String info = "";
		
		// Special arglist assistance for make-instance
		if (function.equals("make-instance")) {
			LispNode exp = LispParser.parse(LispUtil.getCurrentUnfinishedExpression(viewer.getDocument(), offset));
			System.out.println("*" + exp);
			if (exp.get(0).params.size() >= 2) {
				String className = exp.get(0).params.get(1).value;
				className = className.replaceFirst("\'", "");
				System.out.println("className:" + className);
				
				if (editor != null) {
					info = LispPlugin.getDefault().getSwank()
						.getMakeInstanceArglist(className, editor.getPackage(), TIMEOUT);
				} else {
					info = LispPlugin.getDefault().getSwank()
						.getMakeInstanceArglist(className, TIMEOUT);
				}
				makeInstanceInfoFound = true;
			}
		} else if (function.equals("defmethod")) {
			LispNode exp = LispParser.parse(LispUtil.getCurrentUnfinishedExpression(viewer.getDocument(), offset));
			if (exp.get(0).params.size() >= 2) {
				String arg0 = exp.get(0).params.get(1).value;
				
				if (editor != null) {
					info = LispPlugin.getDefault().getSwank()
						.getSpecialArglist("defmethod", arg0, editor.getPackage(), TIMEOUT);
				} else {
					info = LispPlugin.getDefault().getSwank()
						.getSpecialArglist("defmethod", arg0, TIMEOUT);
				}
				defmethodInfoFound = true;
				System.out.println("info:" + info);
			}
		}
		
		if (info.equals("")) {
			makeInstanceInfoFound = false;
			defmethodInfoFound = false;
			if (editor == null) {
				info = swank.getArglist(function, 3000);
			} else {
				info = swank.getArglist(function, 3000, editor.getPackage());
			}
			String docString = swank.getDocumentation(function, 1000);
			if (!docString.equals("")) {
				String[] lines = docString.split("\n");
				if (lines.length > 2) {
					for (int i=0; i<2; ++i) {
						info += "\n" + lines[i];
					}
					info += "...";
				} else {
					info += "\n" + docString;
				}
			}
		}
		if (info != null && !info.equals("") && !info.equals("nil")) {
			return new IContextInformation[] {
				new ContextInformation(info, info)
			};
		}
		return null;
	}
	

	//TODO Use swank:documentation-symbol to get info where available
	// swank:describe-symbol ain't bad either

	private static final char[] infoActivators = new char[] { ' ' };
	
	public char[] getContextInformationAutoActivationCharacters() {
		return infoActivators;
	}

	public String getErrorMessage() {
		return null;
	}

	public IContextInformationValidator getContextInformationValidator() {
		return new ArglistContext();
	}

	

	/**
	 * Tells Eclipse when the given context info is no longer valid.
	 *  Also provides text formatting for the tooltip.
	 * @author Tim
	 *
	 */
	private class ArglistContext implements IContextInformationValidator, IContextInformationPresenter {
		private ITextViewer viewer;
		IContextInformation info;

		public void install(IContextInformation info, ITextViewer viewer, int offset) {
			this.viewer = viewer;
			this.info = info;
		}

		public boolean isContextInformationValid(int offset) {
			try {
				char c = viewer.getDocument().getChar(offset-1);
				if (c == '(' || c == ')') {
					return false;
				}
				if (!makeInstanceInfoFound && Character.isWhitespace(c) &&
						LispUtil.getCurrentFunction(viewer.getDocument(), offset).equals("make-instance")) {
					return false;
				}
				if (!defmethodInfoFound && Character.isWhitespace(c) &&
						LispUtil.getCurrentFunction(viewer.getDocument(), offset).equals("defmethod")) {
					return false;
				}
			} catch (BadLocationException e) {
				
			}
			return true;
		}



		public boolean updatePresentation(int offset, TextPresentation pres) {
			String display = info.getInformationDisplayString();
			LispNode stuff = LispParser.parse(display);
			pres.addStyleRange(new StyleRange(0, stuff.get(0).endOffset, null, null, SWT.BOLD));
			return true;
		}
	}


}
