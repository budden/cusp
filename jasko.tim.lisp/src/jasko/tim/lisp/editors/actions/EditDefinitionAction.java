package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.*;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.util.*;

import java.util.*;

import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;

public class EditDefinitionAction extends Action implements IEditorActionDelegate {
	private LispEditor editor;
	
	public EditDefinitionAction() {
	}
	
	public EditDefinitionAction(LispEditor editor) {
		this.editor = editor;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (LispEditor) targetEditor;
	}
	
	public void run() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		
		String symbol = LispUtil.getCurrentFullWord(doc, offset);
		symbol = symbol.replace("'", "");
		symbol = symbol.replace("`", "");
		
		//System.out.println("*" + symbol);
		
		if (!symbol.equals("")) {
			SwankInterface swank = LispPlugin.getDefault().getSwank();
			swank.sendFindDefinitions(symbol, editor.getPackage(), new OpenDefinitionRunnable(symbol));
		}
		
	}
	
	private class OpenDefinitionRunnable extends SwankRunnable {
		private String symbol;
		
		public OpenDefinitionRunnable(String symbol) {
			this.symbol = symbol;
		}
		public void run() {
			LispNode guts = result.getf(":return").getf(":ok");
			ArrayList<String> names = new ArrayList<String>(guts.params.size());
			ArrayList<LispNode> data = new ArrayList<LispNode>(guts.params.size());
			ArrayList<String> tips = new ArrayList<String>(guts.params.size());
			for (LispNode possibility: guts.params) {
				String name = possibility.get(0).value;
				names.add(name);
				data.add(possibility);
				if (possibility.get(1).get(0).value.equals(":error")) {
					tips.add(possibility.get(1).get(1).value);
					System.out.println("**" + possibility.get(1).get(1).value);
				} else {
					tips.add(possibility.get(1).getf(":file").value);
					System.out.println("**" + possibility.get(1).getf(":file").value);
				}
			}
			
			LispNode chosen;
			
			if (names.size() <= 0) {
				editor.showPopupInfo("Unable to find definitions");
				return;
			} else if (names.size() == 1) {
				chosen = data.get(0);
			} else {
				ListDialog<LispNode> win = new ListDialog<LispNode>(editor.getSite().getShell(),
						names, data, tips);
				win.create();
				win.setTitle("Definitions");
				
				if (names.get(0).startsWith("(DEFGENERIC")) {
					win.setSelection(1);
				} else {
					win.setSelection(0);
				}
				
				if (win.open() == Dialog.OK) {
					chosen = win.getData();
				} else {
					return;
				}
			}
			
			LispNode location = chosen.get(1);
			System.out.println(location);
			if (location.get(0).value.equals(":error")) {
				editor.showPopupInfo(location.getf(":error").value);
				return;
			}
			String path = location.getf(":file").value;
			int position = location.getf(":position").asInt();
			String snippet = location.getf(":snippet").value;
			
			LispEditor.jumpToDefinition(path, position, snippet, symbol);
			
		}
	}

	public void run(IAction action) {
		run();
	}
	

	public void selectionChanged(IAction action, ISelection selection) {

	}
	


}
