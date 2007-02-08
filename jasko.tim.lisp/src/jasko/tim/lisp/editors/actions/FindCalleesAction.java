package jasko.tim.lisp.editors.actions;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.LispNode;
import jasko.tim.lisp.swank.SwankInterface;
import jasko.tim.lisp.swank.SwankRunnable;
import jasko.tim.lisp.util.LispUtil;

import java.util.ArrayList;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.AbstractTextEditor;

public class FindCalleesAction extends Action implements IEditorActionDelegate {
	private LispEditor editor;
	
	public FindCalleesAction() {
	}
	
	public FindCalleesAction(AbstractTextEditor editor) {
		this.editor = (LispEditor) editor;
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		editor = (LispEditor) targetEditor;
	}
	
	public void run() {
		ITextSelection ts = (ITextSelection) editor.getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		String symbol = LispUtil.getCurrentFullWord(
				editor.getDocumentProvider().getDocument(editor.getEditorInput()), offset);
		SwankInterface swank = LispPlugin.getDefault().getSwank();
		
		swank.sendGetCallees(symbol, editor.getPackage(), new SwankRunnable() {
			public void run() {
				LispNode guts = result.getf(":return").getf(":ok");
				
				ArrayList<String> optionNames = new ArrayList<String>(guts.params.size());
				ArrayList<LispNode> optionData = new ArrayList<LispNode>(guts.params.size());
				ArrayList<String> tips = new ArrayList<String>(guts.params.size());
				for (LispNode gut: guts.params) {
					for (int i = 1; i<gut.params.size(); ++i) {
						LispNode possibility = gut.params.get(i);
						String name = possibility.get(0).value;
						String tip = possibility.getf(":location").getf(":file").value;
						optionNames.add(name);
						optionData.add(possibility);
						tips.add(tip);
					}
				}
				
				LispNode chosen;
				
				if (optionNames.size() <= 0) {
					editor.showPopupInfo("No calls from this function were found");
					return;
				} else if (optionNames.size() == 1) {
					chosen = optionData.get(0);
				} else {
					ListDialog<LispNode> win = new ListDialog<LispNode>(editor.getSite().getShell(), optionNames, optionData, tips);
					win.create();
					win.setTitle("Callees");
				
					if (win.open() == Dialog.OK) {
						chosen = win.getData();
					} else {
						return;
					}
				}
				
				LispNode location = chosen.getf(":location");
				String path = location.getf(":file").value;
				int position = location.getf(":position").asInt();
				String snippet = location.getf(":snippet").value;
				
				LispEditor.jumpToDefinition(path, position, snippet);
			}
		
		});
	}

	public void run(IAction action) {
		run();
	}

	public void selectionChanged(IAction action, ISelection selection) {

	}

}
