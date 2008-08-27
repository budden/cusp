package jasko.tim.lisp.editors.actions;

import java.util.ArrayList;

import jasko.tim.lisp.editors.LispEditor;
import jasko.tim.lisp.swank.*;

import org.eclipse.jface.dialogs.Dialog;


public class FindCallersAction extends LispAction {
	private static final int TIMEOUT = 2000;
	
	public FindCallersAction() {
	}
	
	public FindCallersAction(LispEditor editor) {
		super(editor);
	}
	
	public void run() {
		String symbol = getSymbol();

		boolean haveDefinition = getSwank().haveDefinitions(symbol, 
				getPackage(), TIMEOUT);
		
		if ( !haveDefinition )
		{
			editor.showPopupInfo("No calls from this function were found");
			return;
		}
		
		getSwank().sendGetCallers(symbol, getPackage(), new SwankRunnable() {
			public void run() {
				LispNode guts = result.getf(":return").getf(":ok");
				
				ArrayList<String> optionNames = new ArrayList<String>(guts.params.size());
				ArrayList<LispNode> optionData = new ArrayList<LispNode>(guts.params.size());
				ArrayList<String> tips = new ArrayList<String>(guts.params.size());
				for (LispNode gut: guts.params) {
					if ( gut.params.size() == 2 && gut.params.get(1).get(0).value.equalsIgnoreCase(":location") ){
						String name = gut.get(0).value;
						String tip = gut.getf(":location").getf(":file").value;
						optionNames.add(name);
						optionData.add(gut);
						tips.add(tip);				
					} else {
						for (int i = 1; i<gut.params.size(); ++i) {
							LispNode possibility = gut.params.get(i);
							String name = possibility.get(0).value;
							String tip = possibility.getf(":location").getf(":file").value;
							optionNames.add(name);
							optionData.add(possibility);
							tips.add(tip);
						}						
					}
				}
				
				LispNode chosen;
				
				if (optionNames.size() <= 0) {
					editor.showPopupInfo("No calls to this function were found");
					return;
				} else if (optionNames.size() == 1) {
					chosen = optionData.get(0);
				} else {
					ListDialog<LispNode> win = new ListDialog<LispNode>(editor.getSite().getShell(), optionNames, optionData, tips);
					win.create();
					win.setTitle("Callers");
					if (win.open() == Dialog.OK) {
						chosen = win.getData();
					} else {
						return;
					}
				}
				
				LispNode location = chosen.getf(":location");
				String path = location.getf(":file").value;
				LispNode positionNode = location.getf(":position");
				LispNode snippetNode = location.getf(":snippet");
				
				if( positionNode.value.equals("") ){
					for( LispNode x : chosen.params ){
						positionNode = x.getf(":position");
						if( !positionNode.value.equals("") ){
							break;
						}
					}
				}
				
				if( snippetNode.value.equals("") ){
					for( LispNode x : chosen.params ){
						snippetNode = x.getf(":snippet");
						if( !snippetNode.value.equals("") ){
							break;
						}
					}
				}
				
				int position = positionNode.asInt();
				String snippet = snippetNode.value;
				
				LispEditor.jumpToDefinition(path, position, snippet);
			}
		
		});
	}

}
