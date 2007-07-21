package jasko.tim.lisp.editors.outline;

import jasko.tim.lisp.*;
import jasko.tim.lisp.editors.*;
import jasko.tim.lisp.preferences.PreferenceConstants;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.util.*;
import jasko.tim.lisp.util.TopLevelItemSort.Sort;

import java.util.*;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.viewers.*;
import org.eclipse.jface.action.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.views.contentoutline.*;

/**
 * TODO: Make this not clear out and repopulate every time we save the document.
 *  A clever man could get it to only remove those items which were removed and add only those which we added.
 *  A <i>really</i> clever man could get it to update <i>as the user types</i>.
 *  Clever would be more than enough.
 * @author Tim Jasko
 *
 */
public class LispOutlinePage extends ContentOutlinePage implements MouseListener, KeyListener {
	
	Sort sort = Sort.Position;
	IAction sortType;
	IAction sortAlpha;
	IAction sortPosition;
	
	LispEditor editor;
	
	private ArrayList<TopLevelItem> items = new ArrayList<TopLevelItem>();
	private HashMap<TopLevelItem,Position> itemPos = 
		new HashMap<TopLevelItem,Position>();
	
	public LispOutlinePage(LispEditor editor) {
		this.editor = editor;
		sort = getDefaultSort();
	}
	
	private Sort getDefaultSort() {
		IPreferenceStore prefs = LispPlugin.getDefault().getPreferenceStore();
		int sortInt = prefs.getInt(PreferenceConstants.OUTLINE_SORT);
		switch (sortInt) {
		case 0:
			return Sort.Alpha;
		case 1:
			return Sort.Position;
		case 2:
			return Sort.Type;
		default:
			return Sort.Alpha;	
		}
	}
	
	private void setDefaultSort() {
		IPreferenceStore prefs = LispPlugin.getDefault().getPreferenceStore();
		//System.out.println("######" + sort);
		int sortInt = 0;
		switch(sort) {
		case Alpha:
			sortInt = 0;
			break;
		case Position:
			sortInt = 1;
			break;
		case Type:
			sortInt = 2;
			break;
		}
		
		prefs.setValue(PreferenceConstants.OUTLINE_SORT, sortInt);
		
	}
	
	public void makeContributions(IMenuManager menuMgr,
         IToolBarManager toolBarMgr,
         IStatusLineManager statusLineMgr) {
		
		sortAlpha = new Action("Sort by name") {
			public void run() {
				sort = Sort.Alpha;
				setDefaultSort();
				this.setChecked(true);
				sortType.setChecked(false);
				sortPosition.setChecked(false);
				sortItems();
				redoTree();
			}
		};
		sortAlpha.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.SORT_ALPHA));
		if (sort == Sort.Alpha) {
			sortAlpha.setChecked(true);
		} else {
			sortAlpha.setChecked(false);
		}
		sortAlpha.setToolTipText("Sort by name");
		
		
		sortType = new Action("Sort by type") {
			public void run() {
				sort = Sort.Type;
				setDefaultSort();
				this.setChecked(true);
				sortAlpha.setChecked(false);
				sortPosition.setChecked(false);
				sortItems();
				redoTree();
			}
		};
		sortType.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.SORT_TYPE));
		if (sort == Sort.Type) {
			sortType.setChecked(true);
		} else {
			sortType.setChecked(false);
		}
		sortType.setToolTipText("Sort by type");
		
		sortPosition = new Action("Sort by position") {
			public void run() {
				sort = Sort.Position;
				setDefaultSort();
				this.setChecked(true);
				sortType.setChecked(false);
				sortAlpha.setChecked(false);
				sortItems();
				redoTree();
			}
		};
		sortPosition.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.SORT_POSITION));
		if (sort == Sort.Position) {
			sortPosition.setChecked(true);
		} else {
			sortPosition.setChecked(false);
		}
		sortPosition.setToolTipText("Sort by position");

		toolBarMgr.add(sortPosition);
		toolBarMgr.add(sortAlpha);
		toolBarMgr.add(sortType);
	}
	
	public void createControl(Composite parent) {
		super.createControl(parent);
		
		IDocument doc = editor.getDocumentProvider().getDocument(
				  editor.getEditorInput());
		LispNode file = LispParser.parse(doc.get() + "\n)");
		
		getTreeViewer().getControl().addMouseListener(this);
		getTreeViewer().getControl().addKeyListener(this);
		
		fillTree(file);
	}
	
	private void fillTree(LispNode file) {
		items = LispUtil.getTopLevelItems(file,"");
		itemPos.clear();
		editor.clearOutlinePositions();
		for( TopLevelItem item : items ){
			Position pos = new Position(item.offset);
			editor.addOutlinePosition(pos);
			itemPos.put(item, pos);
		}
		sortItems();
		redoTree();
	}
	
	private void sortItems() {
		TopLevelItemSort sorter = new TopLevelItemSort();
		sorter.sortItems(items, sort);
	}
	
	private void redoTree() {
		
		getControl().setRedraw(false);
		Tree tree = getTreeViewer().getTree();
		tree.removeAll();
		String currType = "()"; //impossible type
		TreeItem category = null;
		for (TopLevelItem item: items) {
			TreeItem temp;
			if (sort == Sort.Alpha) {
				temp = new TreeItem(tree, SWT.NONE);
			} else if ( sort == Sort.Position ) {
				if(category == null) {
					temp = new TreeItem(tree, SWT.NONE);
					if ( item.type.equals("section") ) {
						category = temp;
					}
				} else {
					if ( item.type.equals("section")){
						temp = new TreeItem(tree, SWT.NONE);
						category = temp;
					} else {
						temp = new TreeItem(category, SWT.NONE);
					}
				}
			} else { // sort by type
				if (!item.type.equals(currType)) {
					currType = item.type;
					category = new TreeItem(tree, SWT.NONE);
					category.setText(currType);
					category.setImage(LispImages.getImageForType(currType));
					category.setData("");
				}
				temp = new TreeItem(category, SWT.NONE);
			}
			
			temp.setImage(LispImages.getImageForType(item.type));
			temp.setText(item.name);
			temp.setData(item);
			
		}
		
		getControl().setRedraw(true);
	}
	
	
	private TopLevelItem lastSelection;
	
	public void selectionChanged(SelectionChangedEvent event) {
		try {
			IStructuredSelection sel = (IStructuredSelection) event.getSelection();
			
			if (! sel.isEmpty()) {
				if (sel.getFirstElement() instanceof TopLevelItem) {
					TopLevelItem item = (TopLevelItem) sel.getFirstElement();
					if (item != lastSelection) {
						lastSelection = item;
						if ( item.type.equals("section") ) {
 							editor.selectAndReveal(itemPos.get(item).offset, item.name.length());							
 						} else {
 							editor.selectAndReveal(itemPos.get(item).offset, item.type.length() + 1);							
 						}
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	} // void selectionChanged( ... )
	
	public void mouseDown(MouseEvent e) {
		IStructuredSelection sel = (IStructuredSelection) getTreeViewer().getSelection();
		if (! sel.isEmpty()) {
			if (sel.getFirstElement() instanceof TopLevelItem) {
				TopLevelItem item = (TopLevelItem) sel.getFirstElement();
				lastSelection = item;
				int pos = itemPos.get(item).offset;
				if ( item.type.equals("section") ) {
					editor.selectAndReveal(pos, item.name.length());							
				} else {
					editor.selectAndReveal(pos, item.type.length() + 1);							
				}
			}
		}
	}
	
	String search = "";
	
	private boolean isSearchable(char c) {
		if ("1234567890qwertyuiopasdfghjklzxcvbnm!@#$%^&*()_-=+{}|[]\\:;\"\'<>?,./`~".indexOf(
				Character.toLowerCase(c)) >= 0) {
			return true;
		} else {
			return false;
		}
	}
	
	public void keyPressed(KeyEvent e) {
		System.out.println(search);
		if (e.keyCode == SWT.ESC) {
			search = "";
		} else if (e.character == SWT.BS) {
			search = search.substring(0, search.length() - 1);
		} else if (isSearchable(e.character)) {
			search += e.character;
			
			for (TreeItem node: getTreeViewer().getTree().getItems()) {
				if (node.getText().startsWith(search)) {
					getTreeViewer().getTree().setSelection(node);
					if (node.getData() instanceof TopLevelItem) {
						TopLevelItem item = (TopLevelItem) node.getData();
						lastSelection = item;
						if ( item.type.equals("section") ) {
 							editor.selectAndReveal(itemPos.get(item).offset, item.name.length());							
 						} else {
 							editor.selectAndReveal(itemPos.get(item).offset, item.type.length() + 1);							
 						}
					}
					return;
				}
			}
			this.getSite().getShell().getDisplay().beep();
		}
	}
	
	public void setFocus() {
		search = "";
	}
	
	
	/**
	 * Updates the content view to reflect changes in the file.
	 */
	public void update(LispNode file) {
		fillTree(file);
	} // void update()
	
	public void mouseDoubleClick(MouseEvent e) {
		// meh
	}

	public void mouseUp(MouseEvent e) {
		// also meh
	}

	public void keyReleased(KeyEvent e) {
		// powers of meh combine!
	}
	

}
