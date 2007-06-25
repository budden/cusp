package jasko.tim.lisp.editors.outline;

import jasko.tim.lisp.*;
import jasko.tim.lisp.editors.*;
import jasko.tim.lisp.swank.*;

import java.util.*;

import org.eclipse.jface.text.*;
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
	private enum Sort {
		Position,
		Alpha,
		Type
	}
	Sort sort = Sort.Position;
	IAction sortType;
	IAction sortAlpha;
	IAction sortPosition;
	
	LispEditor editor;
	private ArrayList<OutlineItem> items = new ArrayList<OutlineItem>();
	
	
	public LispOutlinePage(LispEditor editor) {
		this.editor = editor;
	}
	
	public void makeContributions(IMenuManager menuMgr,
         IToolBarManager toolBarMgr,
         IStatusLineManager statusLineMgr) {
		
		sortAlpha = new Action("Sort by name") {
			public void run() {
				sort = Sort.Alpha;
				this.setChecked(true);
				sortType.setChecked(false);
				sortPosition.setChecked(false);
				sortItems();
				redoTree();
			}
		};
		sortAlpha.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.SORT_ALPHA));
		sortAlpha.setChecked(false);
		sortAlpha.setToolTipText("Sort by name");
		
		
		sortType = new Action("Sort by type") {
			public void run() {
				sort = Sort.Type;
				this.setChecked(true);
				sortAlpha.setChecked(false);
				sortPosition.setChecked(false);
				sortItems();
				redoTree();
			}
		};
		sortType.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.SORT_TYPE));
		sortType.setChecked(false);
		sortType.setToolTipText("Sort by type");
		
		sortPosition = new Action("Sort by position") {
			public void run() {
				sort = Sort.Position;
				this.setChecked(true);
				sortType.setChecked(false);
				sortAlpha.setChecked(false);
				sortItems();
				redoTree();
			}
		};
		sortPosition.setImageDescriptor(
				LispImages.getImageDescriptor(LispImages.SORT_POSITION));
		sortPosition.setChecked(true);
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
		items = new ArrayList<OutlineItem>(file.params.size());
		for (LispNode exp: file.params) {
			//System.out.println(exp);
			OutlineItem item = new OutlineItem();
			
			item.type = exp.get(0).value.toLowerCase();
			item.name = exp.get(1).toLisp();
			item.offset = exp.offset;
			if (! item.type.startsWith("def")) {
				item.name = item.type;
				if (item.type.equalsIgnoreCase("in-package")) {
					item.name = "in-package " + exp.get(1).toLisp(); 
				}
			} else if (item.type.equalsIgnoreCase("defstruct")) {
				LispNode name = exp.get(1); 
				if (!name.value.equals("")) {
					item.name = name.value;
				} else {
					item.name = name.get(0).value;
				}
			} else if (item.type.equalsIgnoreCase("defmethod")) {
				String name = exp.get(2).toLisp();
				if (name.startsWith(":")) {
					item.name += " " + name + " " + exp.get(3).toLisp();
				} else {
					item.name += " " + name;
				}
			}
			
			if (item.name.equals("")) {
				if (exp.params.size() >= 2) {
					if (exp.get(1).toLisp().startsWith(":")) {
						item.name = exp.get(1).toLisp() + " " + exp.get(2).toLisp();
					} else {
						item.name = exp.get(1).toLisp();
					}
				}
			}
			
			if (! item.name.equals("")) {
				items.add(item);
			}
		}
		
		// add section comments
		for ( LispComment comment: file.comments ) {
			if ( comment.isSectionComment() ) {
				OutlineItem item = new OutlineItem();
				
				item.type = "section";
				item.name = comment.SectionName();
				item.offset = comment.offset + LispComment.SECTION_START.length();
				if (! item.name.equals("")) {
					items.add(item);
				}
			}
		}
		
		sortItems();
		
		redoTree();
	}
	
	private void sortItems() {
		if (sort == Sort.Alpha) {
			Collections.<OutlineItem>sort(items);
		} else if (sort == Sort.Type) {
			Collections.<OutlineItem>sort(items, new TypeComparator());
		} else if (sort == Sort.Position) {
			Collections.<OutlineItem>sort(items, new PositionComparator());
		} else { // Really shouldn't ever happen
			Collections.<OutlineItem>sort(items);
		}
	}
	
	private void redoTree() {
		
		getControl().setRedraw(false);
		Tree tree = getTreeViewer().getTree();
		tree.removeAll();
		String currType = "()"; //impossible type
		TreeItem category = null;
		for (OutlineItem item: items) {
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
	
	
	private OutlineItem lastSelection;
	
	public void selectionChanged(SelectionChangedEvent event) {
		try {
			IStructuredSelection sel = (IStructuredSelection) event.getSelection();
			
			if (! sel.isEmpty()) {
				if (sel.getFirstElement() instanceof OutlineItem) {
					OutlineItem item = (OutlineItem) sel.getFirstElement();
					if (item != lastSelection) {
						lastSelection = item;
						if ( item.type.equals("section") ) {
 							editor.selectAndReveal(item.offset, item.name.length());							
 						} else {
 							editor.selectAndReveal(item.offset, item.type.length() + 1);							
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
			if (sel.getFirstElement() instanceof OutlineItem) {
				OutlineItem item = (OutlineItem) sel.getFirstElement();
				lastSelection = item;
				if ( item.type.equals("section") ) {
					editor.selectAndReveal(item.offset, item.name.length());							
				} else {
					editor.selectAndReveal(item.offset, item.type.length() + 1);							
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
					if (node.getData() instanceof OutlineItem) {
						OutlineItem item = (OutlineItem) node.getData();
						lastSelection = item;
						if ( item.type.equals("section") ) {
 							editor.selectAndReveal(item.offset, item.name.length());							
 						} else {
 							editor.selectAndReveal(item.offset, item.type.length() + 1);							
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
	
	
	
	private class OutlineItem implements Comparable<OutlineItem> {
		public String name;
		public int offset;
		public String type;
		
		public int compareTo(OutlineItem o) {
			return name.toLowerCase().compareTo( 
				o.name.toLowerCase() );
		}
	}
	
	private class TypeComparator implements Comparator<OutlineItem> {
		public int compare(OutlineItem arg0, OutlineItem arg1) {
			return arg0.type.toLowerCase().compareTo(arg1.type.toLowerCase());
		}
	}
	
	private class PositionComparator implements Comparator<OutlineItem> {
 		public int compare(OutlineItem arg0, OutlineItem arg1) {
 			return arg0.offset - arg1.offset;
 		}
 	} 		
	
	

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
