package jasko.tim.lisp.editors.outline;

import jasko.tim.lisp.*;
import jasko.tim.lisp.editors.*;
import jasko.tim.lisp.preferences.PreferenceConstants;
import jasko.tim.lisp.swank.*;
import jasko.tim.lisp.util.*;
import jasko.tim.lisp.util.TopLevelItemSort.Sort;

import java.util.*;

import jasko.tim.lisp.editors.assist.*;

import org.eclipse.swt.graphics.*;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.jface.action.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.views.contentoutline.*;

public class LispOutlinePage extends ContentOutlinePage 
	implements MouseListener, KeyListener, MouseTrackListener{
	
	Sort sort = Sort.Position;
	IAction sortType;
	IAction sortAlpha;
	IAction sortPosition;
	
	LispEditor editor;
	LispTextHoverControlCreator tooltipCreator;
	IInformationControl tooltip;
	
	private ArrayList<TopLevelItem> items = new ArrayList<TopLevelItem>();
	private HashMap<TopLevelItem,Position> itemPos = 
		new HashMap<TopLevelItem,Position>();
	private HashMap<TopLevelItem,TreeItem> itemTr = 
		new HashMap<TopLevelItem,TreeItem>();
	
	public LispOutlinePage(LispEditor editor) {
		this.editor = editor;
		sort = getDefaultSort();
	}

	private void updateTree(){
		// get changed offsets
		ArrayList<Integer> changedTopLevelPos;
		IDocument doc = editor.getDocument();
		if( null == doc ){
			return;
		}
		
		Position[] changedPos = editor.getAndClearChangedPosForOutline();
		changedTopLevelPos = new ArrayList<Integer>();
		int cachedOffset = -1;
		for(Position pos : changedPos ){
			if( !pos.isDeleted ){
				Integer offset = new Integer(LispUtil
						.getTopLevelOffset(doc, 
								Math.min(pos.offset,doc.getLength()-1), true));
				if( offset.intValue() != cachedOffset 
						&& !changedTopLevelPos.contains(offset) ){
					cachedOffset = offset.intValue();
					changedTopLevelPos.add(offset);
				}
				for( int i = 0; i < pos.length; ++i ){
					offset = new Integer(LispUtil
							.getTopLevelOffset(doc, 
									Math.min(pos.offset+i,doc.getLength()-1),
											true));
					if( offset.intValue() != cachedOffset 
							&& !changedTopLevelPos.contains(offset) ){
						cachedOffset = offset.intValue();
						changedTopLevelPos.add(offset);
					}			
				}
			}
		}
		// split these to added and modified
		// first put all old positions as candidates for modified
		ArrayList<Integer> modifiedPos = new ArrayList<Integer>(itemPos.size());
		for(Position pos : itemPos.values()){
			modifiedPos.add(new Integer(pos.offset-1));
		}
		// possibly modified positions
		modifiedPos.retainAll(changedTopLevelPos);
		// added positions
		changedTopLevelPos.removeAll(modifiedPos);
		
		//run over elements in tree, update offsets of items,
		//and modify name and type if necessary
		//also remove cached tooltips
		//also resort if by alpha or type
		for( TopLevelItem item : itemPos.keySet()){
			Position pos = itemPos.get(item);
			if( item.offset != pos.offset-1){
				item.offset = pos.offset-1;
				item.info = "";
			}
			if( modifiedPos.contains(new Integer(item.offset))){
				TopLevelItem itm = null; //"throw away" item
				try{
					if( doc.getChar(item.offset) == ';'){ //section
						String val = doc.get(item.offset,
								doc.getLineLength(doc
										.getLineOfOffset(item.offset)));
						itm = LispUtil.getSectionItem(
								new LispComment(val, item.offset,
										item.offset + val.length()),
								item.offset);
					} else { //sexp
						LispNode exp = LispParser.parse(LispUtil
								.getTopLevelExpression(doc, 
										Math.min(item.offset + 1,
												doc.getLength()-1))).get(0);
						itm = LispUtil.getTopLevelItem(exp, "", item.offset);
					}
				} catch ( BadLocationException e ){
					e.printStackTrace();
				}
				if( itm != null ){
					item.offset = itm.offset;
					item.offsetEnd = itm.offsetEnd;
					item.info = "";
					TreeItem tr = itemTr.get(item);
					tr.setImage(CuspResources.getImageForType(itm.type));

					if( sort == Sort.Type && !item.type.equals(itm.type) ){
						TreeItem[] typeNodes = 
							getTreeViewer().getTree().getItems();
						int i, j;
						for( i = 0; i < typeNodes.length; ++i ){
							if( typeNodes[i].getText().equals(itm.type) ){
								break;
							}
						}
						for( j = 0; j < typeNodes.length; ++j ){
							if( typeNodes[j].getText().equals(item.type) ){
								break;
							}
						}
						TreeItem typeNode;
						TreeItem oldtypeNode = typeNodes[j];
						if( i >= typeNodes.length ){ //create new category
							typeNode = 
								new TreeItem(getTreeViewer().getTree(),SWT.NONE);
							typeNode.setText(itm.type);
							typeNode.setImage(CuspResources
									.getImageForType(itm.type));
						} else {
							typeNode = typeNodes[i];
						}
						TreeItem tmp = 
							new TreeItem(typeNode,SWT.NONE,
									getIndex(item.offset,
											typeNode.getItems()));
						tr.setText(itm.name);
						copyItem(tr,tmp);
						itemTr.put(item, tmp);
						tr.dispose();
						if( oldtypeNode.getItemCount() < 1 ){ //remove this category
							oldtypeNode.dispose();
						}
					} else if( sort == Sort.Alpha 
							&& !item.name.equals(itm.name)){
						TreeItem[] nodes = 
							getTreeViewer().getTree().getItems();
						int i;
						for( i = 0; i < nodes.length; ++i ){
							if( nodes[i].getText()
									.compareToIgnoreCase(itm.name) >= 0 ){
								break;
							}
						}
						TreeItem tmp = 
							new TreeItem(getTreeViewer().getTree(),SWT.NONE,i);
						tr.setText(itm.name);
						copyItem(tr,tmp);
						itemTr.put(item, tmp);
						tr.dispose();
					} else {
						tr.setText(itm.name);
					}
					item.name = itm.name;
					item.type = itm.type;						
				}
			}
		}

		// get new items to add
		ArrayList<TopLevelItem> newItems = 
			new ArrayList<TopLevelItem>(changedTopLevelPos.size());

		for( Integer offs : changedTopLevelPos){
			TopLevelItem itm = null;
			int offset = offs.intValue();
			try{
				if( doc.getChar(offset) == ';'){ //section
					String val = doc.get(offset, 
							doc.getLineLength(doc.getLineOfOffset(offset)));
					itm = LispUtil.getSectionItem(
							new LispComment(val, offset, offset + val.length()),
							offset);
				} else { //sexp
					LispNode exp = LispParser.parse(LispUtil
							.getTopLevelExpression(doc, offset + 1)).get(0);
					itm = LispUtil.getTopLevelItem(exp, 
							LispUtil.getPackage(doc.get(), offset), offset);
				}
				newItems.add(itm);
				Position pos = new Position(itm.offset+1);
				editor.addOutlinePosition(pos);
				itemPos.put(itm, pos);
			} catch ( BadLocationException e ){
				e.printStackTrace();
			}
		}
		
		// finally refresh tree adding new items
		// first sort new items
		TopLevelItemSort sorter = new TopLevelItemSort();
		sorter.sortItems(newItems, sort);
		
		if( sort == Sort.Position ){
			Tree tree = getTreeViewer().getTree();
			for( TopLevelItem item : newItems){
				TreeItem[] roots = getTreeViewer().getTree().getItems();
				int i = getIndex(item.offset,roots);
				TreeItem tmp = null;
				if( i > 0 && ((TopLevelItem)(roots[i-1].getData())).type
						.equals("section") ){
					if( item.type.equals("section") ){
						//move necessary items from previous section to this
						tmp = new TreeItem(tree,SWT.NONE,i);
						TreeItem[] kids = roots[i-1].getItems();
						int start = getIndex(item.offset,kids);
						for( int j = start; j < kids.length; ++j ){
							TreeItem tmpkid = new TreeItem(tmp,SWT.NONE);
							copyItem(kids[j],tmpkid);
							kids[j].dispose();
						}
					} else {
						tmp = new TreeItem(roots[i-1],
								SWT.NONE,getIndex(item.offset, 
										roots[i-1].getItems()));						
					}
				} else {
					tmp = new TreeItem(tree,SWT.NONE,i);						
					if( item.type.equals("section") ){
						//move necessary items from tree to this
						for( int j = i; j < roots.length; ++j ){
							if( ((TopLevelItem)(roots[j].getData())).type
									.equals("section") ){
								break;
							} else {
								TreeItem tmpkid = new TreeItem(tmp,SWT.NONE);
								copyItem(roots[j],tmpkid);
								roots[j].dispose();								
							}
						}						
					}
				}
				tmp.setImage(CuspResources.getImageForType(item.type));
				tmp.setText(item.name);
				tmp.setData(item);
				itemTr.put(item, tmp);
			}
		} else if ( sort == Sort.Type ) {
			for( TopLevelItem item : newItems){
				TreeItem[] typeNodes = getTreeViewer().getTree().getItems(); //types
				//find type
				int i;
				for( i = 0; i < typeNodes.length; ++i ){
					if( typeNodes[i].getText().equals(item.type) ){
						break;
					}
				}
				TreeItem typeNode;
				if( i >= typeNodes.length ){ //create new category
					typeNode = 
						new TreeItem(getTreeViewer().getTree(),SWT.NONE);
					typeNode.setText(item.type);
					typeNode.setImage(CuspResources
							.getImageForType(item.type));
				} else {
					typeNode = typeNodes[i];
				}
				TreeItem tmp = 
					new TreeItem(typeNode,SWT.NONE,
							getIndex(item.offset,
									typeNode.getItems()));
				tmp.setImage(CuspResources.getImageForType(item.type));
				tmp.setText(item.name);
				tmp.setData(item);
				itemTr.put(item, tmp);
			}
			// remove top level items without kids
			for( TreeItem node : getTreeViewer().getTree().getItems()){
				if(0 == node.getItemCount()){
					node.dispose();
				}
			}
		} else { // sort is Alpha
			for( TopLevelItem item : newItems){
				TreeItem[] nodes = getTreeViewer().getTree().getItems(); //types
				int i;
				for( i = 0; i < nodes.length; ++i ){
					if( nodes[i].getText().compareToIgnoreCase(item.name) >= 0 ){
						break;
					}
				}
				TreeItem tmp = 
					new TreeItem(getTreeViewer().getTree(),SWT.NONE,i);
				tmp.setImage(CuspResources.getImageForType(item.type));
				tmp.setText(item.name);
				tmp.setData(item);
				itemTr.put(item, tmp);
			}
		}
		//finally update items array
		items.clear();
		items.addAll(itemPos.keySet());
	}

	// get index in items where offset is located, when sorted by offsets
	private int getIndex(int offset, TreeItem[] items){
		if(items == null || items.length == 0 ){
			return 0;
		}
		TopLevelItem item0 = (TopLevelItem)(items[0].getData());
		int i = 0;
		if( offset > item0.offset ){
			for( i = 1; i < items.length; ++i ){
				TopLevelItem item1 = (TopLevelItem)(items[i].getData());
				if( offset > item0.offset 
						&& offset < item1.offset ){
					break;
				}
				item0 = item1;
			}
		}
		
		return i;
	}
	
	// === dealing with the tree - they could have made it easier!
	
	//copies most data "from" "to"
	private void copyItem(TreeItem from, TreeItem to){
		if( from == null || to == null ){
			return;
		}
		to.setBackground(from.getBackground());
		to.setChecked(from.getChecked());
		to.setData(from.getData());
		to.setFont(from.getFont());
		to.setForeground(from.getForeground());
		to.setGrayed(from.getGrayed());
		to.setImage(from.getImage());
		to.setText(from.getText());
	}
	
	// move all kids "from" "to" putting them at the end of "to" 
	// (don't move disposed and their kids)
	// also update itemTr
	private void moveKids(TreeItem from, TreeItem to){
		if( from == null || from.getItemCount() == 0){
			return;
		}
		if( to != null ){
			for( TreeItem item: from.getItems() ){
				if( !item.isDisposed() ){
					TreeItem tmp = new TreeItem(to,item.getStyle());
					copyItem(item,tmp);
					itemTr.put((TopLevelItem)(item.getData()), tmp);
					if(item.getItemCount() > 0){
						moveKids(item,tmp);
					}					
				}
			}			
		} else { // if to == null move to front of the tree 
			//(before first element with kids)
			//find index of first non disposed element with kids
			Tree tree = getTreeViewer().getTree();
			TreeItem[] items = tree.getItems();
			int i = 0;
			for( i = 0; i < items.length; ++i ){
				if(items[i].getItemCount() > 0 && !items[i].isDisposed()){
					break;
				}
			}
			for( TreeItem item: from.getItems() ){
				if( !item.isDisposed() ){
					TreeItem tmp = new TreeItem(tree,item.getStyle(),i);
					copyItem(item,tmp);
					itemTr.put((TopLevelItem)(item.getData()), tmp);
					++i;
					if(item.getItemCount() > 0 ){
						moveKids(item,tmp);
					}					
				}
			}
		}
	}
	
	private boolean isDeletedTopLevel(TopLevelItem item){
		if( item == null ){
			return true;
		}
		Position pos = itemPos.get(item);
		if( pos == null || pos.isDeleted ){
			return true;
		}
		int offset =
			LispUtil.getTopLevelOffset(editor.getDocument(), pos.offset,true); 
		if( offset != pos.offset-1 ){
			return true;
		}
		return false;
	}
	
//works on tree up to 2 levels deep (exactly what we use for LispOutlinePage)
	private void removeDeletedItems(){
		Tree tree = getTreeViewer().getTree();
		TreeItem[] items = tree.getItems();
		for( int i = 0; i < items.length; ++i ){
			TreeItem titem = items[i];
			if( !titem.isDisposed() ){
				if( sort != Sort.Type ){
					TopLevelItem item = (TopLevelItem)titem.getData();
					
					//- sections) or alpha
					// first remove deleted kids (they in turn do not have kids)
					for( TreeItem itm : titem.getItems() ){
						TopLevelItem tmp = (TopLevelItem)itm.getData();
						if( tmp != null && isDeletedTopLevel(tmp)){
							itemPos.remove(tmp);
							itemTr.remove(tmp);
							itm.dispose();
						}
					}
					// now delete items and move kids
					if(isDeletedTopLevel(item)){ //remove item
						if( titem.getItemCount() > 0 ){
							if(i == 0){
								moveKids(titem,null);
							} else if ( items[i-1].getItemCount() > 0 ){
								moveKids(titem,items[i-1]);
							} else {
								moveKids(titem,null);
							}
						}
						itemPos.remove(item);
						itemTr.remove(item);
						titem.dispose();
					}				
				} else {
					for( TreeItem itm : titem.getItems() ){
						TopLevelItem tmp = (TopLevelItem)itm.getData();
						if( tmp != null && isDeletedTopLevel(tmp)){
							itemPos.remove(tmp);
							itemTr.remove(tmp);
							itm.dispose();
						}
					}					
				}
			}
		}
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
	
	public void updateOutline(){
		removeDeletedItems();
		updateTree();		
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
				CuspResources.getImageDescriptor(CuspResources.SORT_ALPHA));
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
				CuspResources.getImageDescriptor(CuspResources.SORT_TYPE));
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
				CuspResources.getImageDescriptor(CuspResources.SORT_POSITION));
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
		
		if( null == doc ){
			return;
		}

		Tree tree = getTreeViewer().getTree();
		
		getTreeViewer().getControl().addMouseListener(this);
		getTreeViewer().getControl().addKeyListener(this);		
		tree.addMouseTrackListener(this);
		
		tooltipCreator = new LispTextHoverControlCreator();
		tooltip = tooltipCreator.createInformationControl(tree.getShell());
		
		LispNode file = LispParser.parse(doc.get() + "\n)");
		fillTree(file);
	}

	
	private void fillTree(LispNode file) {
		items = LispUtil.getTopLevelItems(file,"");
		itemPos.clear();
		editor.clearOutlinePositions();
		for( TopLevelItem item : items ){
			Position pos = new Position(item.offset+1);
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
		itemTr.clear();
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
					category.setImage(CuspResources.getImageForType(currType));
					category.setData("");
				}
				temp = new TreeItem(category, SWT.NONE);
			}
			
			temp.setImage(CuspResources.getImageForType(item.type));
			temp.setText(item.name);
			temp.setData(item);
			itemTr.put(item, temp);
			
		}
		
		getControl().setRedraw(true);
	}	
	
	private TopLevelItem lastSelection;
	
	public void selectionChanged(SelectionChangedEvent event) {
		try {
			IStructuredSelection sel = 
				(IStructuredSelection) event.getSelection();
			
			if (! sel.isEmpty()) {
				if (sel.getFirstElement() instanceof TopLevelItem) {
					TopLevelItem item = (TopLevelItem) sel.getFirstElement();
					if (item != lastSelection) {
						lastSelection = item;
						editor.selectAndReveal(itemPos.get(item).offset-1,
								item.type.length() + 1);
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	} // void selectionChanged( ... )
	
	public void mouseDown(MouseEvent e) {
		IStructuredSelection sel = 
			(IStructuredSelection) getTreeViewer().getSelection();
		if (! sel.isEmpty()) {
			if (sel.getFirstElement() instanceof TopLevelItem) {
				TopLevelItem item = (TopLevelItem) sel.getFirstElement();
				lastSelection = item;
				int pos = itemPos.get(item).offset-1;
				editor.selectAndReveal(pos, item.type.length() + 1);							
			}
		}
	}
	
	String search = "";
	
	private boolean isSearchable(char c) {
		if ("1234567890qwertyuiopasdfghjklzxcvbnm!@#$%^&*()_-=+{}|[]\\:;\"\'<>?,./`~"
				.indexOf(Character.toLowerCase(c)) >= 0) {
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
						editor.selectAndReveal(itemPos.get(item).offset-1, 
								item.type.length() + 1);
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

	public void mouseEnter(MouseEvent e){
	}
	
	// Still allows the user to escape the outline by moving over the tooltip
	public void mouseExit(MouseEvent e){
		//Tree tree = getTreeViewer().getTree();
		//Rectangle rt = tree.getClientArea();
		//if( e.x <= rt.x || e.y <= rt.y 
		//		|| e.x >= rt.x + rt.width || e.y >=rt.y + rt.height ){
			tooltip.setVisible(false);
			tooltipItem = null;
		//}
	}
	
	//show tooltip
	private TreeItem tooltipItem = null;
	
	public void mouseHover(MouseEvent e){
		IPreferenceStore prefs = LispPlugin.getDefault().getPreferenceStore();
		Boolean showToolTip = 
			prefs.getBoolean(PreferenceConstants.SHOW_OUTLINE_HINT);
		
		if( showToolTip ){
			Point pt = new Point(e.x,e.y);
			final Tree tree = getTreeViewer().getTree();
			TreeItem item = tree.getItem(pt);
			
			if( item != null && item != tooltipItem && item.getData() != null ){
				final Point ptHint = 
					new Point(e.x,item.getBounds().y 
							+ (int)Math.round(1.5*item.getBounds().height));
				tooltipItem = item;
				final TopLevelItem tr = (TopLevelItem)(item.getData());
				if( !tr.type.equals("section") ){
					Position pos = itemPos.get(tr);
					if( pos != null ){
						if( tr.info == null || tr.info.equals("") || tr.info.equals("nil") ){ //not cached
							final String variable = tr.name;
							final SwankInterface swank = LispPlugin.getDefault().getSwank();
							swank.sendGetArglist(variable, tr.pkg, new SwankRunnable() {
								public void run() {										
									final String args = getReturn().value;
									swank.sendGetDocumentation(variable, tr.pkg, new SwankRunnable() {
										public void run() {
											String docstr = getReturn().value;
											tr.info = args;
											if( tr.info.equalsIgnoreCase("nil") ){
												tr.info = "";
											}
											if(docstr != null && !docstr.equals("") 
													&& !docstr.equals("nil")){
												if( tr.info.equals("") ){
													tr.info = docstr;
												} else {
													tr.info += "\n"+docstr;										
												}
											}
											if( tr.info != null && !tr.info.equals("") 
													&& !tr.info.equals("nil") ){
												tooltip.dispose();
												tooltip = tooltipCreator.createInformationControl(tree.getShell());
												tooltip.setInformation(tr.info);
												Point size = tooltip.computeSizeHint();
												tooltip.setSize(size.x, size.y);
												tooltip.setLocation(tree.toDisplay(ptHint));
												tooltip.setVisible(true);
											} else {
												tooltip.setVisible(false);
											}
										}
									});
								}
							});				
						} else { //cached
							tooltip.dispose();
							tooltip = tooltipCreator.createInformationControl(tree.getShell());
							tooltip.setInformation(tr.info);
							Point size = tooltip.computeSizeHint();
							tooltip.setSize(size.x, size.y);
							tooltip.setLocation(tree.toDisplay(ptHint));
							tooltip.setVisible(true);
						}
					} else {
						tooltip.setVisible(false);
					}
				} else {
					tooltip.setLocation(tree.toDisplay(ptHint));
				}
			}
		}
	}
}
