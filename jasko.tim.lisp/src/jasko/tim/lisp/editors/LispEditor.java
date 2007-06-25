package jasko.tim.lisp.editors;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;

import jasko.tim.lisp.ColorManager;
import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.ColorManager.ColorChangeEvent;
import jasko.tim.lisp.editors.actions.IndentAction;
import jasko.tim.lisp.editors.assist.LispInformationControlManager;
import jasko.tim.lisp.editors.outline.LispOutlinePage;
import jasko.tim.lisp.swank.LispNode;
import jasko.tim.lisp.swank.LispParser;
import jasko.tim.lisp.util.LispUtil;

import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.*;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.*;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

public class LispEditor extends TextEditor implements ILispEditor {
	private LispOutlinePage outline;
	private ColorManager.ChangeEventListener colorPrefChangeListener;
    private final LispConfiguration config = new LispConfiguration(this, LispPlugin.getDefault().getColorManager());
    
    private final CurrentExpressionHighlightingListener highlighter = new CurrentExpressionHighlightingListener();
    
    /**
     * Returns IFile associated with this editor
     * @return IFile or null
     */
    private IFile getIFile(){
    	IEditorInput input= getEditorInput();
    	IFile original= (input instanceof IFileEditorInput) ?
    	                  ((IFileEditorInput) input).getFile() : null;
    	return original;
    }
    
	public LispEditor() {
		super();
		setSourceViewerConfiguration(config);
		setDocumentProvider(new LispDocumentProvider());
		colorPrefChangeListener = new ColorManager.ChangeEventListener() {

			public void colorPreferenceChanged(ColorChangeEvent event) {
				
			}
			
		};
        
		//setRangeIndicator(new DefaultRangeIndicator());
	}
    
    public String showParameterHints () {
        return config.showParameterHints();
    }
    
    public String showContentCompletions () {
        return config.showContentCompletions();
    }

    public void callUrl(String url) {
    	//TODO: this code is almost duplicated in ReplView
		ITextSelection ts = (ITextSelection) getSelectionProvider().getSelection();
		int offset = ts.getOffset();
		IDocument doc = getDocumentProvider().getDocument(getEditorInput());
		
		String identifier = LispUtil.getCurrentFullWord(doc, offset);
		identifier = identifier.replace("'", "");
		identifier = identifier.replace("`", "");
		
		IWorkbenchBrowserSupport browser = LispPlugin.getDefault().getWorkbench().getBrowserSupport();
		try {
			browser.createBrowser("jasko.tim.lisp.lispdoc").openURL(new URL(
					url.replace("%s", identifier)));
		} catch (PartInitException e) {
			e.printStackTrace();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
    }

	/**
	 * Jumps the user to a given position in the given file.
	 * If an editor is not open for this file, one is opened.
	 * If the file is not in the workspace, a link is created to it.
	 * If the position is out of bound, we try to find the snippet.
	 * If we can't find that, at least they have the file open.
	 * @param filePath
	 * @param position
	 * @param snippet
	 */
	public static void jumpToDefinition(String filePath, int position, String snippet) {
		jumpToDefinition(filePath, position, snippet, null);
	}
	
	/**
	 * Jumps the user to a given position in the given file.
	 * If an editor is not open for this file, one is opened.
	 * If the file is not in the workspace, a link is created to it.
	 * If the position is out of bound, we try to find the snippet.
	 * If the snippet isn't found, we go to the first instance of symbol.
	 * If we can't find that, at least they have the file open.
	 * 
	 * Really, it is very unlikely that we'll ever use any of the fall-backs
	 * @param filePath
	 * @param position
	 * @param snippet
	 * @param symbol
	 */
	public static void jumpToDefinition(String filePath, int position, String snippet, String symbol) {
		System.out.println("*jump: " + filePath + ":" + position);
		IWorkbenchPage page =
			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IPath path = new Path(filePath);
		IFile[] files = root.findFilesForLocation(path);
		
		IEditorPart editor = null;
		
		if (files.length > 0) {
			try {
				editor = IDE.openEditor(page, files[0], true);
			} catch (PartInitException e) {
				e.printStackTrace();
			}
		} else {
			System.out.println("File not found:" + path);
			try {
				IProject project = root.getProject(".External Lisp Files");
				if (!project.exists()) {
					project.create(null);
				}
				if (!project.isOpen()) {
					project.open(null);
				}
				IFile file = project.getFile(path.lastSegment());
				file.createLink(path, IResource.NONE, null);
			
				editor = IDE.openEditor(page, file, true);
			} catch (CoreException ex) {
				ex.printStackTrace();
			}
		}
		
		if (editor != null) {
			//System.out.println("0");
			TextEditor editor2 = (TextEditor) editor;
			try {
				IDocument doc = editor2.getDocumentProvider().getDocument(editor2.getEditorInput());
				String contents = doc.get();
				
				if (symbol == null) {
					//System.out.println("A0 " + snippet);
					
					int offset = contents.indexOf(snippet, position);
					if (offset >= 0) {
						//System.out.println("A1 " + offset);
						editor2.selectAndReveal(offset, 0);
					} else {
						//System.out.println("A2 " + position);
						editor2.selectAndReveal(position, 0);
					}
				} else {
					//System.out.println("B0");
					int offset = contents.indexOf(symbol, position);
					if (offset >= 0) {
						//System.out.println("B1 " + offset);
						editor2.selectAndReveal(offset, symbol.length());
					} else {
						//System.out.println("B2 " + position);
						editor2.selectAndReveal(position, 0);
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
				System.out.println("**jump fallback 5");
				// Somewhere something went wrong. Who knows precisely what?
				// We'll just jump to the given position
				editor2.selectAndReveal(position, 0);
			}
			System.out.println("**** jump done");
			
		}
	}
	
	
	private ProjectionSupport projectionSupport;
	private ProjectionAnnotationModel projectionAnnotationModel;
	
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		
		ProjectionViewer viewer = (ProjectionViewer) getSourceViewer();
		projectionSupport = new ProjectionSupport(viewer, getAnnotationAccess(),
				getSharedColors());
		projectionSupport.addSummarizableAnnotationType(
				"org.eclipse.ui.workbench.texteditor.error");
		projectionSupport.addSummarizableAnnotationType(
				"org.eclipse.ui.workbench.texteditor.warning");
		projectionSupport.install();
		
		viewer.doOperation(ProjectionViewer.TOGGLE);
		projectionAnnotationModel = viewer.getProjectionAnnotationModel();

		licm = new LispInformationControlManager(this);
		licm.install(this.getSourceViewer().getTextWidget());
	}
	
	
	protected ISourceViewer createSourceViewer (Composite parent, IVerticalRuler ruler, int styles) {
		SourceViewer viewer = new ProjectionViewer(parent, ruler,
				getOverviewRuler(), isOverviewRulerVisible(), styles);
		// ensure decoration support has been created and configured.
		getSourceViewerDecorationSupport(viewer);
        highlighter.install(viewer);
		
		return viewer;
	}
	
	private LispInformationControlManager licm;
	
	public void showPopupInfo(String message) {
		StyledText txt = this.getSourceViewer().getTextWidget();
		Point p = txt.getLocationAtOffset(this.getSourceViewer().getSelectedRange().x);
		
		licm.setLocation(p);
		licm.setText(message);
		licm.showInformation();
	}
	
    protected void initializeKeyBindingScopes() {
		super.initializeKeyBindingScopes();
		setKeyBindingScopes(new String[] { "jasko.tim.lisp.context1" });  
	}
	
	/**
	 * Behold, my secret shame.
	 * What you see below is technically wrong, or at the very least, obsolete.
	 * This is no longer how the Eclipse folks want you to set up key bindings.
	 * HOWEVER, their documentation is not very clear on how to do this in the
	 *  new fashion, and I finally settled on this in frustration.
	 *  
	 *  TODO: register our key bindings like good little boys.
	 */
	protected void createActions() {
		super.createActions();
		
		IKeyBindingService keys = this.getSite().getKeyBindingService();
		IndentAction indent = new IndentAction(this);
		indent.setActionDefinitionId(IndentAction.ID);
		keys.registerAction(indent);
		
		/*CompileAction compile = new CompileAction(this);
		compile.setActionDefinitionId("jasko.tim.lisp.actions.CompileAction");
		keys.registerAction(compile);
		
		CompileSlowAction compileSlow = new CompileSlowAction(this);
		compileSlow.setActionDefinitionId("jasko.tim.lisp.actions.CompileSlowAction");
		keys.registerAction(compileSlow);
		
		CompileTopAction compileTop = new CompileTopAction(this, true);
		compileTop.setActionDefinitionId("jasko.tim.lisp.actions.CompileTopAction");
		keys.registerAction(compileTop);
		
		CompileTopAction compileTopSlow = new CompileTopAction(this, false);
		compileTopSlow.setActionDefinitionId("jasko.tim.lisp.actions.CompileTopSlowAction");
		keys.registerAction(compileTopSlow);
		
		LispDocAction lispdoc = new LispDocAction(this);
		lispdoc.setActionDefinitionId("jasko.tim.lisp.actions.LispDocAction");
		keys.registerAction(lispdoc);
		
		HyperSpecAction hyperSpec = new HyperSpecAction(this);
		hyperSpec.setActionDefinitionId("jasko.tim.lisp.actions.HyperSpecAction");
		keys.registerAction(hyperSpec);
		
		EvalTopLevelExpAction evalTopLevelExp = new EvalTopLevelExpAction(this);
		evalTopLevelExp.setActionDefinitionId("jasko.tim.lisp.actions.EvalTopLevelExpAction");
		keys.registerAction(evalTopLevelExp);
		
		EditDefinitionAction editDefinition = new EditDefinitionAction(this);
		editDefinition.setActionDefinitionId("jasko.tim.lisp.actions.EditDefinitionAction");
		keys.registerAction(editDefinition);
		
		IndentAction indent = new IndentAction(this);
		indent.setActionDefinitionId("jasko.tim.lisp.actions.IndentAction");
		keys.registerAction(indent);
		
		MacroExpandAction macroExpand = new MacroExpandAction(this, false);
		macroExpand.setActionDefinitionId("jasko.tim.lisp.actions.MacroExpandAction");
		keys.registerAction(macroExpand);
		
		MacroExpandAction macroExpandAll = new MacroExpandAction(this, true);
		macroExpandAll.setActionDefinitionId("jasko.tim.lisp.actions.MacroExpandAllAction");
		keys.registerAction(macroExpandAll);
		
		UndefineFunctionAction undefineFunc = new UndefineFunctionAction(this);
		undefineFunc.setActionDefinitionId("jasko.tim.lisp.actions.UndefineFunctionAction");
		keys.registerAction(undefineFunc);
		
		FindCallersAction findCallersFunc = new FindCallersAction(this);
		findCallersFunc.setActionDefinitionId("jasko.tim.lisp.actions.FindCallersAction");
		keys.registerAction(findCallersFunc);
		
		SwitchToReplAction switchToRepl = new SwitchToReplAction(this);
		undefineFunc.setActionDefinitionId("jasko.tim.lisp.actions.SwitchToReplAction");
		keys.registerAction(switchToRepl);
		
		ProfileFunctionAction profileFunc = new ProfileFunctionAction(this);
		profileFunc.setActionDefinitionId(ProfileFunctionAction.ID);
		keys.registerAction(profileFunc);
		
		ProfileResetAction profileReset = new ProfileResetAction(this);
		profileReset.setActionDefinitionId(ProfileResetAction.ID);
		keys.registerAction(profileReset);*/
	}


	private void deleteTasks() {
		IFile file = getIFile();
		if( file == null ){
			return;
		}		
		try {
			file.deleteMarkers(IMarker.TASK, false, IResource.DEPTH_ZERO);
			//file.deleteMarkers(null, false, IResource.DEPTH_ZERO);
		} catch (CoreException ce) {
		}
	}

	
	private void addTask(String message, int lineNumber) {
		IFile file = getIFile();
		if( file == null ){
			return;
		}
		try {
			IMarker marker = file.createMarker(IMarker.TASK);
			marker.setAttribute(IMarker.MESSAGE, message);
			if (lineNumber == -1) {
				lineNumber = 1;
			}
			marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}


	private void updateTasks() {
		IFile file = getIFile();
		if( file == null ){
			return;
		}
		deleteTasks();
		String[] lines = getDocument().get().split("\n");
		int numLines = 0;
		for( String line : lines ){
			if ( line.matches(".*;.*TODO:.*") ){
				String[] strs = line.split("TODO:");
				for ( int i = 1; i < strs.length; ++i ) {
					addTask("TODO:" + strs[i],numLines+1);
				}
			}
			++numLines;			
		}
	}
	
	
	public void doSave(IProgressMonitor monitor) {
		super.doSave(monitor);
		
		try {
			IDocument doc = getDocument();
			LispNode contents = LispParser.parse(doc.get() + "\n)");
			outline.update(contents);
			updateTasks();
			//updateFolding(contents); TODO: change outline in same way as folding now
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void updateFoldingStructure(HashSet<Position> positions, 
			Position lastSection)
	{
		// these hold mapping between oldannotations and their positions
		HashSet<Position> oldPositions = new HashSet<Position>();
		HashMap<Position,ProjectionAnnotation> hashAnnotations = 
			new HashMap<Position, ProjectionAnnotation>();

		// prepare data structures
		{
			Iterator it = projectionAnnotationModel.getAnnotationIterator();
			while(it.hasNext()){
				ProjectionAnnotation a = (ProjectionAnnotation)it.next();
				Position p = projectionAnnotationModel.getPosition(a);
				oldPositions.add(p);
				hashAnnotations.put(p, a);
			}
		}
		
		//get removed annotations
		HashSet<Position> remPositions = new HashSet<Position>();
		remPositions.addAll(oldPositions);
		remPositions.removeAll(positions);
		Annotation[] remAnnotations = new Annotation[remPositions.size()];
		{
			Iterator it = remPositions.iterator();
			int i = 0;
			while(it.hasNext()){
				remAnnotations[i] = hashAnnotations.get(it.next());
				++i;
			}
		}
		
		//get new annotations
		HashMap<ProjectionAnnotation, Position> newAnnotations = 
			new HashMap<ProjectionAnnotation, Position>();

		positions.removeAll(oldPositions);
		{
			Iterator it = positions.iterator();
			while(it.hasNext()){
				newAnnotations.put(new ProjectionAnnotation(), (Position)it.next());
			}
		}
		
		//deal with possible garbage in last section when it is collapsed
		ProjectionAnnotation lastSectionAnnotation = hashAnnotations.get(lastSection);
		boolean lastSectionCollapsed = false;
		if (lastSectionAnnotation != null ) {
			lastSectionCollapsed = lastSectionAnnotation.isCollapsed();
			projectionAnnotationModel.expand(lastSectionAnnotation);
		}
		
		projectionAnnotationModel.modifyAnnotations(remAnnotations,newAnnotations,null);
		
		if ( lastSectionCollapsed ) {
			projectionAnnotationModel.collapse(lastSectionAnnotation);
		}
	}
	
	
    public void dispose () {
        super.dispose();
        highlighter.uninstall(getSourceViewer());
    }
    
	/**
	 * Seriously, couldn't they have made this a little easier to get at?
	 */
	public IDocument getDocument() {
		return getDocumentProvider().getDocument(getEditorInput());
	}
	
	private String inPackage = ")"; //impossible
	
	// updated by LispReconcilingStrategy
	public void setPackage(String str){
		inPackage = str;
	}
	
	public String getPackage() {
		if ( inPackage.equals(")") ) {
			LispNode contents = LispParser.parse(getDocument().get() + "\n");
			for (int i=0; i<contents.params.size(); ++i) {
				LispNode sexp = contents.params.get(i);
				
				if (sexp.get(0).value.equalsIgnoreCase("in-package") ||
						sexp.get(0).value.equalsIgnoreCase("defpackage")) {
					inPackage = sexp.get(1).value;
				}
			}
		}
		if ( inPackage.equals(")") ) {
			inPackage = "";
		}

		return inPackage;
	}
	
	
	/**
	 * We overload this to enable our handy outline page
	 */
	public Object getAdapter(Class adapter) {
		if (adapter.equals(IContentOutlinePage.class)) {
			
			outline = new LispOutlinePage(this);
			return outline;
			
		} else {
			return super.getAdapter(adapter);
		}
	}
	
	

}
