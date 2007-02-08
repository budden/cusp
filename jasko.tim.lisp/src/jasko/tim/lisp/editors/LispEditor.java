package jasko.tim.lisp.editors;

import java.util.HashMap;

import jasko.tim.lisp.*;
import jasko.tim.lisp.editors.assist.*;
import jasko.tim.lisp.editors.outline.*;
import jasko.tim.lisp.editors.actions.*;
import jasko.tim.lisp.swank.*;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.jface.text.source.projection.*;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.editors.text.*;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.views.contentoutline.*;


public class LispEditor extends TextEditor {

	private LispOutlinePage outline;

	public LispEditor() {
		super();
		setSourceViewerConfiguration(new LispConfiguration(this, LispPlugin.getDefault().getColorManager()));
		setDocumentProvider(new LispDocumentProvider());
		//setRangeIndicator(new DefaultRangeIndicator());
	}
	
	public void dispose() {
		super.dispose();
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
		jumpToDefinition(filePath, position, snippet, "");
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
			TextEditor editor2 = (TextEditor) editor;
			try {
				editor2.selectAndReveal(position, 0);
			} catch (Exception e) {
				e.printStackTrace();
				try {
					IDocument doc = editor2.getDocumentProvider().getDocument(editor2.getEditorInput());
					String contents = doc.get();
					// The snippet is of code *before* the part that we care about, so we'll want to jump to the end of it
					int snippetLength = snippet.length();
					// Search for the snippet in the document. The position given by SBCL tends to be early,
					//  so check for it after that.
					int offset = contents.indexOf(snippet, position);
					System.out.println("**jump fallback 1");
					if (offset >= 0) {
						editor2.selectAndReveal(offset + snippetLength, 0);
					} else {
						System.out.println("**jump fallback 2");
						// That failed. See if the snippet occurs before the position given
						offset = contents.indexOf(snippet);
						if (offset >= 0) {
							editor2.selectAndReveal(offset + snippetLength, 0);
						} else {

							System.out.println("**jump fallback 3");
							// Still we fail! Check for usage of the symbol after position.
							//  That should be close to correct
							offset = contents.indexOf(symbol, position);
							if (offset >= 0) {
								editor2.selectAndReveal(offset, 0);
							} else {
								System.out.println("**jump fallback 4");
								// Screw it
								editor2.selectAndReveal(position, 0);
							}
						}
					}
				} catch (Exception e2) {
					System.out.println("**jump fallback 5");
					// Somewhere something went wrong. Who knows precisely what?
					// We'll just jump to the symbol, if we can find it
					IDocument doc = editor2.getDocumentProvider().getDocument(editor2.getEditorInput());
					int offset = doc.get().indexOf(symbol);
					if (offset >= 0) {
						editor2.selectAndReveal(offset, 0);
					}
					
				}
			}
		}
	}
	
	
	private ProjectionSupport projectionSupport;
	private ProjectionAnnotationModel annotationModel;
	
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		
		ProjectionViewer viewer = (ProjectionViewer) getSourceViewer();
		projectionSupport = new ProjectionSupport(viewer, getAnnotationAccess(),
				getSharedColors());
		projectionSupport.install();
		viewer.doOperation(ProjectionViewer.TOGGLE);
		annotationModel = viewer.getProjectionAnnotationModel();

		licm = new LispInformationControlManager(this);
		licm.install(this.getSourceViewer().getTextWidget());
		
		initFolding();
	}
	
	
	protected ISourceViewer createSourceViewer(Composite parent,
         IVerticalRuler ruler, int styles) {
		
		ISourceViewer viewer = new ProjectionViewer(parent, ruler,
				getOverviewRuler(), isOverviewRulerVisible(), styles);
		// ensure decoration support has been created and configured.
		getSourceViewerDecorationSupport(viewer);
		
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
	
	/*protected void configureSourceViewerDecorationSupport(SourceViewerDecorationSupport support) {
		super.configureSourceViewerDecorationSupport(support);
	}*/
	
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
		CompileAction compile = new CompileAction(this);
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
	}
	
	public void doSave(IProgressMonitor monitor) {
		super.doSave(monitor);
		
		try {
			IDocument doc = getDocument();
			LispNode contents = LispParser.parse(doc.get() + "\n)");
			outline.update(contents);
			updateFolding(contents);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private void initFolding() {
		IDocument doc = getDocument();
		LispNode contents = LispParser.parse(doc.get() + "\n)");
		updateFolding(contents);
	}
	
	private Annotation[] prevAnnotations;
	
	// TODO: make it so we don't over-write folds that haven't changed.
	private void updateFolding(LispNode contents) {
		IDocument doc = getDocument();
		HashMap<ProjectionAnnotation, Position> newAnnotations = new HashMap<ProjectionAnnotation, Position>();
		Annotation[] annotations = new Annotation[contents.params.size()]; 
		for (int i=0; i<contents.params.size()-1; ++i) {
			LispNode sexp = contents.params.get(i);
			//LispNode nextp = contents.params.get(i+1);
			
			ProjectionAnnotation annotation = new ProjectionAnnotation();
			
			/* Let's all take this moment to poo-poo the Eclipse designers who decided to use
			 *  the Position class here, when it is not in fact a position that is being asked
			 *  for. You've even got a perfectly good Region class, kids!
			 *  
			 *  Don't poo-poo too hard, though. They're probably still smarter than you.
			 */
			if (sexp.endOffset > sexp.offset) {
				//System.out.println("***end offset was useful!");
				try {
					int startLine = doc.getLineOfOffset(sexp.offset);
					int endLine = doc.getLineOfOffset(sexp.endOffset);
					if (endLine > startLine) {
						int startOffset = doc.getLineOffset(startLine);
						int endOffset = doc.getLineOffset(endLine) + doc.getLineLength(endLine);
						newAnnotations.put(annotation, new Position(startOffset, endOffset - startOffset));
					} else { //single line. No point in folding it.
					}
				} catch (BadLocationException e) {
					e.printStackTrace();
					newAnnotations.put(annotation, new Position(sexp.offset, sexp.endOffset - sexp.offset));
				}
				
			} else {
				//newAnnotations.put(annotation, new Position(sexp.offset, nextp.offset - sexp.offset));
			}
			
			annotations[i] = annotation;
			
			
			//Check the package of this file while we're at it.
			if (sexp.get(0).value.equals("in-package")) {
				inPackage = sexp.get(1).value;
			}
		}
		
		if (contents.params.size() > 0) {
			LispNode last = contents.params.get(contents.params.size()-1);
			ProjectionAnnotation annotation = new ProjectionAnnotation();
			newAnnotations.put(annotation, new Position(last.offset, doc.getLength() - last.offset));
			
			annotations[contents.params.size()-1] = annotation;	
			
			
			annotationModel.modifyAnnotations(prevAnnotations, newAnnotations, null);
			prevAnnotations = annotations;
		}
	}
	
	/**
	 * Seriously, couldn't they have made this a little easier to get at?
	 */
	public IDocument getDocument() {
		return getDocumentProvider().getDocument(getEditorInput());
	}
	
	private String inPackage = "nil";
	
	public String getPackage() {
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
