package jasko.tim.lisp.editors;

import jasko.tim.lisp.ColorManager;
import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.ColorManager.ColorChangeEvent;
import jasko.tim.lisp.editors.actions.IndentAction;
import jasko.tim.lisp.editors.assist.LispInformationControlManager;
import jasko.tim.lisp.editors.outline.LispOutlinePage;
import jasko.tim.lisp.swank.LispNode;
import jasko.tim.lisp.swank.LispParser;
import jasko.tim.lisp.util.LispUtil;

import java.util.Arrays;
import java.util.HashMap;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.*;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.*;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

public class LispEditor extends TextEditor implements ILispEditor {
	private LispOutlinePage outline;
	private ColorManager.ChangeEventListener colorPrefChangeListener;
    private final LispConfiguration config = new LispConfiguration(this, LispPlugin.getDefault().getColorManager());
    
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
    private IAnnotationModel annotationModel;
	
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		
		ProjectionViewer viewer = (ProjectionViewer) getSourceViewer();
		projectionSupport = new ProjectionSupport(viewer, getAnnotationAccess(),
				getSharedColors());
		projectionSupport.install();
		viewer.doOperation(ProjectionViewer.TOGGLE);
		projectionAnnotationModel = viewer.getProjectionAnnotationModel();
        annotationModel = viewer.getAnnotationModel();

		licm = new LispInformationControlManager(this);
		licm.install(this.getSourceViewer().getTextWidget());
		
		initFolding();
	}
	
	
	protected ISourceViewer createSourceViewer(Composite parent,
         IVerticalRuler ruler, int styles) {
		
		SourceViewer viewer = new ProjectionViewer(parent, ruler,
				getOverviewRuler(), isOverviewRulerVisible(), styles);
		// ensure decoration support has been created and configured.
		getSourceViewerDecorationSupport(viewer);
 
        CurrentExpressionHighlightingListener highlighter = new CurrentExpressionHighlightingListener();
        viewer.getTextWidget().addKeyListener(highlighter);
        viewer.getTextWidget().addMouseListener(highlighter);
		
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
	
    private class CurrentExpressionHighlightingListener implements KeyListener, MouseListener {
        private final IDocument doc = getDocument();
        private final Annotation currentHighlightAnnotation = new Annotation("jasko.tim.lisp.editors.LispEditor.current-sexp",
        																    false, "does this show up anywhere?");
        private int[] currentHighlightRange;
        
        private void removeHighlight () {
            if (currentHighlightAnnotation != null) {
                annotationModel.removeAnnotation(currentHighlightAnnotation);
            }
        }
        
        private void updateHighlighting () {
            ITextSelection ts = (ITextSelection)getSelectionProvider().getSelection();
            try {
                int[] range = LispUtil.getCurrentExpressionRange(getDocument(), ts.getOffset());
                try {
                    if (range == null) {
                        removeHighlight();
                    } else if (currentHighlightRange != null && Arrays.equals(currentHighlightRange, range)) {
                        // leave current highlight in place, still valid
                    } else {
                        removeHighlight();
                        annotationModel.addAnnotation(currentHighlightAnnotation, new Position(range[0], range[1]));
                    }
                } finally {
                    currentHighlightRange = range;
                }
            } catch (BadLocationException ex) {
                ex.printStackTrace();
            }
        }
        
        public void keyPressed (KeyEvent e) {// only need to update highlighting for key events that might move us into a different s-expression scope
            switch (e.character) {
                case '(':
                case ')':
                case '\b':
                    updateHighlighting();
                    return;
            }
            switch (e.keyCode) {
                case SWT.ARROW_RIGHT:
                case SWT.ARROW_LEFT:
                case SWT.ARROW_DOWN:
                case SWT.ARROW_UP:
                case SWT.HOME:
                case SWT.END:
                case SWT.PAGE_DOWN:
                case SWT.PAGE_UP:
                case SWT.DEL:
                    updateHighlighting();
            }
        }

        public void keyReleased (KeyEvent e) {
        }

        public void mouseDoubleClick (MouseEvent e) {
        }

        public void mouseDown (MouseEvent e) {
            updateHighlighting();
        }

        public void mouseUp (MouseEvent e) {
        }
        
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
			
			if (projectionAnnotationModel != null) {
                // there are times when the annotation model is null, such as when one opens a lisp file from disk
                // that is not part of a project.  I don't know how to really fix this, but it seems that it's better
                // to show the editor than to fail because we can't get folding to work.  - Chas
    			projectionAnnotationModel.modifyAnnotations(prevAnnotations, newAnnotations, null);
    			prevAnnotations = annotations;
            }
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
