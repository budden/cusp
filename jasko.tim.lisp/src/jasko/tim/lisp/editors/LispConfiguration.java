package jasko.tim.lisp.editors;

import jasko.tim.lisp.ColorManager;
import jasko.tim.lisp.editors.assist.*;

import org.eclipse.swt.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;
import org.eclipse.jface.text.presentation.*;
import org.eclipse.jface.text.rules.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.ui.editors.text.*;


/**
 * Eclipse uses this class to do the majority of the customization that makes our Lisp
 *  editor a Lisp editor.
 * @author Tim Jasko
 *
 */
public class LispConfiguration extends TextSourceViewerConfiguration {
	protected RuleBasedScanner tagScanner;
	
	protected ColorManager colorManager;
	protected ContentAssistant ca;
	protected LispEditor editor;

	public LispConfiguration(LispEditor editor, ColorManager colorManager) {
		this.editor = editor;
		this.colorManager = colorManager;
		
		ca = new ContentAssistant();
		ca.setContentAssistProcessor(new ArglistAssistProcessor(editor), 
			IDocument.DEFAULT_CONTENT_TYPE);
		
		ca.enableAutoActivation(true);
		ca.setAutoActivationDelay(700);
		ca.setProposalPopupOrientation(ContentAssistant.CONTEXT_INFO_BELOW);
		ca.setContextInformationPopupOrientation(ContentAssistant.CONTEXT_INFO_ABOVE);
		
	}
	
	
	public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {
		return ca;
	}
	
	public ITextHover getTextHover(ISourceViewer sourceViewer, String contentType) {
		if (contentType.equals(IDocument.DEFAULT_CONTENT_TYPE)) {
			return new LispTextHover(editor);
		} else {
			return null;
		}
	}
	
	public IAnnotationHover getAnnotationHover(ISourceViewer sourceViewer) {
		return new MarkerAnnotationHover();
	}
	
	public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
		return new String[] {
			IDocument.DEFAULT_CONTENT_TYPE,
			LispPartitionScanner.LISP_COMMENT,
			LispPartitionScanner.LISP_STRING,
			LispPartitionScanner.LISP_CHARACTER
			};
	}

	protected RuleBasedScanner getLispScanner() {
		if (tagScanner == null) {
			tagScanner = new LispScanner(colorManager, null);
			tagScanner.setDefaultReturnToken(
				new Token(
					new TextAttribute(
						colorManager.getColor(ColorManager.TokenType.DEFAULT))));
		}
		return tagScanner;
	}

	public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer) {
		//Set up the parentheses matching while we're here
		ParenMatcher pm = new ParenMatcher();
		MatchingCharacterPainter painter = new MatchingCharacterPainter(
				sourceViewer, pm);
		pm.setPainter(painter);
		//painter.setColor(Display.getDefault().getSystemColor(SWT.COLOR_DARK_GRAY));
		ITextViewerExtension2 extension = (ITextViewerExtension2)sourceViewer;
		extension.addPainter(painter);
		
		
		PresentationReconciler reconciler = new PresentationReconciler();

		DefaultDamagerRepairer dr =
			new DefaultDamagerRepairer(getLispScanner());
		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

		NonRuleBasedDamagerRepairer ndr =
			new NonRuleBasedDamagerRepairer(
				new TextAttribute(
					colorManager.getColor(ColorManager.TokenType.COMMENT), null, SWT.ITALIC));
		reconciler.setDamager(ndr, LispPartitionScanner.LISP_COMMENT);
		reconciler.setRepairer(ndr, LispPartitionScanner.LISP_COMMENT);
		
		NonRuleBasedDamagerRepairer ndr3 =
			new NonRuleBasedDamagerRepairer(
				new TextAttribute(
					colorManager.getColor(ColorManager.TokenType.STRING)));
		reconciler.setDamager(ndr3, LispPartitionScanner.LISP_CHARACTER);
		reconciler.setRepairer(ndr3, LispPartitionScanner.LISP_CHARACTER);
		
		NonRuleBasedDamagerRepairer ndr2 =
			new NonRuleBasedDamagerRepairer(
				new TextAttribute(
					colorManager.getColor(ColorManager.TokenType.STRING)));
		reconciler.setDamager(ndr2, LispPartitionScanner.LISP_STRING);
		reconciler.setRepairer(ndr2, LispPartitionScanner.LISP_STRING);

		return reconciler;
	}
	
	public IAutoEditStrategy[] getAutoEditStrategies(ISourceViewer sourceViewer, String contentType) {
		return new IAutoEditStrategy[] {new LispIndenter() };
	}
	
	public String[] getIndentPrefixes(ISourceViewer sourceViewer,
         String contentType) {
		return null;
	}
	
	public int getTabWidth(ISourceViewer sourceViewer) {
		return 8; // Seems to match emacs defaults. Or at least the defaults on our particular build.
	}
}