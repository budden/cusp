package jasko.tim.lisp.editors;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.core.resources.IFile;

// a relatively hacky way for us to be able to treat the LispEditor and the editable part of the REPL
// as the same thing in certain respects
public interface ILispEditor {
    public String showParameterHints ();
    
    public String showContentCompletions ();
    
    public void callUrl(String url);

    public ISelectionProvider getSelectionProvider ();
    
    public IFile getIFile();
    
    public IDocument getDocument ();
}
