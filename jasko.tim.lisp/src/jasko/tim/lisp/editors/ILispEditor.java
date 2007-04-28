package jasko.tim.lisp.editors;

// a relatively hacky way for us to be able to treat the LispEditor and the editable part of the REPL
// as the same thing in certain respects
public interface ILispEditor {
    public String showParameterHints ();
    
    public String showContentCompletions ();
}
