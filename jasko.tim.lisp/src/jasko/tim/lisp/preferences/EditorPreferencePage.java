package jasko.tim.lisp.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;
import jasko.tim.lisp.LispPlugin;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */

public class EditorPreferencePage
	extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {

	public EditorPreferencePage() {
		super(GRID);
		setPreferenceStore(LispPlugin.getDefault().getPreferenceStore());
		setDescription("The editing environment (colors, autocomplete, etc.).");
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
        addField(new BooleanFieldEditor(
                PreferenceConstants.PAIR_EDIT_BRACKETS,
                "Automatically close '('",
                getFieldEditorParent()));
    
        addField(new BooleanFieldEditor(
                PreferenceConstants.PAIR_SMART_BRACKETS,
                "If automatically close '('.\nIf next is complete sexp, put closing ')' after it",
                getFieldEditorParent()));
    
        addField(new BooleanFieldEditor(
                PreferenceConstants.PAIR_EDIT_BRACES,
                "If complete sexp starts at next position, pressing '[' encloses it in '()'",
                getFieldEditorParent()));
    
        addField(new BooleanFieldEditor(
                PreferenceConstants.PAIR_EDIT_QUOTES,
                "Automatically close '\"'",
                getFieldEditorParent()));
    
        addField(new BooleanFieldEditor(
                PreferenceConstants.PAIR_EDIT_COMMENTS,
                "Automatically close '#|'",
                getFieldEditorParent()));
    
        addField(new BooleanFieldEditor(
                    PreferenceConstants.AUTO_POPUP_COMPLETIONS,
                    "Automatically show content completions and parameter hints",
                    getFieldEditorParent()));
        
        addField(new StringFieldEditor(
                PreferenceConstants.AUTO_POPUP_COMPLETIONS_DELAY,
                "Delay before completions are shown and quick doc updated \n(requires Eclipse restart):",
                getFieldEditorParent()));
        //TODO: actually should listen for change in this variable and perform operation similar to
        // LispConfiguration.java: 		ca.setAutoActivationDelay(ps.getInt(PreferenceConstants.AUTO_POPUP_COMPLETIONS_DELAY));

        
        addField(new StringFieldEditor(
                PreferenceConstants.MAX_HINT_LINES,
                "Maximum number of lines in documentation hints:",
                getFieldEditorParent()));

        addField(new BooleanFieldEditor(
                PreferenceConstants.ARGLIST_BELOW,
                "Show documentation hints below cursor. (Requires restart)",
                getFieldEditorParent()));
        
        addField(new BooleanFieldEditor(
                PreferenceConstants.AUTO_INSERT_COMPLETIONS,
                "Automatically insert single content completion option",
                getFieldEditorParent()));

		addField(new StringFieldEditor(PreferenceConstants.AUTO_COMPLETIONS_NLIMIT, 
				"Max size of completion list. 0 or empty if no limit:", getFieldEditorParent()) );

        addField(new BooleanFieldEditor(
                PreferenceConstants.AUTO_FUZZY_COMPLETIONS,
                "Use fuzzy mode for autocompletions",
                getFieldEditorParent()));

        addField(new BooleanFieldEditor(
                PreferenceConstants.DOCS_IN_COMPLETIONS,
                "Show quick documentation with auto completions list",
                getFieldEditorParent()));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}
	
}