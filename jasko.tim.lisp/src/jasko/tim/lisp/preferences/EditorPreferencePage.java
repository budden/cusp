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
                    PreferenceConstants.AUTO_POPUP_COMPLETIONS,
                    "Automatically show content completions and parameter hints",
                    getFieldEditorParent()));
        
        addField(new BooleanFieldEditor(
                PreferenceConstants.AUTO_INSERT_COMPLETIONS,
                "Automatically insert single content completion option",
                getFieldEditorParent()));

        addField(new BooleanFieldEditor(
                PreferenceConstants.AUTO_FUZZY_COMPLETIONS,
                "Use fuzzy mode for autocompletions",
                getFieldEditorParent()));

		addField(new StringFieldEditor(PreferenceConstants.AUTO_FUZZY_COMPLETIONS_TLIMIT, 
				"&Time limit for fuzzy search (ms). 0 or empty if no limit:", getFieldEditorParent()) );

        addField(new BooleanFieldEditor(
                PreferenceConstants.DOCS_IN_COMPLETIONS,
                "Show quick documentation with auto completions list",
                getFieldEditorParent()));

        addField(new StringFieldEditor(
                PreferenceConstants.DOCS_IN_COMPLETIONS_TLIMIT,
                "Maximum time (ms) to use to populate docs for completion list",
                getFieldEditorParent()));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}
	
}