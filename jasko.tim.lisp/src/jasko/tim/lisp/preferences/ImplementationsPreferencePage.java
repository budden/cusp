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

public class ImplementationsPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public ImplementationsPreferencePage() {
		super(GRID);
		setPreferenceStore(LispPlugin.getDefault().getPreferenceStore());
		setDescription("Add/remove installed Lisp implementations.");
	}
	
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
		addField(
				new BooleanFieldEditor(
					PreferenceConstants.USE_SITEWIDE_LISP,
					"Use a lisp &sitewide installation (executable is on the path)",
					getFieldEditorParent()));

		addField(new FileFieldEditor(PreferenceConstants.LISP_EXE, 
				"&Lisp Executable:", getFieldEditorParent()));

		addField(new FileFieldEditor(PreferenceConstants.LISP_INI, 
				"&Initialization File:", getFieldEditorParent()));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}


	
}