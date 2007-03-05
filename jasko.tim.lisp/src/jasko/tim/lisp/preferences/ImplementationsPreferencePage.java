package jasko.tim.lisp.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
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

public class ImplementationsPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

	public ImplementationsPreferencePage() {
		super();
		setPreferenceStore(LispPlugin.getDefault().getPreferenceStore());
		setDescription("Add/remove installed Lisp implementations.");
	}
	
	public class ImplementationListFieldEditor extends ListEditor {
		
		public ImplementationListFieldEditor(String name, String label, Composite parent)
		{
			super(name, label, parent);
		}

		@Override
		protected String createList(String[] items) {
			return items.toString();
		}

		@Override
		protected String getNewInputObject() {
			FileDialog dialog = new FileDialog(this.getShell());
			dialog.open();
			return dialog.getFileName();
		}

		@Override
		protected String[] parseString(String stringList) {
			return new String[]{ "Stuff 1", "more things 2", "and-yet-more 3" };
		}
		
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}

	@Override
	protected Control createContents(Composite parent) {
		// TODO Auto-generated method stub
		return null;
	}
	
}