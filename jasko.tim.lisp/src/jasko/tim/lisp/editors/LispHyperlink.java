package jasko.tim.lisp.editors;

import org.eclipse.jface.text.Assert;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import jasko.tim.lisp.editors.actions.EditDefinitionAction;

public class LispHyperlink implements IHyperlink {

	private String fURLString;
	private IRegion fRegion;
	private LispEditor editor;

	public LispHyperlink(LispEditor edt, IRegion region, String urlString) {
		Assert.isNotNull(urlString);
		Assert.isNotNull(region);

		fRegion= region;
		fURLString= urlString;
		editor = edt;
	}

	public IRegion getHyperlinkRegion() {
		return fRegion;
	}

	public String getHyperlinkText() {
		return fURLString;
	}

	public String getTypeLabel() {
		return null;
	}

	public void open() {
		EditDefinitionAction eda = new EditDefinitionAction(editor);
		eda.run();
	}

}
