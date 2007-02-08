package jasko.tim.lisp.editors;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.ui.editors.text.FileDocumentProvider;

public class LispDocumentProvider extends FileDocumentProvider {

	protected IDocument createDocument(Object element) throws CoreException {
		IDocument document = super.createDocument(element);
		if (document != null) {
			connectPartitioner(document);
		}
		return document;
	}
	
	public static void connectPartitioner(IDocument doc) {
		IDocumentPartitioner partitioner = new FastPartitioner(
				new LispPartitionScanner(), LispPartitionScanner.PARTITIONS);
		partitioner.connect(doc);
		doc.setDocumentPartitioner(partitioner);
	}
}