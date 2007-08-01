package jasko.tim.lisp.inspector;


import jasko.tim.lisp.swank.*;

public class InspectorRunnable extends SwankRunnable {

	public void run() {
		if (! result.getf(":return").get(0).value.equalsIgnoreCase(":abort")) {
			LispNode ret = getReturn();

			if( ret.value.equalsIgnoreCase("nil")){
				return;
			} else {
				String title = ret.getf(":title").value;
				String type = ret.getf(":type").value;
				LispNode content = ret.getf(":content");
				
				InspectorView.showInspector(title, type, content);				
			}
		}
	}
}
