package jasko.tim.lisp.util;

public class TopLevelItem implements Comparable<TopLevelItem> {
	public String name;
	public int offset;
	public String type;
	
	public int compareTo(TopLevelItem o) {
		return name.toLowerCase().compareTo( 
			o.name.toLowerCase() );
	}
}
