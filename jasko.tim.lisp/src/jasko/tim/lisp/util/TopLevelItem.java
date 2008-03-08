package jasko.tim.lisp.util;

public class TopLevelItem implements Comparable<TopLevelItem> {
	public String name;
	public int offset; //offset of "(" encompassing top level sexp
	public int offsetEnd; //offset of ")" encompassing top level sexp
	public int nameOffset; //offset of name of top level sexp
	public String type;
	public String pkg;
	
	public int compareTo(TopLevelItem o) {
		return name.toLowerCase().compareTo( 
			o.name.toLowerCase() );
	}
	
	public String toString(){
		return "{"+type+","+name+" ("+offset+")}";
	}
}
