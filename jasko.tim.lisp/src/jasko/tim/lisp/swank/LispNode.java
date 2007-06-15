package jasko.tim.lisp.swank;

import java.util.*;

/**
 * Handy structure I use to store s-expressions.
 *  Luckily for us, we don't actually do much with them beyond
 *  pass them back into Lisp, so we don't actually have to worry much about the data type.
 * @author Tim Jasko
 */
public class LispNode {
	public int offset = 0;
	public int endOffset = 0;
	public String value = "";
	public ArrayList<LispNode> params = new ArrayList<LispNode>();
	public ArrayList<LispComment> comments = new ArrayList<LispComment>(); //keep comments inside a file only in root LispNode
	
	public boolean isString = false;
	
	public LispNode() {
	}
	
	public LispNode(int offset) {
		this.offset = offset;
	}
	
	public LispNode(int offset, int endOffset) {
		this.offset = offset;
		this.endOffset = endOffset;
	}
	
	public LispNode(String val) {
		value = val;
	}
	
	public LispNode(String val, int offset) {
		value = val;
		this.offset = offset;
	}
	
	public LispNode(String val, int offset,int endOffset) {
		value = val;
		this.offset = offset;
		this.endOffset = endOffset;
	}
	
	public int asInt() {
		int ret = 0;
		try {
			ret = Integer.parseInt(value);
		} catch (NumberFormatException e) {
		}
		return ret;
	}
	
	
	public LispNode car() {
		if (params.size() >= 1) {
			return (LispNode)params.get(0);
		} else {
			return new LispNode();
		}
	}
	
	public LispNode cadr() {
		if (params.size() >= 2) {
			return (LispNode)params.get(1);
		} else {
			return new LispNode();
		}
	}
	
	public LispNode get(int i) {
		try {
			if (params.size() >= i+1) {
				return (LispNode)params.get(i);
			} else {
				return new LispNode();
			}
		} catch (Exception e) {
			System.out.println(e);
			return new LispNode();
		}
	}
	
	/**
	 * Treats this node as a plist.
	 * @param key
	 * @return The value in the list after the key. A blank node is returned if the key isn't found.
	 */
	public LispNode getf(String key) {
		for (int i=0; i<params.size(); ++i) {
			LispNode kid = params.get(i);
			if (kid.value.equalsIgnoreCase(key)) {
				return this.get(i+1);
			} else if (kid.params.size() > 0) {
				LispNode grandKid = kid.get(0);
				if (grandKid.value.equalsIgnoreCase(key)) {
					return kid.get(1);
				}
			}
		}
		
		return new LispNode();
	}

	public String toString() {
		if (!value.equals("")) {
			return "`" + value + "`";
		} else {
			String ret = "(";
			for (int i=0; i<params.size(); ++i) {
				ret += params.get(i).toString() + " ";
			}
			ret += ")";
			return ret;
		}
	}
	
	public String toLisp() {
		if (isString) {
			return '"' + value + '"';
		} else if (value.equals("")) {
			StringBuilder ret = new StringBuilder();
			ret.append("(");
			int size = params.size();
			for (int i=0; i<size; ++i) {
				LispNode param = params.get(i);
				ret.append(param.toLisp());
				if (i < size-1) {
					ret.append(' ');
				}
			}
			ret.append(")");
			return ret.toString();
		} else {
			return value;
		}
	}
	
	public boolean equals(LispNode other) {
		return this.toLisp().equals(other.toLisp());
	}

	public void clearComments(){
		comments.clear();
	}
	
	public void addComment(String val, int offset, int endOffset){
		comments.add(new LispComment(val,offset,endOffset));
	}
	
	public int getNumberOfComments() {
		int ret = 0;
		for( LispComment comment: comments ) {
			if( comment.isSectionComment() ) {
				++ret;
			}
		}
		return ret;
	}
	
	public class LispComment {
		public int offset = 0;
		public int endOffset = 0;
		public String value = "";
		public final static String SECTION_START = ";;;;<";
		public final static String SECTION_END = "\\>";
		public final int SECTION_START_LENGTH = SECTION_START.length(); 
		
		public LispComment() {
		}
		
		public LispComment(String val, int offset,int endOffset) {
			value = val;
			this.offset = offset;
			this.endOffset = endOffset;
		}		

		public boolean isSectionComment() {
			return value.startsWith(SECTION_START);
		}
		
		public String SectionName() {
			if ( isSectionComment() )
			{
				return value.substring(SECTION_START_LENGTH,value.length()).split(SECTION_END)[0];
			} else {
				return "";
			}
		}		
	}
}
