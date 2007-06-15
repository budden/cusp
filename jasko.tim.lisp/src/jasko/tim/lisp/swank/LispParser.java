package jasko.tim.lisp.swank;

import java.util.*;


/**
 * Quick and very dirty lisp parser.
 *  I wouldn't mind seeing it replaced.
 * @author Tim Jasko
 *
 */
public class LispParser {
	public int parenBalance = 0;
	
	public static LispNode parse(String code) {
		return new LispParser().parseCode(code);
	}
	
	public LispNode parseCode(String code) {
		int start = 0;
		parenBalance = 0;
		
		//System.out.println("*parsing:" + code.charAt(0));
		LispNode ret = new LispNode(0);
		Stack<LispNode> s = new Stack<LispNode>();
		
		LispNode curr = ret;
		s.push(curr);
		
		StringBuilder sb = new StringBuilder();
		int length = code.length();
		int i = 0;
		for (i=start; i<length; ++i) {
			char c = code.charAt(i);
			if (c == '(') {
				++parenBalance;
				LispNode next = new LispNode(i);
				if (!sb.toString().equals("")) {
					curr.params.add(new LispNode(sb.toString(), i-sb.toString().length(), i));
					sb = new StringBuilder();
				}
				curr.params.add(next);
				curr = next;
				s.push(curr);
			} else if (c == ')') {
				--parenBalance;
				if (!sb.toString().equals("")) {
					curr.params.add(new LispNode(sb.toString(), i-sb.toString().length(), i-1));
					sb = new StringBuilder();
				}
				curr.endOffset = i;
				
				try {
					s.pop();
				} catch (EmptyStackException e) {
				}
				if (!s.empty()) {
					curr = (LispNode)s.peek();
				}
					
			} else if (c == '"') {
				int offset = i;
				char lit = '"';
				do {
					++i;
					if (i < length) {
						lit = code.charAt(i);
						if (lit == '\\') {
							++i;
							if (i < length) {
								sb.append(code.charAt(i));
							}
						} else if (lit != '"') {
							sb.append(lit);
						}
					}
				} while (lit != '"' && i<length);
				LispNode str = new LispNode(sb.toString(), offset, i);
				str.isString = true;
				curr.params.add(str);
				sb = new StringBuilder();
			}  else if (c == ';') {
 				int i0 = i;
  				char lit = ';';
 				StringBuilder sbtmp = new StringBuilder();
  				do {
 					sbtmp.append(lit);
  					++i;
  					lit = code.charAt(i);
  				} while (lit != '\n' && i<length);
 				ret.addComment(sbtmp.toString(),i0,i-1);
				if (i >= length) {
					--i;
				}
			} else if (c == '#') {
				if (i < length-1) {
					if (code.charAt(i+1) == '|' && i < length-2) {
 						int i0 = i;
 						StringBuilder sbtmp = new StringBuilder();
 						sbtmp.append('#');
  						++i;
 						char lit = '|';
  						boolean done = false;
  						do {
 							sbtmp.append(lit);
							++i;
							lit = code.charAt(i);
							if (lit == '|' && i<length-1) {
								if (code.charAt(i+1) == '#') {
									++i;
									done = true;
								}
							}
						} while (!done && i<length-1);
  						ret.addComment(sbtmp.toString(),i0,i-1);
						
					} else if (code.charAt(i+1) == '\\' && i < length-2) {
						int offset = i;
						i += 2;
						sb.append(code.charAt(i));
						LispNode str = new LispNode(sb.toString(), offset, i);
						str.isString = true; // close enough
						curr.params.add(str);
						sb = new StringBuilder();
					}
				}
			} else if (Character.isWhitespace(c)) {
				if (!sb.toString().equals("")) {
					curr.params.add(new LispNode(sb.toString(), i-sb.toString().length(), i-1));
					sb = new StringBuilder();
				}
			} else {
				sb.append(c);
			}
			
		} // for
		// Make sure we get the endOffset set on everyone even when our sexps are incomplete
		// This is important for LispUtil.getParameterNumber, and thus for LispIndenter
		if (curr.endOffset == 0) {
			curr.endOffset = i;
		}
		while (!s.empty()) {
			curr = s.peek();
			if (curr.endOffset == 0) {
				curr.endOffset = i;
			}
			s.pop();
		}
		return ret;
	}
	
	
	public class LispComment {
 		public int offset = 0;
 		public int endOffset = 0;
 		public String value = "";
 		
 		public LispComment() {
 		}
 		
 		public LispComment(String val, int offset,int endOffset) {
 			value = val;
 			this.offset = offset;
 			this.endOffset = endOffset;
 		}		
 	}

	
}
