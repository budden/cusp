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
		
		//System.out.println("*parsing:" + code.charAt(0));
		LispNode ret = new LispNode(0);
		Stack<LispNode> s = new Stack<LispNode>();
		
		LispNode curr = ret;
		s.push(curr);
		
		StringBuilder sb = new StringBuilder();
		int length = code.length();
		
		for (int i=start; i<length; ++i) {
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
			} else if (c == ';') {
				char lit = ';';
				do {
					++i;
					lit = code.charAt(i);
				} while (lit != '\n' && i<length);
				if (i >= length) {
					--i;
				}
			} else if (c == '#') {
				if (i < length-1) {
					if (code.charAt(i+1) == '|' && i < length-2) {
						++i;
						char lit;
						boolean done = false;
						do {
							++i;
							lit = code.charAt(i);
							if (lit == '|' && i<length-1) {
								if (code.charAt(i+1) == '#') {
									++i;
									done = true;
								}
							}
						} while (!done && i<length-1);
						
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
		return ret;
	}
	

	
}
