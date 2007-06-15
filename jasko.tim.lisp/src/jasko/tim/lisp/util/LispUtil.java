package jasko.tim.lisp.util;


import jasko.tim.lisp.editors.*;
import jasko.tim.lisp.swank.LispNode;
import jasko.tim.lisp.swank.LispParser;

import java.util.*;

import org.eclipse.jface.text.*;

public class LispUtil {
	
	public static int getTopLevelOffset(IDocument doc, int offset) {
		try {
			int line = doc.getLineOfOffset(offset);
			ArrayList<Integer> lineOffsets = new ArrayList<Integer>();
			for (int i=line; i>=0; --i) {
				lineOffsets.add(doc.getLineOffset(i));
			}
			Collections.sort(lineOffsets);
			
			for (int i=offset; i>=0; --i) {
				if (Collections.binarySearch(lineOffsets, i) >= 0) {
					if (doc.getChar(i) == '(') {
						return i;
					}
				}
			}
			
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		return -1;
	}
	
	public static String getTopLevelExpression(IDocument doc, int offset) {
		int expOffset = getTopLevelOffset(doc, offset);
		if (expOffset >= 0) {
			return getExpression(doc, expOffset);
		}
		return "";
	}
	

	public static String getExpression(IDocument doc, int offset) {
		StringBuilder ret = new StringBuilder();
		int open = 0;
		int close = 0;
		try {
			for (int i=offset; i<doc.getLength(); ++i) {
				String partition = doc.getPartition(i).getType();
				if (!partition.equals(LispPartitionScanner.LISP_COMMENT)) {
					char c = doc.getChar(i);
					ret.append(c);
					if (!partition.equals(LispPartitionScanner.LISP_CHARACTER) &&
							!partition.equals(LispPartitionScanner.LISP_STRING)) {
						if (c == '(') {
							++open;
						} else if (c == ')') {
							++close;
							if (close == open) {
								return ret.toString();
							}
						}
					}
				}
			}
			return ret.toString();
		} catch (BadLocationException e) {
			e.printStackTrace();
			return "";
		}
	}
	
	public static String getCurrentWord(IDocument doc, int offset) {
		String source = "";
		try {
			source = doc.get(0, offset);
		} catch (BadLocationException e) {
			e.printStackTrace();
			return "";
		}
		StringBuilder sb = new StringBuilder();
		for (int i=source.length()-1; i >=0; --i) {
			char c = source.charAt(i);
			if (Character.isWhitespace(c) || c == '(' || c ==')' || c=='\'') {
				return sb.toString();
			} else {
				sb.insert(0, c);
			}
		}
		
		return sb.toString();
	}
	
//	public static int[] getCurrentFullWordRange 
	                  
	public static int[] getCurrentFullWordRange (IDocument doc, int offset) {
        int start = -1, end = -1;
		String source = doc.get();

		for (int i = offset - 1; i >= 0; --i) {
			char c = source.charAt(i);
			if (Character.isWhitespace(c) || c == '(' || c ==')') {
				break;
			} else {
                start = i;
			}
		}
        
		for (int i = offset; i < source.length(); ++i) {
			char c = source.charAt(i);
			if (Character.isWhitespace(c) || c == '(' || c ==')') {
                break;
			} else {
                end = i + 1;
			}
		}
        
        if (start == -1 && end == -1) {
            return null;
        } else if (start == -1) {
            start = offset;
        } else if (end == -1) {
            end = offset;
        }
        
        return new int[] { start, end - start };
	}
    
    public static String getCurrentFullWord (IDocument doc, int offset) {
        int[] range = getCurrentFullWordRange(doc, offset);
        if (range != null) {
            try {
                return doc.get(range[0], range[1]);
            } catch (BadLocationException e) {
                e.printStackTrace();
            }
        }
        
        return "";
    }
    
    public static int getParameterNumber(IDocument doc, int offset, FunctionInfo fi) {
    	try {
    		//System.out.println("offsets " + fi.name + ":" + fi.offset + "," + (offset - fi.offset) );
    		if (fi.offset <= 0) {
    			return 0;
    		}
    		
    		offset = offset - fi.offset;
			String current = doc.get(fi.offset, offset);
			LispNode node = LispParser.parse(current).get(0);
			//System.out.println("-node:" + node);
			for (int i=0; i < node.params.size(); ++i) {
				LispNode param = node.params.get(i);
				if (param.offset <= offset && param.endOffset >= offset) {
					return i;
				} else if (param.offset > offset) {
					return i-1;
				}
			}
			return node.params.size();
		} catch (BadLocationException e) {
			e.printStackTrace();
			return 0;
		}
    }
	
	public static class FunctionInfo {
		public FunctionInfo(String name, int offset) {
			this.name = name;
			this.offset = offset;
		}
		public String name;
		public int offset;
	}
	
	public static FunctionInfo getCurrentFunctionInfo(IDocument doc, int offset) {
		if (offset <= 0) {
			return new FunctionInfo("", -1);
		}
		StringBuilder sb = new StringBuilder();
		//String acc="";
		int open = 0;
		int close = 0;
		try {
			for (int i=offset-1; i >=0; --i) {
				char c = doc.getChar(i);
				if (c == ')' && doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
					++close;
					sb = new StringBuilder();
				} else if (c == '(' && doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
					++open;
					if (open > close) {
						return new FunctionInfo(sb.toString(), i);
					} else {
						sb = new StringBuilder();
					}
				} else if (Character.isWhitespace(c)) {
					sb = new StringBuilder();
				} else {
					sb.insert(0, c);
				}
			
			}
			return new FunctionInfo("", -1);
		} catch (BadLocationException e) {
			e.printStackTrace();
			return new FunctionInfo("", -1);
		}
	}
	
	public static String getCurrentFunction(IDocument doc, int offset) {
		String source = "";
		
		try {
			source = doc.get(0, offset);
		} catch (BadLocationException e) {
			e.printStackTrace();
			return "";
		}
		StringBuilder sb = new StringBuilder();
		int open = 0;
		int close = 0;
		try {
			for (int i=source.length()-1; i >=0; --i) {
				char c = source.charAt(i);
				if (c == ')' && doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
					++close;
					sb = new StringBuilder();
				} else if (c == '(' && doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
					++open;
					if (open > close) {
						return sb.toString();
					} else {
						sb = new StringBuilder();
					}
				} else if (Character.isWhitespace(c)) {
					sb = new StringBuilder();
				} else {
					sb.insert(0, c);
				}
			
			}
			return "";
		} catch (BadLocationException e) {
			e.printStackTrace();
			return "";
		}
	}
	

	public static boolean doParensBalance(IDocument doc) {
		return doParensBalance(doc, 0, doc.getLength());
	}
	
	public static boolean doParensBalance(IDocument doc, int start, int end) {
		String code = doc.get();
		int open = 0;
		int close = 0;
		
		for (int i = start; i < end; ++i) {
			char c = code.charAt(i);
			if (c == ')') {
				try {
					if (doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
						++close;
						if (close > open) {
							return false;
						}
					}
				} catch (BadLocationException e) {
					e.printStackTrace();
				}
			} else if (c == '(') {
				try {
					if (doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
						++open;
					}
				} catch (BadLocationException e) {
					e.printStackTrace();
				}
				
			}
		}
		
		return (open == close);
	}
	
	public static int findOpenParen(IDocument doc, int offset) {
		String code = doc.get();
		int open = 0;
		int close = 0;
		for (int i=offset-1; i>=0; --i) {
			char c = code.charAt(i);
			try {
				if (c == ')' && doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
					++close;
				} else if (c == '(' && doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
					++open;
					if (open > close) {
						return i;
					}
				}
			} catch (BadLocationException e) {
				e.printStackTrace();
			}
		}
		
		return -1;
	}
    
    /**
     * Returns a 2-element integer array indicating the range in the current document of the smallest complete
     * s-expression that encloses the given offset.  If no such s-expression is found, this function returns null.
     */
    public static int[] getCurrentFullExpressionRange (IDocument doc, int offset) {
        return getCurrentFullExpressionRange(doc, offset, 0);
    }
    
    /**
     * Returns a 2-element integer array indicating the range in the current document of the smallest complete
     * s-expression that encloses the given select range.  If no such s-expression is found, this function returns null.
     */
    public static int[] getCurrentFullExpressionRange (IDocument doc, int offset, int selectionLength) {
        int begin = findOpenParen(doc, offset);
        if (begin >= 0) {
            int end = findCloseParen(doc, offset, offset + selectionLength);
            if (end >= 0) {
                return new int[] { begin, end - begin + 1 };
            }
        }
        
        return null;
    }
    
	public static String getCurrentFullExpression(IDocument doc, int offset) {
        int[] range = getCurrentFullExpressionRange(doc, offset);
        if (range == null) {
            return "";
        } else {
            try {
                return doc.get(range[0], range[1]);
            } catch (BadLocationException e) {
                return "";
            }
        }
	}
	
	/**
	 * Guesses as to the current expression, which probably hasn't been finished yet
	 * Useful for auto-assist. Not so useful for compilation.
	 */
	public static String getCurrentUnfinishedExpression(IDocument doc, int offset) {
		int begin = findOpenParen(doc, offset);
		if (begin >= 0) {
			int end = findCloseParen(doc, begin+1);
			if (end < 0) {
				end = offset;
			}
			if (end >= 0) {
				try {
					return doc.get(begin, end-begin);
				} catch (BadLocationException e) {
					e.printStackTrace();
				}
			}
		}

		return "";
	}
	
	
	public static int countUnclosedParens(IDocument doc, int offset) {
		String code = doc.get();
		int open = 0;
		int close = 0;
		
		for (int i=0; i<offset; ++i) {
			char c = code.charAt(i);
			if (c == ')') {
				try {
					if (doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
						++close;
					}
				} catch (BadLocationException e) {
					e.printStackTrace();
				}
			} else if (c == '(') {
				try {
					if (doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
						++open;
					}
				} catch (BadLocationException e) {
					e.printStackTrace();
				}
				
			}
		}
		return open - close;
	}
	
    public static int findCloseParen (IDocument doc, int offset) {
        return findCloseParen(doc, offset, offset);
    }
    
	public static int findCloseParen (IDocument doc, int offset, int minSearchOffset) {
		int open = 0;
		int close = 0;
		String code = doc.get();
		for (int i = offset; i < code.length(); ++i) {
			try {
				char c = code.charAt(i);
				if (c == ')' && doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
					++close;
					if (i >= minSearchOffset && close - open == 1) {
						return i;
					}
				} else if (c == '(' && doc.getPartition(i).getType().equals(IDocument.DEFAULT_CONTENT_TYPE)) {
					++open;
				}
			} catch (BadLocationException e) {
				e.printStackTrace();
			}
		}
		
		return -1;
	}
    
    /**
     * Returns a 2-element integer array indicating the range (start, length) in the current document of the current "active"
     * s-expression.  If there is no current s-expression, this function returns null.
     */
    public static int[] getCurrentExpressionRange (IDocument doc, int offset) throws BadLocationException {
        if (offset < doc.getLength() && doc.getChar(offset) == '(') {
            int end = findCloseParen(doc, offset + 1);
            if (end > -1) return new int[] { offset, end - offset + 1 };
        } else if (offset > 0 && doc.getChar(offset - 1) == ')') {
            int start = findOpenParen(doc, offset - 1);
            if (start > -1) return new int[] { start, offset - start };
        } else {
            return getCurrentFullExpressionRange(doc, offset);
        }
        
        return null;
    }

    public static String getCurrentExpression (IDocument doc, int offset, int selLength) {
        try {
            if (selLength > 0) {
                if (doParensBalance(doc, offset, offset + selLength)) {
                    return doc.get(offset, selLength);
                } else {
                    System.out.println("Not evaluating current selection; parens do not balance.");
                }
            } else {
                int[] range = getCurrentExpressionRange(doc, offset);
                if (range != null) return doc.get(range[0], range[1]);
            }
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
        
        return "";
    }
}
