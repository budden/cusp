package jasko.tim.lisp.util;


import jasko.tim.lisp.editors.*;
import jasko.tim.lisp.swank.LispComment;
import jasko.tim.lisp.swank.LispNode;
import jasko.tim.lisp.swank.LispParser;

import java.util.*;

import org.eclipse.jface.text.*;

public class LispUtil {
	
	public static int getTopLevelOffset(IDocument doc, int offset) {
		return getTopLevelOffset(doc,offset,false);
	}
	
	public static int getTopLevelOffset(IDocument doc, int offset, 
			boolean withSections) {
		try {
			// SK: This procedure finds topLevelOffset the following way:
			// 1. Find line of offset.
			// 2. Starting from this line and going backward,
			//    check if '(' is first character of the line. 
			//    If yes, this is topLevelOffset.
			//    if withSections and line starts with SECTION_START also toplevel
			int line = doc.getLineOfOffset(offset);
			for( int i = line; i >= 0; --i ){
				int lineOffset = doc.getLineOffset(i);
				if( doc.getChar(lineOffset) == '(' ){
					return lineOffset;
				} else if ( withSections 
						&& lineOffset+LispComment.SECTION_START.length() 
						      < doc.getLength()-1 
						&& doc.get(lineOffset, 
								LispComment.SECTION_START.length())
								  .equals(LispComment.SECTION_START)){
					return lineOffset;
				}
			}
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		return -1;
	}
	
	public static int[] getTopLevelRange(IDocument doc, int offset) {
		int[] range = getCurrentFullExpressionRange (doc, offset);
		if( range == null ){
			return null;
		}
		return getCurrentFullExpressionRange(doc,
				getTopLevelOffset(doc,range[0])+1);
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
	
	public static int[] getCurrentFullWordRange(IDocument doc, int offset, 
			boolean ignoreComments) {
        int start = -1, end = -1;
		String source = doc.get();

		try{
			if( !ignoreComments && offset < doc.getLength() 
					&& doc.getPartition(offset)
					     .getType().equals(LispPartitionScanner.LISP_COMMENT)) {
				ITypedRegion partition = doc.getPartition(offset); 
				start = partition.getOffset();
				int length = partition.getLength();
				return new int[] {start, length};
			} else {
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
		} catch (BadLocationException e) {
			e.printStackTrace();
			return null;
		}
	}
    
	public static int[] getCurrentFullWordRange (IDocument doc, int offset) {
		return getCurrentFullWordRange(doc,offset,true);
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

    //take different presentations of pkg (i.e. ":mypack", "'mypack", "MYPACK", "\"MYPACK\""
    //and formats it into form "MYPACK" - same format as in return of swank.getAvailablePackages
    //the pkg string should be valid package string, no checks for validity are made
    public static String formatPackage(String pkg){
    	if("".equals(pkg) || pkg == null) {
    		return "";
    	}
    	String res = pkg;
    	char ch0 = pkg.charAt(0);
    	if('\'' == ch0 || ':' == ch0){
    		res = pkg.substring(1).toUpperCase();
    	} else if (pkg.startsWith("\"") && pkg.endsWith("\"") && pkg.length() > 4){
    		res = pkg.substring(2, pkg.length()-3).toUpperCase();
    	} else {
    		res = pkg.toUpperCase();
    	}
    	return res;
    }
    
    // get package definition closest to offset
	public static String getPackage(String code, int offset){
		LispNode contents = LispParser.parse(code + "\n");
		String res = "";
		for (int i=0; i<contents.params.size(); ++i) {
			LispNode sexp = contents.params.get(i);
			
			if(sexp.offset >= offset){
				return res;
			}
			
			if (sexp.get(0).value.equalsIgnoreCase("in-package")) {
				res = sexp.get(1).value;
			}
		}
		
		return res;
	}
	
	public static TopLevelItem getTopLevelItem(LispNode exp, String pkg, int offset){
		TopLevelItem item = new TopLevelItem();
		
		item.type = exp.get(0).value.toLowerCase();
		item.name = exp.get(1).toLisp();
		item.nameOffset = exp.get(1).offset + offset;
		item.offset = exp.offset + offset;
		item.offsetEnd = exp.endOffset + offset;
		item.pkg = pkg;
		if (! item.type.startsWith("def")) {
			item.name = item.type;
			if (item.type.equals("in-package")) {
				item.name = "in-package " + exp.get(1).toLisp();
			}
		} else if (item.type.equals("defstruct")) {
			LispNode name = exp.get(1); 
			if (!name.value.equals("")) {
				item.name = name.value;
			} else {
				item.name = name.get(0).value;
			}
		} else if (item.type.equals("defmethod")) {
			String name = exp.get(2).toLisp();
			if (name.startsWith(":")) {
				item.name += " " + name + " " + exp.get(3).toLisp();
			} else {
				item.name += " " + name;
			}
		}
		
		if (item.name.equals("")) {
			if (exp.params.size() >= 2) {
				if (exp.get(1).toLisp().startsWith(":")) {
					item.name = 
						exp.get(1).toLisp() + " " + exp.get(2).toLisp();
				} else {
					item.name = exp.get(1).toLisp();
				}
			}
		}
	
		return item;
	}
	
	public static TopLevelItem getSectionItem(LispComment comment, int offset){
		if ( comment.isSectionComment() ) {
			TopLevelItem item = new TopLevelItem();
			
			item.type = "section";
			item.name = comment.SectionName();
			item.offset = comment.offset;
			return item;
		} else {
			return null;
		}
	}
	
	public static ArrayList<TopLevelItem> getTopLevelItems(LispNode file, 
			String pkg, int offset){
		ArrayList<TopLevelItem> items = 
			new ArrayList<TopLevelItem>(file.params.size());
		String curpkg = pkg;
		for (LispNode exp: file.params) {
			TopLevelItem item = getTopLevelItem(exp,curpkg,offset);
			if( item.type.equals("in-package") ){
				curpkg = LispUtil.formatPackage(exp.get(1).toLisp());
			}

			if (! item.name.equals("")) {
				items.add(item);
			}
		}
		
		// add section comments
		for ( LispComment comment: file.comments ) {
			TopLevelItem item = getSectionItem(comment,offset);
			if( item != null && !item.name.equals("")){
				items.add(item);				
			}
		}		
		return items;
	}

	public static ArrayList<TopLevelItem> getTopLevelItems(LispNode file, String pkg){
		return getTopLevelItems(file,pkg,0);
	}
}
