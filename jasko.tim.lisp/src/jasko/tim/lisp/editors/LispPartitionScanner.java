package jasko.tim.lisp.editors;

import org.eclipse.jface.text.rules.*;

public class LispPartitionScanner extends RuleBasedPartitionScanner {
	public final static String LISP_DEFAULT = "__lisp_default";
	public final static String LISP_COMMENT = "__lisp_comment";
	public final static String LISP_STRING = "__lisp_string";
	public final static String LISP_CHARACTER = "__lisp_character";
	
	public final static String[] PARTITIONS = {
		LISP_STRING, LISP_COMMENT, LISP_CHARACTER
	};

	public LispPartitionScanner() {

		IPredicateRule[] rules = new IPredicateRule[9];

		IToken lispComment = new Token(LISP_COMMENT);
		rules[0] = new MultiLineRule("#|", "|#", lispComment);
		rules[1] = new EndOfLineRule(";", lispComment);

		IToken lispCharacter = new Token(LISP_CHARACTER);
		rules[2] = new WordPatternRule(new CharacterDetector(), "#\\", "\"", lispCharacter);
		rules[3] = new WordPatternRule(new CharacterDetector(), "#\\", "(", lispCharacter);
		rules[4] = new WordPatternRule(new CharacterDetector(), "#\\", ")", lispCharacter);
		rules[5] = new WordPatternRule(new CharacterDetector(), "#\\", ";", lispCharacter);
		rules[6] = new WordPatternRule(new CharacterDetector(), "#\\", "return", lispCharacter);
		
		
		IToken lispString = new Token(LISP_STRING);
		rules[7] = new MultiLineRule("\"", "\"", lispString, '\\');
		rules[8] = new EndOfLineRule("\"", lispString);
		

		setPredicateRules(rules);
	}
}
