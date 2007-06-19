package jasko.tim.lisp.preferences;


/**
 * Constant definitions for plug-in preferences
 */
public interface PreferenceConstants {
	public static final String COLOR_COMMENT       = "colorComment";
	public static final String COLOR_CONSTANT      = "colorConstant";
	public static final String COLOR_DEFAULT       = "colorDefault";
	public static final String COLOR_KEYWORD       = "colorKeyword";
	public static final String COLOR_GLOBAL        = "colorGlobal";
	public static final String COLOR_NUMBER        = "colorNumber";
	public static final String COLOR_PAREN         = "colorParen";
	public static final String COLOR_PARAMS        = "colorParams";
	public static final String COLOR_SENT_MESSAGE  = "colorSentMessage";
	public static final String COLOR_STRINGS       = "colorStrings";
	public static final String COLOR_SYMBOL        = "colorSymbol";
	public static final String COLOR_UCW        = "colorUcw";

	public static final String LISP_EXE = "lispExecutable";
	public static final String LISP_INI = "lispIni";
	public static final String USE_SITEWIDE_LISP = "usePluginLisp";
	public static final String MANAGE_PACKAGES = "managePackages";
	public static final String CONSOLE_COMPILER_LOG = "consoleCompilerLog";
	public static final String SYSTEMS_PATH = "systemsPath";

	public static final String PAIR_EDIT_BRACKETS = "pairEditBrackets";	
	public static final String PAIR_EDIT_QUOTES = "pairEditQuotes";	
	public static final String PAIR_EDIT_COMMENTS = "pairEditComments";	
	public static final String AUTO_POPUP_COMPLETIONS = "autoPopupCompletions";
	public static final String AUTO_POPUP_COMPLETIONS_DELAY = "autoPopupCompletionsDelay";
    public static final String AUTO_INSERT_COMPLETIONS = "autoInsertCompletions";
    public static final String AUTO_FUZZY_COMPLETIONS = "autoFuzzyCompletions";
    public static final String AUTO_COMPLETIONS_NLIMIT = "autoCompletionsNLimit";
    public static final String DOCS_IN_COMPLETIONS = "docsInCompletions";
    public static final String MAX_HINT_LINES = "maxHintLines";
    
    public static final String DECORATE_REPL_INSPECTABLES = "decorateREPLInspectables";
    public static final String REPL_INSPECTABLE_BG_COLOR = "REPLInspectableColorBG";
    public static final String REPL_INSPECTABLE_FG_COLOR = "REPLInspectableColorFG";
    public static final String REPL_INSPECTABLE_UNDERLINE = "REPLInspectableUnderline";
}
