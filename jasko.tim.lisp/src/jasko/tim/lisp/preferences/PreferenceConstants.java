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
	public static final String COLOR_UCW           = "colorUcw";

	public static final String SHOW_OUTLINE_HINT = "showOutlineHint";
	
	public static final String LISP_EXE = "lispExecutable";
	public static final String LISP_INI = "lispIni";
	public static final String USE_SITEWIDE_LISP = "usePluginLisp";
	public static final String MANAGE_PACKAGES = "managePackages";
	public static final String CONSOLE_COMPILER_LOG = "consoleCompilerLog";
	public static final String SYSTEMS_PATH = "systemsPath";
	public static final String USE_AUTO_BUILD = "useAutoBuild";
	public static final String USE_ECLIPSE_BUILD = "useEclipseBuild";
	public static final String USE_SLIME_BUILD = "useSlimeBuild";
	public static final String BUILD_TYPE = "buildType";
	public static final String SLIME_TYPE_PACK_EVAL = "slimeTypePackEval";
	
	public static final String USE_REMOTE_LISP = "useRemoteLisp";
	public static final String SSH_COMMAND = "sshCommand";
	public static final String REMOTE_HOST = "remoteHost";
	public static final String REMOTE_USER = "remoteUser";
	public static final String REMOTE_LISP_EXE = "remoteLispExecutable";
	public static final String REMOTE_INI = "remoteLispIni";
	public static final String REMOTE_LOCAL_PATH_PREFIX = "localPathPrefix";
	public static final String REMOTE_REMOTE_PATH_PREFIX = "remotePathPrefix";

	public static final String PAIR_EDIT_BRACKETS = "pairEditBrackets";
	public static final String PAIR_SMART_BRACKETS = "pairSmartBrackets";
	public static final String PAIR_EDIT_BRACES = "pairEditBraces";
	public static final String PAIR_EDIT_QUOTES = "pairEditQuotes";
	public static final String PAIR_EDIT_COMMENTS = "pairEditComments";
	public static final String AUTO_POPUP_COMPLETIONS = "autoPopupCompletions";
	public static final String AUTO_POPUP_COMPLETIONS_DELAY = "autoPopupCompletionsDelay";
    public static final String AUTO_INSERT_COMPLETIONS = "autoInsertCompletions";
    public static final String AUTO_FUZZY_COMPLETIONS = "autoFuzzyCompletions";
    public static final String AUTO_COMPLETIONS_NLIMIT = "autoCompletionsNLimit";
    public static final String DOCS_IN_COMPLETIONS = "docsInCompletions";
    public static final String MAX_HINT_LINES = "maxHintLines";
    public static final String ARGLIST_BELOW = "arglistBelow";
    
	public static final String SHOW_EVAL_IN_REPL = "showEvalInRepl";
	public static final String USE_CTRL_ENTER = "useCtrlEnter";
    public static final String REPL_INPUT_BG_COLOR = "REPLInputColorBG";
    public static final String REPL_INPUT_FG_COLOR = "REPLInputColorFG";
    public static final String DECORATE_REPL_INSPECTABLES = "decorateREPLInspectables";
    public static final String REPL_INSPECTABLE_BG_COLOR = "REPLInspectableColorBG";
    public static final String REPL_INSPECTABLE_FG_COLOR = "REPLInspectableColorFG";
    public static final String REPL_INSPECTABLE_UNDERLINE = "REPLInspectableUnderline";
    public static final String REPL_FONT_SIZE = "REPLFontSize";
    
    public static final String OUTLINE_SORT = "OutlineSort";
}
