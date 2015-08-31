Want to help develop Cusp? Here are some good places to start!


## Easier ##

  * Bind "Find Callees" to "Open Call Hierarchy" as the equivalent functionality is called in the Java editor.

  * Make Cusp builds for more platforms. OS X on the PPC in particular is oft-requested. The hardest part of this task is owning the platform in question.

  * Enable the user to connect to a remote swank (also see http://groups.google.com/group/cusp-development/browse_frm/thread/470a95050531387b)

  * Either bundle a copy of the hyperspec with Cusp, or add a field to Cusp's prefs to allow users to refer to their existing local hyperspec


## Moderate ##

  * Optional foom-style highlighting of the current sexp, or an entire file: http://lemonodor.com/archives/001207.html

  * Change the Lisp Navigator to use Eclipse's new Navigator architecture. (See http://scribbledideas.blogspot.com/2006/05/building-common-navigator-based-viewer.html)

  * Paredit functionality.

  * Add click-insert functionality to the REPL

  * Add an optional "interleaved" REPL view, where output and input are displayed in the same view (or, just simulate this with a dynamically-expanding REPL editor area)


## Harder ##

  * Add the ability to evaluate expressions within debugger frames.

  * Refactor debugger to Eclipse Debug Model http://www.eclipse.org/articles/Article-Debugger/how-to.html

  * Add support for remote file operations using RSE http://dsdp.eclipse.org/help/latest/index.jsp?topic=/org.eclipse.rse.doc.user/gettingstarted/g_start.html

  * Allow each project have its own REPL, possibly running different implementations

  * Get something better than Xref for finding references

  * Add code coverage tool

  * Inspect values of globals and constants in a popup

  * Add spell checker to editor

  * Refactoring! (at least simplest change name refactoring)

  * Actions for export symbol from package, intern symbol, add library to project

  * Tutorials for GUI, Web, database applications

  * Add asdf-install support (note: asdf-install does not work on windows, so this would need to be largely a Java implementation).

## Wildest Dreams ##