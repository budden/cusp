## Matters of Style ##


  * I realize that typically one goes with a domain name when forming a Java package, but I don't actually have a domain of my own, so I went with the one that boosted my ego. Now it's easier to keep it that way than change everything.

  * Other Eclipse plugins like to divide every little feature into a separate plugin. I don't really see the point in this, especially since the pieces of Cusp don't make sense apart from each other, so I went with the monolithic architecture.

  * On the other hand, I eschewed Eclipse's love of one monolithic class to hold all constants. I find it just makes it difficult for me to find the constant relevant to the task at hand. So, constants go with the classes they relate to. Thus, the id for the Lisp Project Nature is in LispNature, not CuspConstants (which doesn't exist).

  * Some of the ways I do things are deprecated. Usually, this is because I was able to get it working the old way and couldn't figure out the new way.

  * I like to keep my curly braces on the same line as their associated statement (ie: "if {" ). Code with whichever you prefer. I don't care.


## Matters of Architecture ##

  * In a technical sense, there are actually two Lisp parsers in Cusp: the one the editor uses to colorize code, and the one I use (almost) everywhere else. The editor's parser is a typical Eclipse rule-based partitioner. LispParser, on the other hand, is a hand-rolled parser the splits everything up into LispNodes, making it easy for me to analyze the actual contents of that s-exp. This is heavily used in talking to Swank. It should be noted that partitions made by the editor also come in handy when doing various things from there (they make it easy to ignore comments and string contents, for example).

  * In a technical sense, there are actually _three_ Lisp parsers in Cusp. The methods of LispUtil do smaller Lisp parsing tasks, mostly related to finding the current expression or current keyword or current top-level expression. These tend to rely on the editor's partitions.

  * Why are all the hot-keys Alt+ something? It breaks the usual standard way of handling shortcuts, I know, but there are 2 main reasons for this: the good Ctrl+ shortcuts were all taken, and I wanted to keep them close to the Slime shortcuts (minus the chording, naturally). Alt+ offered a host of unused key combos. So use Alt+. If the user hates it, they can change it easily enough. (Do try to include the Slime key combo for those in the Emacs key configuration. Stubborn buggers).

## Matters of the Heart ##

  * Go after her, you fool! Don't make the same mistake I did! You'll regret it for the rest of your life if you let her slip away.  You'll only regret it for a few years if you don't.