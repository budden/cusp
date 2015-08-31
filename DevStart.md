## Getting Cusp running from source ##

  * Check out the repository with the command "svn checkout http://cusp.googlecode.com/svn/ cusp-read-only"
  * Import the project into Eclipse (File > Import > Existing Projects into Workspace)
  * Create an sbcl directory in the project. Put sbcl in there. Make sure to include the sbcl executable.
  * Run Cusp as a plugin, and behold the glory! (Run > Run... > Eclipse Application > New)
  * Sometimes you need to provide .classpath file to compile and launch plugin from development environment. Here is what you can do: Go to File->New->Project->Plug-in Project Keep everything default and create temporary project. Then go to project folder and find .classpath file. Copy it to jasko.tim.lisp folder before importing plugin to Eclipse using File->Import->Existing Projects into Workspace.
  * On Windows it may be necessary to edit jasko.tim.lisp\build.properties and remove trailing slashes from all directory names. Also Eclipse Plug-in libraries need to be added as an external dependency (project properties->Java Build Path->Libraries->Add Library->Plug-in Dependencies)

## The following references will help you immensely ##

  * [Eclipse API](http://www.jlab.net/eclipse/doc/3.1/)
  * [Eclipse Platform Plugin Developer Guide](http://help.eclipse.org/help32/index.jsp)

  * [SWT Snippets](http://www.eclipse.org/swt/snippets/)
  * [Official Eclipse Faqs](http://wiki.eclipse.org/index.php/Eclipse_FAQs#Implementing_Support_for_Your_Own_Language)

  * [Java 5 API](http://java.sun.com/j2se/1.5.0/docs/api/)



## Notes for development ##

  * If you're looking to hook into swank commands not currently used by Cusp, the fastest way to find them is to load up Slime in Linux, listen to the loopback on Ethereal, and perform the function you want to emulate. Then look at the sniffed packets to find out what swank command you want.


## Submitting your changes ##

Bundle your changes up into a patch and submit it to the newsgroup. Somebody with commit access will look it over and add it in if it looks good. Do that a few times and if it looks like you're on a roll, I'll probably give you commit access.