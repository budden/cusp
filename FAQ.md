## Cusp cannot start/connect to lisp. How do I resolve this? ##
Try one of the procedures described in the following messages:

- http://groups.google.com/group/cusp-development/msg/2284c7efc0e1259f

- http://groups.google.com/group/cusp-development/msg/721a012e3af35dc8

## When trying to start Cusp, I get a java.lang.NoClassDefFoundError error or "Problems opening perspective 'jasko.tim.lisp.perspective.LispPerspectiveFactory'". What's the deal? ##
You have an old version of Java. Make sure you have the latest Java _and_ make sure that Eclipse is running under it.
Feel dumb now, don't you? Just keep in mind that enough people had this problem that it got into the FAQ.

Still not working? You're on Linux, aren't you? I bet you think you upgraded your Java installation but really didn't (the Linux Java installer sucks). In Eclipse, open the Error Log view, and double-click on the error. See the java.version? Bet it's older than 1.5. (You can also get at this information in Help > About Eclipse SDK > Configuration Details.)

Good luck upgrading properly. http://ubuntuforums.org/showpost.php?p=1065271&postcount=8 has more info.

If all else fails, run eclipse with the -vm parameter and point to the new JRE directory. (example: eclipse -vm /usr/java/jdk1.6.0\_01/bin/java)


---



## I tried starting up Cusp, but the REPL just says to me "Unable to create view: An unexpected exception was thrown. java.lang.NullPointerException etc etc." Also I'm on Windows. ##
I'm afraid you're running into an SBCL/Win32 bug. Basically, Cusp can't get SBCL to start.

You may have luck starting SBCL manually before you run Cusp.
First, set SBCL\_HOME to the Cusp/sbcl directory. Then try starting sbcl with a command like "sbcl --dynamic-space-size 112" (you can try a smaller number, even). If you can get sbcl running, the following commands will put it in a state where Cusp can connect to it:

```
(load "path-to-cusp/slime/swank-loader.lisp")
(swank:create-server :port 4005)
```


More information on the issue you're probably encountering can be found at [http://www.archivesat.com/Steel\_Bank\_Common\_Lisp\_(SBCL)/thread2297905.htm](http://www.archivesat.com/Steel_Bank_Common_Lisp_(SBCL)/thread2297905.htm) and [http://groups.google.pl/group/comp.lang.lisp/msg/e03a8ebf612f6a0d](http://groups.google.pl/group/comp.lang.lisp/msg/e03a8ebf612f6a0d)

### I get the same error, but I'm not on Windows: ###
Maybe the automatic installation of sbcl failed. If the sbcl.zip file is already in "plugins/sbcl`_`linux`_`.../" extract it manually and make sbcl executable. Otherwise install sbcl yourself. Don't forget to set up implementation paths in eclipse.


---



## I upgraded to the latest version of Cusp, but none of the nifty new features are showing up. ##
Start Eclipse with the clean option (eclipse -clean).



---



## Why isn't my platform supported? ##
Because I don't own any computers that run your platform. However, if you can get a copy of SBCL for your platform, you should be able to get Cusp running. See below.


---


## How do I get Cusp running on my unsupported platform? ##
Theoretically, if you can run SBCL on your platform, you should be able to run Cusp on it, too. To do so, first install yourself a working copy of SBCL. Now, look at the sbcl folder in your Cusp directory. Throw this out. Replace it with the sbcl folder you installed, which is likely at /usr/local/lib/sbcl . Please note that you will also need to place the sbcl executable in there as well, which tends to be located at /usr/local/bin/sbcl . People have also reported success using symlinks instead of actually copying the files.
Try starting Cusp now. With luck, things work. If they don't work on the first try, give it one more run. Sometimes it's a little slow on the first go.
Note: Updating Slime is not advised, as the swank protocol changes semi-often, and that will most assuredly break Cusp.



---



## Is Cusp open source? ##
Yes. Info on how to access the svn repository is at http://code.google.com/p/cusp/source/checkout . The wiki lives right here.



---



## What license is Cusp released under? ##
Cusp is under a BSD-style license. In essence, you are free to use it for whatever you want, commercial or otherwise. In return, I am in no way liable for any terrible thing it might do to your machine. Or, as I say on the front page, "free of charge and free of warranty."