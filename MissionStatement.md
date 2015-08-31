# Preface #

Starting with lisp is a daunting task. First you download SBCL. Then you check out SBCL from cvs, and use the version you just downloaded to compile a new one. Then you download some flavor of Emacs and install it. Then you do a cvs checkout of Slime (the latest release may not work). Then you customize your .emacs file so that slime can hook up to lisp. Then you fire up Emacs, cross your fingers, and pray that it all worked.

If you're lucky, you have a development environment straight out of the 70's to work with. This, you are told, is the most advanced and powerful programming language around. Somehow, you're somewhat skeptical.


New lispers shouldn't have to go through all of this just to get started with the language. New lispers should not have to learn Emacs to learn Lisp. And lispers should definitely not have to ''use'' Emacs to use Lisp. It's time to decouple the two.


# Goals #
  * Provide a high quality Lisp IDE with all the amenities that users of modern IDE's have come to expect.
  * Ensure that it works straight out of the box.