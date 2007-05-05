package jasko.tim.lisp;

import java.net.*;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.*;
import org.eclipse.swt.graphics.Image;


public class LispImages {
	
	public static final String DEFCLASS = "defclass";
	public static final String DEFCONSTANT = "defconstant";
	public static final String DEFGENERIC = "defgeneric";
	public static final String DEFMACRO = "defmacro";
	public static final String DEFMETHOD = "defmethod";
	public static final String DEFOTHER = "defother";
	public static final String DEFPACKAGE = "defpackage";
	public static final String DEFSYSTEM = "defsystem";
	public static final String DEFPARAMETER = "defparameter";
	public static final String DEFSTRUCT = "defstruct";
	public static final String DEFTYPE = "deftype";
	public static final String DEFUN = "defun";
	public static final String DEFACTION = "defaction";
	public static final String DEFVAR = "defvar";
	public static final String DEFINE_CONDITION = "define-condition";
	public static final String DEFINE_ALIEN_ROUTINE = "define-alien-routine";
	public static final String DEFINE_ALIEN_VARIABLE = "define-alien-variable";
	public static final String DEFINE_ALIEN_TYPE = "define-alien-type";
	public static final String LAMBDA = "lambda";
	public static final String IN_PACKAGE = "in-package";
	public static final String OTHER = "other";
	public static final String SORT_ALPHA = "sort-alpha";
	public static final String SORT_TYPE = "sort-type";
	public static final String ERROR = "error";
	public static final String WARNING = "warning";
	public static final String RECONNECT = "reconnect";
	public static final String DISCONNECTED = "disconnected";
	public static final String CLEAR = "clear";
	public static final String THREAD_DEBUG = "thread-debug";
	public static final String THREAD_KILL = "thread-kill";
	public static final String REFRESH = "refresh";

	static {
		ImageRegistry imageReg = LispPlugin.getDefault().getImageRegistry();
	
		imageReg.put(LispImages.DEFCLASS, loadImageDescriptor("defclass.gif"));
		imageReg.put(LispImages.DEFCONSTANT, loadImageDescriptor("defconstant.gif"));
		imageReg.put(LispImages.DEFGENERIC, loadImageDescriptor("defgeneric.gif"));
		imageReg.put(LispImages.DEFMACRO, loadImageDescriptor("defmacro.gif"));
		imageReg.put(LispImages.DEFMETHOD, loadImageDescriptor("defmethod.gif"));
		imageReg.put(LispImages.DEFOTHER, loadImageDescriptor("defother.gif"));
		imageReg.put(LispImages.DEFPACKAGE, loadImageDescriptor("defpackage.gif"));
		imageReg.put(LispImages.DEFSYSTEM, loadImageDescriptor("defsystem.gif"));
		imageReg.put(LispImages.DEFPARAMETER, loadImageDescriptor("defparameter.gif"));
		imageReg.put(LispImages.DEFSTRUCT, loadImageDescriptor("defstruct.gif"));
		imageReg.put(LispImages.DEFUN, loadImageDescriptor("defun.gif"));
		imageReg.put(LispImages.DEFACTION, loadImageDescriptor("defaction.gif"));
		imageReg.put(LispImages.DEFVAR, loadImageDescriptor("defvar.gif"));
		imageReg.put(LispImages.DEFTYPE, loadImageDescriptor("deftype.gif"));
		imageReg.put(LispImages.DEFINE_CONDITION, loadImageDescriptor("define-condition.gif"));
		imageReg.put(LispImages.DEFINE_ALIEN_ROUTINE, loadImageDescriptor("define-alien-routine.gif"));
		imageReg.put(LispImages.DEFINE_ALIEN_VARIABLE, loadImageDescriptor("define-alien-variable.gif"));
		imageReg.put(LispImages.DEFINE_ALIEN_TYPE, loadImageDescriptor("define-alien-type.gif"));
		imageReg.put(LispImages.LAMBDA, loadImageDescriptor("lispNature.gif"));
		
		imageReg.put(LispImages.IN_PACKAGE, loadImageDescriptor("in-package.gif"));
		imageReg.put(LispImages.OTHER, loadImageDescriptor("other.gif"));
		
		imageReg.put(LispImages.SORT_ALPHA, loadImageDescriptor("sort-alpha.gif"));
		imageReg.put(LispImages.SORT_TYPE, loadImageDescriptor("sort-type.gif"));
		
		imageReg.put(LispImages.ERROR, loadImageDescriptor("error.gif"));
		imageReg.put(LispImages.WARNING, loadImageDescriptor("warning.gif"));
		
		imageReg.put(LispImages.RECONNECT, loadImageDescriptor("reconnect.gif"));
		imageReg.put(LispImages.DISCONNECTED, loadImageDescriptor("disconnected.gif"));
		
		imageReg.put(LispImages.CLEAR, loadImageDescriptor("clear.gif"));
		
		imageReg.put(LispImages.THREAD_DEBUG, loadImageDescriptor("thread-debug.gif"));
		imageReg.put(LispImages.THREAD_KILL, loadImageDescriptor("thread-kill.gif"));
		imageReg.put(LispImages.REFRESH, loadImageDescriptor("refresh.gif"));
		
	}
	
	
	public static Image getImageForType(String type) {
		type = type.replace("(", "").toLowerCase();
		if (type.startsWith("def")) {
			if (type.endsWith("class") || type.endsWith("component")) {
				return LispImages.getImage(LispImages.DEFCLASS);
			} else if (type.endsWith("constant")) {
				return LispImages.getImage(LispImages.DEFCONSTANT);
			} else if (type.endsWith("generic")) {
				return LispImages.getImage(LispImages.DEFGENERIC);
			} else if (type.endsWith("macro")) {
				return LispImages.getImage(LispImages.DEFMACRO);
			} else if (type.endsWith("method")) {
				return LispImages.getImage(LispImages.DEFMETHOD);
			} else if (type.endsWith("package")) {
				return LispImages.getImage(LispImages.DEFPACKAGE);
			} else if (type.endsWith("system")) {
				return LispImages.getImage(LispImages.DEFSYSTEM);
			} else if (type.endsWith("parameter")) {
				return LispImages.getImage(LispImages.DEFPARAMETER);
			} else if (type.endsWith("struct")) {
				return LispImages.getImage(LispImages.DEFSTRUCT);
			} else if (type.endsWith("fun")) {
				return LispImages.getImage(LispImages.DEFUN);
			} else if (type.endsWith("action")) {
				return LispImages.getImage(LispImages.DEFACTION);
			} else if (type.endsWith("var")) {
				return LispImages.getImage(LispImages.DEFVAR);
			} else if (type.endsWith("condition")) {
				return LispImages.getImage(LispImages.DEFINE_CONDITION);
			} else if (type.equals("define-alien-routine")) {
				return LispImages.getImage(LispImages.DEFINE_ALIEN_ROUTINE);
			} else if (type.equals("define-alien-variable")) {
				return LispImages.getImage(LispImages.DEFINE_ALIEN_VARIABLE);
			} else if (type.equals("define-alien-type")) {
				return LispImages.getImage(LispImages.DEFINE_ALIEN_TYPE);
			} else if (type.endsWith("type")) {
				return LispImages.getImage(LispImages.DEFTYPE);
			} else { // Well, they're probably defining *something*
				return LispImages.getImage(LispImages.DEFOTHER);
			}
		} else if (type.equals("lambda")) {
			return LispImages.getImage(LispImages.LAMBDA);
		} else if (type.equals("in-package")) {
			return LispImages.getImage(LispImages.IN_PACKAGE);
		} else {
			return LispImages.getImage(LispImages.OTHER);
		}
	}
	
	
	
	public static Image getImage(String id) {
		ImageRegistry ir = LispPlugin.getDefault().getImageRegistry(); 
		return ir.get(id);
	}
	
	public static ImageDescriptor getImageDescriptor(String id) {
		ImageRegistry ir = LispPlugin.getDefault().getImageRegistry();
		return ir.getDescriptor(id);
	}
	
	
	/**
	 * Loads an ImageDescriptor from a file in the icons directory
	 * 
	 * @param name - name of the file to be loaded
	 * @return An ImageDescriptor representing the image in the file
	 */
	private static ImageDescriptor loadImageDescriptor(String name) {
		try {
			 URL installURL = Platform.getBundle("jasko.tim.lisp").getEntry("/");
			 URL url = new URL(installURL, "icons/" + name);
			 return ImageDescriptor.createFromURL(url);
		} catch (MalformedURLException e) {
			 // should not happen
			 return ImageDescriptor.getMissingImageDescriptor();
		}
	}
}
