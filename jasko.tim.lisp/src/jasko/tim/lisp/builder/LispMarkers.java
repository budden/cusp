package jasko.tim.lisp.builder;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class LispMarkers {

	private static final String LISP_MARKER = "jasko.tim.lisp.lispMarker";
	private static final String LISP_COMPILE_MARKER = "jasko.tim.lisp.lispMarkerCompile";
	private static final String LISP_ERROR_MARKER = "jasko.tim.lisp.lispMarkerCompileError";
	private static final String LISP_WARNING_MARKER = "jasko.tim.lisp.lispMarkerCompileWarning";
	private static final String LISP_PACKAGE_NOT_LOADED_MARKER = "jasko.tim.lisp.lispMarkerCompilePackage";

	public static void deleteMarkers(IFile file){
		if(file != null){
			try{
				file.deleteMarkers(LISP_MARKER, true, IResource.DEPTH_ZERO);
			} catch (CoreException e1) {
 				e1.printStackTrace();
 			}				
		}
	}

	public static void deleteMarkers(IFile file, int offset, int length){
		if( file == null || offset < 0 || length <= 0 ){
			return;
		}
		try{
			IMarker[] markers = 
				file.findMarkers(LISP_MARKER, true, IResource.DEPTH_ZERO);
			for( IMarker m: markers ){
				int moffset = (Integer)m.getAttribute(IMarker.CHAR_START); 
				if( moffset >= offset && moffset <= offset + length ){
					m.delete();
				}
			}
			markers = file.findMarkers(LISP_MARKER, true, IResource.DEPTH_ZERO);
		} catch (CoreException e1) {
			e1.printStackTrace();
		}
	}

	public static void deleteCompileMarkers(IFile file){
		if(file != null){
			try{
				file.deleteMarkers(LISP_COMPILE_MARKER, true, IResource.DEPTH_ZERO);
				} catch (CoreException e1) {
 					e1.printStackTrace();
 				}				
		}
	}

	public static void deletePackageErrorMarkers(IProject project){
		if( project == null ){
			return;
		}
		try{
			for(IMarker m: project.findMarkers(LISP_PACKAGE_NOT_LOADED_MARKER, 
					false, IResource.DEPTH_INFINITE)){
				m.delete();
			}
			return;
		} catch (CoreException e) {
			return;
		}
	}
	
	
	private static void addMarker(IFile file, int offset, int length, String msg,
			int severity, String markerType){

		Map<String, Object> attr = new HashMap<String, Object>();
		attr.put(IMarker.CHAR_START, new Integer(offset));
		attr.put(IMarker.CHAR_END, new Integer(offset+length));
		
		attr.put(IMarker.MESSAGE, msg);
		attr.put(IMarker.SEVERITY, severity);
		
		try {
			MarkerUtilities.createMarker(file, attr, markerType);
		} catch (CoreException e) {
				System.out.println(e);
		}							
	}
	
	private static void addMarker(IFile file, int offset, int length, int lineNum,
			String msg, int severity, String markerType){

		Map<String, Object> attr = new HashMap<String, Object>();
		attr.put(IMarker.CHAR_START, new Integer(offset));
		attr.put(IMarker.CHAR_END, new Integer(offset+length));
		
		attr.put(IMarker.MESSAGE, msg);
		attr.put(IMarker.SEVERITY, severity);
		attr.put(IMarker.LINE_NUMBER, lineNum);
		
		try {
			MarkerUtilities.createMarker(file, attr, markerType);
		} catch (CoreException e) {
				System.out.println(e);
		}							
	}
	
	public static void addCompileErrorMarker(IFile file, int offset, int length, String msg){
		addMarker(file,offset,length,msg,IMarker.SEVERITY_ERROR,LISP_ERROR_MARKER);
	}
	
	public static void addCompileWarningMarker(IFile file, int offset, int length, String msg){
		addMarker(file,offset,length,msg,IMarker.SEVERITY_WARNING,LISP_WARNING_MARKER);
	}
	
	public static void addCompileMarker(IFile file, int offset, int length, String msg, boolean isError){
		if( isError ){
			addCompileErrorMarker(file,offset,length,msg);
		} else {
			addCompileWarningMarker(file,offset,length,msg);
		}
	}
	
	public static void addPackageMarker(IFile file, int offset, int endOffset, 
			int lineNum, String pkg){
		addMarker(file,offset,endOffset - offset + 1, lineNum,
				"Package "+pkg+
				" not loaded. \nRight click on corresponding .asd file and select\nLoad Package",
				IMarker.SEVERITY_ERROR,LISP_PACKAGE_NOT_LOADED_MARKER);
	}

	// multiple function definition marker
	public static void addMultMarker(IFile file, int offsetStart, int offsetEnd, boolean last){
		String msg = "";
		if( last ){
			msg = "The definition redefines the function defined earlier in the file";
		} else {
			msg = "The function is redefined by other definition(s) later in the file";
		}
		addMarker(file,offsetStart,offsetEnd-offsetStart+1,msg,IMarker.SEVERITY_WARNING,LISP_WARNING_MARKER);
	}

	public static void addCommaMarker(IFile file, int offset, int lineNum){
		addMarker(file,offset,1,lineNum,"Comma is not inside a backquote",
				IMarker.SEVERITY_ERROR,LISP_ERROR_MARKER);
	}
	
	public static void addParenMarker(IFile file, int offset, 
			int lineNum, boolean unmatchedOpen){
		String msg = "";
		if(unmatchedOpen){
			msg = "Unmatched open parenthesis.";
		} else {
			msg = "Unmatched closing parenthesis.";
		}
		addMarker(file,offset,1,lineNum,msg,
				IMarker.SEVERITY_ERROR,LISP_ERROR_MARKER);
	}
	
	
}
