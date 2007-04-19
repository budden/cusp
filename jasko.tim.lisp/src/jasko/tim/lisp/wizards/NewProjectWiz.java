/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package jasko.tim.lisp.wizards;

import jasko.tim.lisp.LispPlugin;
import jasko.tim.lisp.builder.LispNature;
import jasko.tim.lisp.swank.SwankInterface;

import org.eclipse.jface.viewers.*;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.operation.*;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.*;
import org.eclipse.core.runtime.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;


import java.io.*;
import java.lang.reflect.InvocationTargetException;


/**
 * The wizard to create a new Lisp project.
 * 
 * @author Tim Jasko
 */

public class NewProjectWiz extends Wizard implements INewWizard {
	private NewProjectWizPage1 page;
	private ISelection selection;
	private IWorkbench workbench;
	

	/**
	 * Constructor for NewProjectWiz.
	 */
	public NewProjectWiz() {
		super();
		setNeedsProgressMonitor(true);
	}
	
	/**
	 * Adding the page to the wizard.
	 */
	public void addPages() {
		page = new NewProjectWizPage1(selection);
		addPage(page);
	}

	/**
	 * This method is called when 'Finish' button is pressed in
	 * the wizard. We will create an operation and run it
	 * using wizard as execution context.
	 */
	public boolean performFinish() {
		final String projectName = page.getProjectName();
		final String customProjectPath = page.getCustomProjectPath();
		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) throws InvocationTargetException {
				try {
					doFinish(projectName, customProjectPath, monitor);
				} catch (CoreException e) {
					e.printStackTrace();
					throw new InvocationTargetException(e);
				} finally {
					monitor.done();
				}
			}
		};
		try {
			getContainer().run(true, false, op);
			workbench.showPerspective(
				"jasko.tim.lisp.perspective.LispPerspectiveFactory",
				workbench.getWorkbenchWindows()[0]);
		} catch (InterruptedException e) {
			return false;
		} catch (InvocationTargetException e) {
			Throwable realException = e.getTargetException();
			MessageDialog.openError(getShell(), "Error", realException.getMessage());
			return false;
		} catch (WorkbenchException e) {
			MessageDialog.openError(getShell(), "Error", e.getMessage());
			return false;
		}
		return true;
		
	}
	
	/**
	 * The worker method. It will create a new project, then
	 * create the appropriate Soar heirarchy and files.
	 */
	private void doFinish (String projectName, String customProjectPath, IProgressMonitor monitor) throws CoreException {
  		monitor.beginTask("Creating " + projectName, 10);
  		
         IWorkspace workspace = ResourcesPlugin.getWorkspace();
 		IWorkspaceRoot root = workspace.getRoot();
  		IProject newProject = root.getProject(projectName);
  		
  		//creation of the project
  		if (newProject.exists()) {
  			throwCoreException("Project \"" + projectName + "\" already exists");
  		} else {
             IProjectDescription pdesc = workspace.newProjectDescription(newProject.getName());
             pdesc.setLocation(customProjectPath != null ? 
                     new Path(new File(customProjectPath, projectName).getAbsolutePath()) : null);
             
             newProject.create(pdesc, monitor);
  			newProject.open(monitor);
  			
  			try {
 				String[] natures = pdesc.getNatureIds();
  				String[] newNatures = new String[natures.length + 1];
  				System.arraycopy(natures, 0, newNatures, 0, natures.length);
  				newNatures[natures.length] = LispNature.NATURE_ID;
                 pdesc.setNatureIds(newNatures);
  				
 				newProject.setDescription(pdesc, IResource.FORCE, monitor);
  			} catch (CoreException e) {
  				e.printStackTrace();
  			} // catch
			
		} // else
		
		monitor.worked(2);
		
		
		// Create the contents of the project's root directory
		String pkg = projectName.toLowerCase();
		
		InputStream contents = Templater.getTemplate("main.lisp", pkg);
		IFile main = newProject.getFile("main.lisp");
		if (!main.exists()) {
			main.create(contents ,true, monitor);
		} // if
		try {
			contents.close();
		} catch (IOException e) {
		}
		monitor.worked(1);
		
		// Make the asd file
		contents = Templater.getTemplate("asd.asd", pkg);
		IFile asd = newProject.getFile(projectName + ".asd");
		if (!asd.exists()) {
			asd.create(contents, true, monitor);
		}
		try {
			contents.close();
		} catch (IOException e) {
		}
		monitor.worked(1);
		
		// Make the defpackage file
		contents = Templater.getTemplate("defpackage.lisp", pkg);
		IFile defpackage = newProject.getFile("defpackage.lisp");
		if (!defpackage.exists()) {
			defpackage.create(contents, true, monitor);
		}
		try {
			contents.close();
		} catch (IOException e) {
		}
		monitor.worked(1);
		
		
		newProject.close(monitor);
		newProject.open(monitor);
		

		SwankInterface swank = LispPlugin.getDefault().getSwank();
		swank.sendCompileFile(defpackage.getLocation().toString(), null);
		
		monitor.worked(2);
		monitor.done();
		
	} // void doFinish(...)
	

	private void throwCoreException(String message) throws CoreException {
		IStatus status =
			new Status(IStatus.ERROR, "jasko.tim.lisp.wizards", IStatus.OK, message, null);
		throw new CoreException(status);
	}

	/**
	 * We will accept the selection in the workbench to see if
	 * we can initialize from it.
	 */
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.workbench = workbench;
		this.selection = selection;
	}
}