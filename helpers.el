(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s
  not. Return a list of installed packages or nil for every skipped
  package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))
