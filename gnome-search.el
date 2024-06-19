;;; gnome-search.el --- Gnome search interface                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;;         Alexis Purslane <alexispurslane@pm.me>
;; Maintainer: Jürgen Hötzel <juergen@hoetzel.info>
;; Keywords: convenience
;; Homepage: https://github.com/juergenhoetzel/emacs-gnome-search
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides generic utilities to access Gnome search
;; providers using their dbus interface.
;;

;;; Code:

(require 'xdg)
(require 'dbus)

(defvar gnome-search-providers-directory "/usr/share/gnome-shell/search-providers/") ;FIXME: Hardcoded!

(defgroup gnome-search nil
  "Options concerning GNOME search."
  :tag "Gnome Search"
  :group 'gnome-search)

(defcustom gnome-search-providers-directory
  "/usr/share/gnome-shell/search-providers/"
  "The directory to look for GNOME Shell Search Providers."
  :type 'string
  :group 'gnome-search) ;FIXME: Hardcoded!

(defcustom gnome-search-ignored-names
  nil
  "List of ignored dbus search-provider-names."
  :type '(repeat (string :tag "Bus name: "))
  :group 'gnome-search)

(defun gnome-search-locate-desktop-file-name (desktop-name)
  "Return absolute file-name for DESKTOP-NAME.

DESKTOP-NAME must be a .desktop file-name as defined in the XDG Desktop Entry specification."
  (seq-some (lambda (dir)
	      (let ((absolute-name (concat (file-name-as-directory dir) "applications/" desktop-name)))
		(when (file-exists-p absolute-name)
		  absolute-name)))
	    (xdg-data-dirs)))


;; The basic structure search providers config file

(cl-defstruct (gnome-search-provider)
  "A record containing the information relevant to a GNOME
search provider: a DESKTOP-ID, a BUS-NAME, an OBJECT-PATH, and of
course a regular NAME."
  desktop-id bus-name object-path name)

(defun gnome-search-make-provider (filename)
  "Get `gnome-search-provider' structure from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((provider (make-gnome-search-provider)))
      (while (search-forward-regexp "\\(.*\\)=\\(.*\\)\n?" nil t)
	(setf (pcase (match-string 1)
		("DesktopId" (gnome-search-provider-desktop-id provider))
		("BusName" (gnome-search-provider-bus-name provider))
		("ObjectPath" (gnome-search-provider-object-path provider)))
	      (match-string 2)))
      (if-let* ((file-name (gnome-search-locate-desktop-file-name
                            (gnome-search-provider-desktop-id provider)))
		(name (gethash "Name" (xdg-desktop-read-file  file-name))))
	  (setf (gnome-search-provider-name provider) name))
      provider)))

(defvar gnome-search-providers nil
  "List of available `gnome-search-provider' instances.")

(defun gnome-search--get-providers ()
  "Fetch list of providers available.

The `gnome-search-providers-directory' is searched for files with the suffix =.ini=."
  (let ((installed-search-providers (mapcar #'gnome-search-make-provider (directory-files gnome-search-providers-directory t "\.ini$")))
	(names (seq-union (dbus-list-known-names :session) (dbus-list-activatable-names :session))))
    (cl-remove-if (lambda (provider) (or (member (gnome-search-provider-bus-name provider) gnome-search-ignored-names)
					 (not (member (gnome-search-provider-bus-name provider) names))))
		  installed-search-providers)))

(defun gnome-search-internal (provider terms);; FIXME Non-blocking: Remove
  "Search list of TERMS via search provider PROVIDER."
  (dbus-call-method
   :session (gnome-search-provider-bus-name provider) (gnome-search-provider-object-path provider)
   "org.gnome.Shell.SearchProvider2"
   "GetInitialResultSet" terms))

(defun gnome-search-results-metas (provider results)
  "Return list of meta data used to display each given result in RESULTS.

Results is a list of unique matches returned by `gnome-search-provider' PROVIDER."
  (dbus-call-method
   :session (gnome-search-provider-bus-name provider) (gnome-search-provider-object-path provider)
   "org.gnome.Shell.SearchProvider2"
   "GetResultMetas" results))

(defun gnome-search-get-provider (desktop-id)
  "Return `gnome-search-provider' matching string DESKTOP-ID."
  (cl-find-if  (lambda (provider) (equal (gnome-search-provider-desktop-id provider) desktop-id)) (gnome-search--get-providers)))

(cl-defstruct (gnome-search-result)
  "A record representing the information contained in a search
result returned by a GNOME Search Provider."
  provider id name description icon icon-data terms)

(defun gnome-search--result-from-meta (provider metadata)
  "Create a `gnome-search-result' record from METADATA provided by PROVIDER method GetResultMetas."
  (let ((result (make-gnome-search-result :provider provider)))
    (dolist (kv metadata)
      (pcase (car kv)
	("id" (setf (gnome-search-result-id result) (caadr kv)))
	("name" (setf (gnome-search-result-name result) (caadr kv)))
	("description" (setf (gnome-search-result-description result) (caadr kv)))
	("icon" (setf (gnome-search-result-icon result) (caadr kv)))
	("icon-data" (setf (gnome-search-result-icon-data result) (caadr kv)))))
    result))


(defun gnome-search-internal-async (provider terms callback)
  "Search list of TERMS via search provider PROVIDER."
  (dbus-call-method-asynchronously
   :session (gnome-search-provider-bus-name provider) (gnome-search-provider-object-path provider)
   "org.gnome.Shell.SearchProvider2"
   "GetInitialResultSet" callback terms))

(defun gnome-search-async (terms callback &optional providers)
  "Search list of TERMS using search PROVIDERS.

This calls the function CALLBACK as (apply CALLBACK provider result).
Return an association of results with desktop-id of the provider as key."
  (dolist (provider (or providers  (gnome-search--get-providers)))
    (gnome-search-internal-async provider terms
				 (apply-partially callback provider))))

(defun gnome-search (terms &optional providers)
  "Search list of TERMS via all PROVIDERS.

Return an association of results with desktop-id of the provider
as key."
  (unless (listp terms)
    (setq terms (split-string terms)))
  (thread-last
    (or providers  (gnome-search--get-providers))
    (seq-keep (lambda (provider)
		(if-let ((results (gnome-search-internal provider terms))
			 (metas (gnome-search-results-metas provider results)))
		    (mapcar (apply-partially #'gnome-search--result-from-meta provider) metas))))
    (apply #'append )))

(defun gnome-search--create-image (result &optional save-p)
  "Create an image from RESULT item received from `gnome-search'.

If optional arg SAVE-P is non-nil, save image as
gnome-search_NNNN.pbm also as `default-directory')."
  (if-let ((icon-data (gnome-search-result-icon-data result)))
      (pcase icon-data
	(`(,width  ,height ,stride ,(and (pred booleanp) has-alpha)  8 ,n-channels ,image-data)
	 (with-temp-buffer
	   (insert "P6\n")
	   (insert (format "%d %d\n255\n" width height))
	   (seq-do-indexed (lambda (b i)
			     (unless (and has-alpha (eq (% i n-channels) (1- n-channels))) ;P6 Netpbm doesn't support alpha
			       (insert (byte-to-string b))))
			   image-data)
	   (when save-p			;for debugging only
	     (if-let* ((last-str (car (last (directory-files "." nil "gnome-search_[0-9][0-9][0-9][0-9].pbm"))))
		       ((string-match "gnome-search_\\([0-9][0-9][0-9][0-9]\\).pbm" last-str))
		       (n2 (1+ (string-to-number (match-string 1 last-str)))))
		 (write-region (point-min) (point-max) (format "gnome-search_%04d.pbm" n2))
	       (write-region (point-min) (point-max) "gnome-search_0000.pbm")))
	   (create-image (string-make-unibyte (buffer-substring-no-properties (point-min) (point-max))) nil t :height ( - (frame-char-height) 2) :ascent 'center)))
	(_ (progn (warn "Unknown image-data format: %s" icon-data) nil)))
    (if-let* ((icon-serialized (gnome-search-result-icon result))
	      ((string= (car icon-serialized) "file")))
	(create-image (url-filename (url-generic-parse-url (caadr icon-serialized))) nil nil :height ( - (frame-char-height) 2) :ascent 'center))))

(provide 'gnome-search)
;;; gnome-search.el ends here

