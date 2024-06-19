;;; consult-gnome-search.el --- Gnome search interface using consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;;         Alexis Purslane <alexispurslane@pm.me>
;; Keywords: convenience
;; Homepage: https://github.com/juergenhoetzel/emacs-gnome-search
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.1") (consult "0.8"))

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

;; This package provides functions to interactively query
;; Gnome search providers using consult.

;;; Code:

(require 'gnome-search)
(require 'url-util)

(defface consult-gnome-search-name
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight names in `consult-gnome-search'.")

(defgroup consult-gnome-search nil
  "Options concerning consult gnome search."
  :tag "Consult Gnome Search"
  :group 'consult-gnome-search)


(defcustom consult-gnome-search-max-results 5
  "Maximum number of results to receive for a provider.

  0 means no limit."
  :type 'integer
  :group 'consult-gnome-search)


(defun consult-gnome-search-find-nautilus-id (result-id)
  "Edit filename associated with RESULT-ID which is expected to be a file URL."
  (if-let ((file-name (url-unhex-string (url-filename (url-generic-parse-url result-id)))))
      (find-file file-name)
    (warn "Failed to parse result-id %s to as file-name" result-id)))

(defcustom consult-gnome-search-activate-functions '(("org.gnome.Nautilus" . consult-gnome-search-find-nautilus-id))
  "How the command `consult-gnome-search' activates the result.

An association list of (PROVIDER . FUNCTION) pairs.  PROVIDER
identifies bus name of a `gnome-search-provider'.  The associated
FUNCTION specifies a function of a single result-id argument
activating the result instead using the default method of the gnome
search provider"
  :group 'consult-gnome-search
  :type '(repeat (cons (string :tag "D-Bus name")
		       (function :tag "Activation function"))))


(defun consult-gnome-search--receive-candidates (async provider terms results)
  "Create the final (including metadata) list of `gnome-search-result' candidates.

PROVIDER the associated `gnome-search-provider'.
TERMS is a list of search terms used to create RESULTS.
ASYNC is the async function which receives the candidates."
  (unless (zerop consult-gnome-search-max-results)
    (setq results (seq-take results consult-gnome-search-max-results)))
  (let* ((metas (gnome-search-results-metas provider results))
	 (gs-results (mapcar  (lambda (meta)
				(let ((gs-result (gnome-search--result-from-meta provider meta)))
					;keep track of search terms (dbus ActivateResult callback)
				  (setf (gnome-search-result-terms gs-result) terms)
				  gs-result))
			      metas)))
    (funcall async gs-results)))

(defun consult-gnome-search--transformer (gs-result)
  "Transform `gnome-search-result' GS-RESULT to a completion candidate."
  (propertize
   (concat (if-let ((image (gnome-search--create-image gs-result)))
	       (propertize " "		;optional icon
			   'display  image))
	   (gnome-search-result-name gs-result))
   'consult--candidate gs-result
   'face 'consult-gnome-search-name))

(defun consult-gnome-search--async-search (async)
  "Async search provider for `consult-gnome-search'.

ASYNC is the async function which receives the candidates."
  (lambda (action)
    (pcase-exhaustive action
      ((pred stringp)
       (when (not (string-empty-p (string-trim action)))
	 (funcall async #'flush)
	 (gnome-search-async (string-split action)
			     (lambda (provider result)
			       (when result
				 (consult-gnome-search--receive-candidates
				  async provider (string-split action) result))))))
      (_ (funcall async action))))) 	;FIXME: Catchall?

(defun consult-gnome-search-collection ()
  "Generate an async completion function for `consult-gnome-search'."
  (thread-first
    (consult--async-sink)
    (consult--async-refresh-immediate)
    (consult--async-map #'consult-gnome-search--transformer)
    (consult-gnome-search--async-search)
    (consult--async-throttle)
    (consult--async-split)))

(defun consult-gnome-search--narrow ()
  "Return narrow key configuration used with `consult-gnome-search'.

For the format see `consult--read', for the value types see the
name slot in `gnome-search--get-providers'."
  (let ((available-keys `(,@(number-sequence ?A ?Z) ,@(number-sequence ?a ?z)))
	narrow-keys)
    ;; the list of provider-names ist different on each system: Create a deterministic dynamic key configuration
    (dolist (provider (gnome-search--get-providers))
      ;; prefer lowercase
      (if-let ((name (gnome-search-provider-name provider))
	       (key (seq-some (lambda (c)
				(car (or (member (downcase c) available-keys) (member (upcase c) available-keys))))
			      name)))
	  (progn
	    (push (cons key name) narrow-keys)
	    (setq available-keys (delq key available-keys)))
	(push (cons (car available-keys) name) narrow-keys)				;fallback, choose random
	(setq available-keys (cdr available-keys))))
    (nreverse narrow-keys)))


(defun consult-gnome-search--group (cand transform)
  "Return title for CAND or TRANSFORM the candidate."
  (if transform cand
    (gnome-search-provider-name
     (gnome-search-result-provider (get-text-property 0 'consult--candidate cand)))))

(defun consult-gnome-search--activate-result (result)
  "Activate search result RESULT."
  (unless (gnome-search-result-p result)
    (error "Invalid argument: %s" (type-of result)))
  (let ((id (gnome-search-result-id result))
	(bus-name (gnome-search-provider-bus-name (gnome-search-result-provider result)))
	(object-path (gnome-search-provider-object-path (gnome-search-result-provider result)))
	(timestamp (time-convert nil 'integer))
	(search-terms (gnome-search-result-terms result)))
    (if-let ((f (cdr (assoc bus-name consult-gnome-search-activate-functions))))
	(funcall f id)
      (dbus-call-method :session bus-name object-path "org.gnome.Shell.SearchProvider2" "ActivateResult" id search-terms  timestamp))))

;;;###autoload
(defun consult-gnome-search (&optional initial)
  "Search gnome search providers given INITIAL input.

  The input string is not preprocessed and passed literally to the
  underlying man commands."
  (interactive)
  (consult-gnome-search--activate-result
   (consult--read
    (consult-gnome-search-collection)
    :prompt "Gnome search: "
    :sort nil
    :require-match t
    :lookup #'consult--lookup-candidate
    :group #'consult-gnome-search--group
    :narrow (let ((narrow-key-configuration (consult-gnome-search--narrow)))
	      (list :predicate
		    (lambda (cand)
		      (let ((narrow-name (cdr (assoc consult--narrow narrow-key-configuration))))
			(equal (gnome-search-provider-name
				(gnome-search-result-provider
				 (get-text-property 0 'consult--candidate cand)))
			       narrow-name)))
		    :keys narrow-key-configuration))
    :category 'consult-gnome-search)))

(provide 'consult-gnome-search)
;;; consult-gnome-search.el ends here

