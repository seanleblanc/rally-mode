;;; rally-mode --- Summary
;;; rally-mode.el - a mode to interact with the Rally Software web site.

;;; Commentary:

;; Copyright (c) 2015 Sean LeBlanc

;; Author: Sean LeBlanc

;; rally-mode is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; rally-mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with rally-mode.  If not, see http://www.gnu.org/licenses.



;; To use - M-x rally-current-iteration, enter Rally username and password.


(require 'url-util)

;;; Code: 

(defvar rally-user)
(defvar rally-password)

(define-derived-mode rally-mode special-mode "rally-mode"
  "Major mode for interacting with Rally website."
  :group 'rally-mode)

(define-key rally-mode-map (kbd "g") 'rally-current-iteration)



(defvar xyz-block-authorisation nil 
   "Flag whether to block url.el's usual interactive authorisation procedure")

(defadvice url-http-handle-authentication (around xyz-fix)
  (unless xyz-block-authorisation
    ad-do-it))

(ad-activate 'url-http-handle-authentication)

(defun rally-build-url (url-server-string params)
  (concat url-server-string "?"
	  (url-build-query-string params)))

(defun rally-basic-auth (url user pass)
  ;;(princ url) 
  (let ((xyz-block-authorisation t)
	(url-request-method "GET")
	(url-request-extra-headers 
	 `(("Content-Type" . "application/xml")
	   ("Authorization" . ,(concat "Basic "
				       (base64-encode-string
					(concat user ":" pass)))))))
     (with-current-buffer (url-retrieve-synchronously url t)
       (delete-region 1 url-http-end-of-headers)
       (buffer-string)
       )))

(defun rally-make-url-lst (username password)
  `(
    (query ,(format "((Owner.Name = %s ) AND (( Iteration.StartDate <= today ) AND (Iteration.EndDate >= today)) )" username ) )
    (order Rank)
    (fetch "true,WorkProduct,Tasks,Iteration,Estimate,State,ToDo,Name")
    ))

(defun rally-make-query (username password)
   (rally-build-url
	      "https://rally1.rallydev.com/slm/webservice/v2.0/task"
	      (rally-make-url-lst username password)
	       ))

(defun rally-current-iteration-info (username password)
  (rally-basic-auth
   (rally-make-query username password) 
	       username 
	       password))

;; TODO - better way?
(defun assoc-val (key lst)
  (cdr (assoc key lst)))

(defun rally-extract-info (lst)
  (list
   `(TaskName . ,(assoc-val '_refObjectName lst ))
   `(WorkItemDescription . ,(assoc-val '_refObjectName (find 'WorkProduct lst :key #'car ) ))
   `(SprintName . ,(assoc-val '_refObjectName (find 'Iteration lst :key #'car ) ))
   (assoc 'State lst)
   (assoc 'ToDo lst)
   (assoc 'Estimate lst)
   `(WorkItemName . ,(assoc-val 'FormattedID (find 'WorkProduct lst :key #'car)))
   ))


(defun rally-fetch-current-iteration-info-as-json ()
  (rally-current-iteration-info
   (setq rally-user (read-string "Rally user/email:" rally-user))
   (setq rally-password (read-passwd "Rally password:" nil (if (boundp 'rally-password) rally-password nil )))))

(if (boundp 'rally-password) rally-password nil)

(defun rally-parse-json-results (json-string)
  (json-read-from-string json-string))

(defun rally-get-task-list (parsed-json)
  (cdr (assoc 'Results (assoc 'QueryResult parsed-json))))


(defun rally-fetch-and-parse-current-iteration-info ()
  (mapcar #'rally-extract-info (rally-get-task-list (rally-parse-json-results (rally-fetch-current-iteration-info-as-json)))))

(defun rally-get-buffer ()
  (get-buffer-create "*rally-current-iteration*"))

(defvar rally-line-string "%-6s %-60s %-10s %-4s %-4s\n")

(defun rally-write-task-line (parsed-json)
  (insert (format rally-line-string
		  (assoc-val 'WorkItemName parsed-json)
		  (assoc-val 'WorkItemDescription parsed-json)
		  (assoc-val 'State parsed-json)
		  (assoc-val 'Estimate parsed-json)
		  (assoc-val 'ToDo parsed-json)
		  )))

(defun rally-write-output-to-buffer (buf parsed-json)
  (with-current-buffer buf 
    (insert (format rally-line-string
		    "Story"
		    "Description"
		    "Status"
		    "Est."
		    "ToDo"
		    ))
    (mapcar #'rally-write-task-line parsed-json)))

(defun rally-current-iteration ()
  "Pulls up current iteration information for the supplied user."
  (interactive)
  
  (let ((rally-current-iteration-buffer (rally-get-buffer)))
    (switch-to-buffer rally-current-iteration-buffer)
    (rally-mode)
    (let (buffer-read-only)
      (erase-buffer)
      (rally-write-output-to-buffer rally-current-iteration-buffer (rally-fetch-and-parse-current-iteration-info)))))

(provide 'rally-mode)
;;; rally-mode ends here
