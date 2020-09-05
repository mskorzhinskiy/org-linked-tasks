;;; org-linked-tasks.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Mikhail Skorzhisnkii
;;
;; Author: Mikhail Skorzhisnkii <http://github/mskorzhinskiy>
;; Maintainer: Mikhail Skorzhisnkii <mskorzhinskiy@eml.cc>
;; Created: August 15, 2020
;; Modified: August 15, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/mskorzhinskiy/org-linked-tasks
;; Package-Requires: ((emacs 27.0.91) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'org-id)
(require 'org-ql)

(defcustom org-linked-tasks-files #'org-agenda-files-with-current-file
  "Files to use when doing searched for linked tasks.

Function: accept no arguments, should return list of `org-mode' files."
  :group 'org-agenda
  :type 'function)

(defcustom org-linked-tasks-property "LINKED"
  "Property name to be used for linked tasks."
  :group 'org-agenda
  :type 'string)

;; TODO: Should I keep this funcion here for the test purpouses, or is it too
;; lame?
(defun org-agenda-files-with-current-file ()
  "Small helper for demonstration. Return all agenda files and current buffer."
  (let ((files (org-agenda-files t nil)))
    (when (string= major-mode 'org-mode)
      (push (buffer-file-name (current-buffer)) files))
    files))

(defun org-linked-tasks/schedule-and-mark-next-trigger (change-plist)
  "Hook function for `org-trigger-hook'.

Schedule for today and mark parent task as NEXT if this is the
last item."
  (when (member (plist-get change-plist :to) org-done-keywords)
    (let ((parent-id (org-entry-get (point) org-linked-tasks-property)))
      (when parent-id
        (org-delete-property org-linked-tasks-property)
        (let ((entries (org-ql-select (funcall org-linked-tasks-files)
                         `(and (property ,org-linked-tasks-property ,parent-id)
                               (not (done))))))
          (message (format ">> %d" (length entries)))
          (if (= 0 (length entries))
              (let ((marker (org-id-find parent-id 'marker)))
                (with-current-buffer (marker-buffer marker)
                  (goto-char (marker-position marker))
                  (org-schedule nil (current-time))
                  (org-todo "NEXT")))))))))

(defun org-linked-tasks/check-blocked-state (change-plist)
  "Hook function for `org-blocker-hook'.

Check blocked state of the task based on its ID."
  (if (member (plist-get change-plist :to) org-done-keywords)
    (let* ((id (org-id-get))
           (headings (when id
                       (org-ql-select (funcall org-linked-tasks-files)
                         `(property ,org-linked-tasks-property ,id)
                         :action (lambda () (org-get-heading))))))
      (if headings
          (let* ((first-heading (pop headings))
                 (others-count (length headings)))
            (setq org-block-entry-blocking
                  (if (> others-count 0)
                      (format "%s and %s others" first-heading others-count)
                    first-heading))
            ;; Blocked: QL search returned a list
            nil)
        ;; Not blocked: QL search returned nothing
        t))
    ;; Not blocked: Changing to non-done is always possible
    t))

(defun org-linked-tasks/show-linked-tasks ()
  "Helper function to show all tasks that are blocking this task."
  (interactive)
  (let ((id (org-id-get)))
    (if id
        (org-ql-search (funcall org-linked-tasks-files)
          `(property ,org-linked-tasks-property ,id)
          :title (concat "Linked for: " (org-get-heading t)))
      (message "No ID stored in this task!"))))

;; TODO: should I add autoload?
(defun org-linked-tasks-load ()
  "Install all required hooks for this package to operate."
  (add-hook #'org-trigger-hook
            #'org-linked-tasks/schedule-and-mark-next-trigger)
  (add-hook #'org-blocker-hook
            #'org-linked-tasks/check-blocked-state))

;; TODO: should I add autoload?
(defun org-linked-tasks-unload ()
  "Uninstall all hooks."
  (remove-hook #'org-trigger-hook
               #'org-linked-tasks/schedule-and-mark-next-trigger)
  (remove-hook #'org-blocker-hook
               #'org-linked-tasks/check-blocked-state))

(provide 'org-linked-tasks)
;;; org-linked-tasks.el ends here
