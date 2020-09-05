;;; org-linked-tasks.el --- `org-mode' property for linking tasks together -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mikhail Skorzhisnkii

;; Author: Mikhail Skorzhisnkii <mskorzhinskiy@eml.cc>
;; Created: August 15, 2020
;; Version: 0.1.0
;; Keywords:
;; Homepage: https://github.com/mskorzhinskiy/org-linked-tasks
;; Package-Requires: ((emacs "26.1") (org "9.0") (org-ql "0.5-pre"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `org-linked-tasks' is implements special `org-mode' property to link
;; headlines together based on their IDs.

;;; License

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

;;; Code:

;;;; Requirements

(require 'org-id)
(require 'org-ql)

;;;; Customization

(defgroup org-linked-tasks nil
  "Customisation for `org-linked-tasks'"
  :group 'org
  :link '(url-link "https://github.com/mskorzhinskiy/org-linked-tasks"))

(defcustom org-linked-tasks-files #'org-agenda-files-with-current-file
  "Files to use when doing searched for linked tasks.

Function: accept no arguments, should return list of `org-mode' files."
  :group 'org-linked-tasks
  :type 'function)

(defcustom org-linked-tasks-property "LINKED"
  "Property name to be used for linked tasks."
  :group 'org-linked-tasks
  :type 'string)

(defcustom org-linked-tasks-action #'org-linked-tasks-schedule-and-next
  "What to do when linked all linked tasks are finally done.

Function: accept to arguments, called with pointer and buffer set
on parrent task."
  :group 'org-linked-tasks
  :type 'function)

;;;; Functions

(defun org-linked-tasks-schedule-and-next ()
  "Schedule and switch headline to NEXT."
  (org-schedule nil (current-time))
  (org-todo "NEXT"))

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

CHANGE-PLIST is a property list with TODO state changes.

Schedule for today and mark parent task as NEXT if this is the
last item."
  (when (member (plist-get change-plist :to) org-done-keywords)
    (let ((parent-id (org-entry-get (point) org-linked-tasks-property)))
      (when parent-id
        (org-delete-property org-linked-tasks-property)
        (let ((entries (org-ql-select (funcall org-linked-tasks-files)
                         `(and (property ,org-linked-tasks-property ,parent-id)
                               (not (done))))))
          (if (= 0 (length entries))
              (let ((marker (org-id-find parent-id 'marker)))
                (with-current-buffer (marker-buffer marker)
                  (goto-char (marker-position marker))
                  (funcall org-linked-tasks-action)))))))))

(defun org-linked-tasks/check-blocked-state (change-plist)
  "Hook function for `org-blocker-hook'.

CHANGE-PLIST is a property list with TODO state changes.

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

;;;###autoload
(defun org-linked-tasks-load ()
  "Install all required hooks for this package to operate."
  (add-hook #'org-trigger-hook
            #'org-linked-tasks/schedule-and-mark-next-trigger)
  (add-hook #'org-blocker-hook
            #'org-linked-tasks/check-blocked-state))

;;;###autoload
(defun org-linked-tasks-unload ()
  "Uninstall all hooks."
  (remove-hook #'org-trigger-hook
               #'org-linked-tasks/schedule-and-mark-next-trigger)
  (remove-hook #'org-blocker-hook
               #'org-linked-tasks/check-blocked-state))

(provide 'org-linked-tasks)
;;; org-linked-tasks.el ends here
