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

(add-hook #'org-trigger-hook
          #'org-linked-tasks/schedule-and-mark-next-trigger)

(add-hook #'org-blocker-hook
          #'org-linked-tasks/check-blocked-state)

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
    (let ((parent-id (org-entry-get (point) "LINKED")))
      (when parent-id
        (org-delete-property "LINKED")
        (let ((entries (org-ql-select (org-agenda-files-with-current-file)
                         `(and (property "LINKED" ,parent-id)
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
                       (org-ql-select (org-agenda-files-with-current-file)
                         `(property "LINKED" ,id)
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
        (org-ql-search (org-agenda-files-with-current-file)
          `(property "LINKED" ,id)
          :title (concat "Linked for: " (org-get-heading t)))
      (message "No ID stored in this task!"))))

(provide 'org-linked-tasks)
;;; org-linked-tasks.el ends here
