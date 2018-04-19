;;; counsel-org-clock.el --- Counsel commands for org-clock -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (ivy "0.10.0"))
;; URL: https://github.com/akirak/counsel-org-clock

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides the following two commands via Ivy interface:
;;
;; - `counsel-org-clock-context' displays the currently clocked task as well as
;;   its ancestors and descendants. If there is no currently running clock,
;;   this function behaves the same as `counsel-org-clock-history'.
;; - `counsel-org-clock-history' displays entries in `org-clock-history'
;;   variable.

;;; Code:
(require 'cl-lib)
(require 'ivy)
(require 'org-clock)

;;;; Counsel commands

;;;###autoload
(defun counsel-org-clock-context ()
  "Display the current org-clock, its ancestors, and its descendants via Ivy.

If there is no clocking task, display the clock history using
`counsel-org-clock-history'."
  (interactive)
  (if (org-clocking-p)
      (counsel-org-clock--ivy-context org-clock-marker
                                      (format "headings around current org-clock [%s]: "
                                              (file-name-nondirectory
                                               (buffer-file-name (marker-buffer org-clock-marker)))))
    (counsel-org-clock-history)))

(defun counsel-org-clock--ivy-context (marker prompt)
  "Display the context of an org heading pointed by MARKER with PROMPT via Ivy.

The ancestors of the heading, the heading, and its descendants are shown in this
order."
  (cl-destructuring-bind
      (selection candidates)
      (counsel-org-clock--get-marker-context marker)
    (ivy-read prompt
              candidates
              :caller 'counsel-org-clock-context
              :require-match t
              :preselect selection
              :action #'counsel-org-clock--run-context-action)))

(defun counsel-org-clock--get-marker-context (marker)
  "Get the context of MARKER.

The returned value is a list which consists of the following elements:

1. A string to be used as :preselect in ivy.
2. An alist of candidates to be used in ivy.

The result is used in `counsel-org-clock-context' when there is a clocking task."
  (with-current-buffer (org-base-buffer (marker-buffer marker))
    (org-with-wide-buffer
     (goto-char marker)
     (let ((current (counsel-org-clock--candidate-at-point))
           (ancestors (nreverse
                       (save-excursion
                         (cl-loop for cont = (org-up-heading-safe)
                                  while cont
                                  collect (counsel-org-clock--candidate-at-point)))))
           (descendants (cdr (org-map-entries
                              'counsel-org-clock--candidate-at-point
                              nil 'tree))))
       (list (car current)
             (append ancestors (list current) descendants))))))

;;;###autoload
(defun counsel-org-clock-history ()
  "Display the history of org-clock via Ivy."
  (interactive)
  (ivy-read "org-clock history: "
            (cl-remove-duplicates
             (cl-loop for marker in org-clock-history
                      when (markerp marker)
                      when (buffer-live-p (marker-buffer marker))
                      collect (ignore-errors
                                (with-current-buffer (org-base-buffer (marker-buffer marker))
                                  (org-with-wide-buffer
                                   (goto-char marker)
                                   (counsel-org-clock--candidate-path-at-point)))))
             :key #'cdr)
            :caller 'counsel-org-clock-history
            :require-match t
            :action #'counsel-org-clock--run-history-action))

;;;; Functions to format candidates

(defun counsel-org-clock--candidate-at-point ()
  "Build a candidate for Ivy."
  (cons (concat (make-string (nth 0 (org-heading-components)) ?\*)
                " "
                (org-get-heading))
        (point-marker)))

(defun counsel-org-clock--candidate-path-at-point ()
  "Build a candidate for Ivy containing the path."
  (cons (format "%s: %s%s"
                (file-name-nondirectory (buffer-file-name))
                (apply 'concat
                       (nreverse (save-excursion
                                   (cl-loop for cont = (org-up-heading-safe)
                                            while cont
                                            collect (concat (counsel-org-clock--get-heading-clean)
                                                            " > ")))))
                (org-get-heading))
        (save-excursion
          ;; Move the position to the beginning of the entry for checking duplicates
          (goto-char (org-entry-beginning-position))
          (point-marker))))

(defun counsel-org-clock--get-heading-clean ()
  "`org-get-heading' with options applied."
  (org-get-heading t t)
  ;; TODO: org-get-heading accepts only 0-2 arguments?
  ;; (if (version< (org-version) "9.1")
  ;;     (org-get-heading t t)
  ;;   ;; Third and fourth arguments have been supported since 9.1:
  ;;   ;; https://code.orgmode.org/bzg/org-mode/commit/53ee147f4537a051bfde366ea1459f059fdc13ef
  ;;   ;; https://github.com/emacs-china/org-mode/blob/6dc6eb3b029ec00c10e20dcfaa0b6e328bf36e03/etc/ORG-NEWS
  ;;   (org-get-heading t t t t))
  )

;;;; Actions
;;;;; Macros to help you define actions

(defmacro counsel-org-clock--candidate-display-action (&rest form)
  "Create an anonymous function to display a given candidate and run FORM."
  `(lambda (cand)
     (let ((marker (cdr cand)))
       (when (buffer-live-p (marker-buffer marker))
         (pop-to-buffer (marker-buffer marker))
         (when (or (> marker (point-max)) (< marker (point-min)))
           (widen))
         (goto-char marker)
         (org-show-context)
         ,@form))))

(defmacro counsel-org-clock--candidate-widen-action (&rest form)
  "Create an anonymous function to run FORM silently with a given candidate.

This is deprecated in 0.2. Use `counsel-org-clock--with-marker'."
  `(lambda (cand)
     (let ((marker (cdr cand)))
       (when (buffer-live-p (marker-buffer marker))
         (with-current-buffer (marker-buffer marker)
           (org-with-wide-buffer
            (goto-char marker)
            ,@form))))))

(defmacro counsel-org-clock--with-marker (marker &rest form)
  "Temporarily go to MARKER and run FORM."
  (declare (indent 1))
  `(if (buffer-live-p (marker-buffer ,marker))
       (with-current-buffer (org-base-buffer (marker-buffer ,marker))
         (org-with-wide-buffer
          (goto-char ,marker)
          ,@form))
     (error "Cannot find location")))

(defmacro counsel-org-clock--candidate-interactive-action (&rest form)
  "Create an anonymous function to run FORM in a temporary window."
  `(lambda (cand)
     (let ((marker (cdr cand)))
       (save-window-excursion
         (pop-to-buffer-same-window (marker-buffer marker))
         (delete-other-windows)
         (save-excursion
           (save-restriction
             (widen)
             (goto-char marker)
             (org-narrow-to-subtree)
             ,@form))))))

;;;;; Action functions
(defun counsel-org-clock-goto-action (cand)
  "Jump to the heading in counsel-org-clock.

CAND is a cons cell whose cdr is a marker to the entry.

This function is deprecated in 0.2."
  (org-goto-marker-or-bmk (cdr cand)))

(defun counsel-org-clock--clock-in-marker (marker)
  "Start a clock on the heading pointed by MARKER."
  (counsel-org-clock--with-marker marker
    (org-clock-in)))

(defun counsel-org-clock-clock-in-action (cand)
  "Clock in to the heading in counsel-org-clock.

CAND is a cons cell whose cdr is a marker to the entry.

This function is deprecated in 0.2."
  (let ((marker (cdr cand)))
    (if (buffer-live-p (marker-buffer marker))
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (org-clock-in)))
      (error "Cannot find location"))))

(defun counsel-org-clock--clock-dwim-marker (marker)
  "Toggle the clocking status on the heading pointed by MARKER."
  (counsel-org-clock--with-marker marker
    (if (and (org-clocking-p)
             (eq (org-entry-beginning-position)
                 (with-current-buffer (marker-buffer org-clock-marker)
                   (save-excursion
                     (goto-char org-clock-marker)
                     (org-entry-beginning-position)))))
        (org-clock-out)
      (org-clock-in))))

(defun counsel-org-clock-clock-dwim-action (cand)
  "Toggle the clocking status on the heading in counsel-org-clock.

If the selected entry is currently clocked, clock out from it.
Otherwise, clock in it.

CAND is a cons cell whose cdr is a marker to the entry.

This function is deprecated in 0.2."
  (let ((marker (cdr cand)))
    (if (buffer-live-p (marker-buffer marker))
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (if (and (org-clocking-p)
                    (eq (org-entry-beginning-position)
                        (with-current-buffer (marker-buffer org-clock-marker)
                          (save-excursion
                            (goto-char org-clock-marker)
                            (org-entry-beginning-position)))))
               (org-clock-out)
             (org-clock-in))))
      (error "Cannot find location"))))

;;;;; Dispatching an action
(defun counsel-org-clock--dispatch-action (action cand)
  "Dispatch ACTION on CAND.

ACTION is a symbol indicating an action or a function on a marker.

CAND is a cons cell whose cdr is a marker to an entry in an org buffer."
  (let ((marker (if (markerp (cdr cand))
                    (cdr cand)
                  (error "Invalid candidate: %s" (prin1-to-string cand)))))
    (pcase action
      ('nil (org-goto-marker-or-bmk marker))
      ('goto (org-goto-marker-or-bmk marker))
      ('clock-in (counsel-org-clock--clock-in-marker marker))
      ('clock-dwim (counsel-org-clock--clock-dwim-marker marker))
      ;; Handle deprecated options
      ('counsel-org-clock-goto-action (org-goto-marker-or-bmk marker))
      ('counsel-org-clock-clock-in-action (counsel-org-clock--clock-in-marker marker))
      ('counsel-org-clock-clock-dwim-action (counsel-org-clock--clock-dwim-marker marker))
      ;; User-defined action
      ((pred functionp) (funcall action marker))
      (_ (error "Unsupported type of action: %s" (prin1-to-string action))))))

;;;;; Default action

(defcustom counsel-org-clock-default-action
  'goto
  "Default action for commands in counsel-org-clock."
  :type '(choice (const :tag "Go to" goto)
                 (const :tag "Clock in" clock-in)
                 (const :tag "Toggle clock in/out" clock-dwim)
                 function)
  :group 'counsel-org-clock)

(defun counsel-org-clock--run-context-action (cand)
  "The default action in `counsel-org-clock-context'.

CAND is a cons cell whose cdr is a marker to the heading.

See `counsel-org-clock-default-action'."
  ;; TODO: Allow the user to override the default action for the command
  (counsel-org-clock--dispatch-action counsel-org-clock-default-action
                                      cand))

(defun counsel-org-clock--run-history-action (cand)
  "The default action in `counsel-org-clock-history'.

CAND is a cons cell whose cdr is a marker to the heading.

See `counsel-org-clock-default-action'."
  ;; TODO: Allow the user to override the default action for the command
  (counsel-org-clock--dispatch-action counsel-org-clock-default-action
                                      cand))

;;;;; Alternative actions

(defcustom counsel-org-clock-actions
  `(("g" (lambda (x) (org-goto-marker-or-bmk (cdr x))) "goto")
    ("n" ,(counsel-org-clock--candidate-display-action
           (org-narrow-to-subtree)) "narrow")
    ("s" ,(counsel-org-clock--candidate-display-action
           (org-tree-to-indirect-buffer)) "show in indirect buffer")
    ("t" ,(counsel-org-clock--candidate-interactive-action
           (org-todo)) "change todo state")
    ("q" ,(counsel-org-clock--candidate-interactive-action
           (org-set-tags-command)) "set tags")
    ("p" ,(counsel-org-clock--candidate-interactive-action
           (call-interactively 'org-set-property)) "set property")
    ("I" ,(counsel-org-clock--candidate-widen-action
           (org-clock-in)) "clock in")
    ("O" ,(counsel-org-clock--candidate-widen-action
           (when (org-clocking-p)
             (let ((clock (with-current-buffer (marker-buffer org-clock-marker)
                            (save-excursion
                              (goto-char org-clock-marker)
                              (org-entry-beginning-position)))))
               (when (eq clock (org-entry-beginning-position))
                 (org-clock-out))))) "clock out (if current)")
    ("l" ,(counsel-org-clock--candidate-widen-action
           (call-interactively 'org-store-link)) "store link"))
  "List of actions available in commands in counsel-org-clock.

These actions will be available in `counsel-org-clock-context' and
`counsel-org-clock-history' commands. To customize the actions in those
commands, you have to set this variable before the package is loaded."
  :group 'counsel-org-clock)

(ivy-set-actions 'counsel-org-clock-context counsel-org-clock-actions)
(ivy-set-actions 'counsel-org-clock-history counsel-org-clock-actions)

(provide 'counsel-org-clock)
;;; counsel-org-clock.el ends here
