;;; prodigy.el --- Manage processes from within Emacs

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; URL: http://github.com/rejeep/prodigy.el
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (ht "1.5") (f "0.14.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 's)
(require 'dash)
(require 'ht)
(require 'f)

(defgroup wrap-region nil
  "Manage processes from within Emacs."
  :prefix "prodigy-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/rejeep/prodigy.el"))

(defface prodigy-line-face
  '((((class color)) :background "#4A708B"))
  "Color of current line."
  :group 'prodigy)

(defconst prodigy-buffer-name "*prodigy*"
  "Name of Prodigy mode buffer.")

(defcustom prodigy-completion-system 'ido
  "The completion system to be used by Prodigy."
  :group 'prodigy
  :type 'symbol
  :options '(ido default))

(defvar prodigy-mode-hook nil
  "Mode hook for `prodigy-mode'.")

(defvar prodigy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'prodigy-quit)
    (define-key map (kbd "n") 'prodigy-next)
    (define-key map (kbd "p") 'prodigy-prev)
    (define-key map (kbd "m") 'prodigy-mark)
    (define-key map (kbd "M") 'prodigy-mark-all)
    (define-key map (kbd "u") 'prodigy-unmark)
    (define-key map (kbd "U") 'prodigy-unmark-all)
    (define-key map (kbd "s") 'prodigy-start)
    (define-key map (kbd "S") 'prodigy-stop)
    (define-key map (kbd "r") 'prodigy-restart)
    (define-key map (kbd "l") 'prodigy-switch-to-buffer)
    (define-key map (kbd "o") 'prodigy-browse)
    map)
  "Keymap for `prodigy-mode'.")

(defvar prodigy-services (ht-create)
  "All registered services.

Keys is the name of a service and the value is a hash table per
service.")


;;;; Internal functions

(defun prodigy-sorted-services ()
  "Return list of services sorted by name."
  (-sort
   (lambda (service-1 service-2)
     (string<
      (ht-get service-1 :name)
      (ht-get service-2 :name)))
   (ht-values prodigy-services)))

(defun prodigy-service-at-line (&optional line)
  "Return service at LINE or current line."
  (unless line
    (setq line (line-number-at-pos)))
  (let ((point
         (save-excursion
           (goto-char (point-min))
           (forward-line (1- line))
           (line-beginning-position))))
    (let ((service-name (get-text-property point 'service-name)))
      (ht-get prodigy-services service-name))))

(defun prodigy-service-at-line-p (&optional line)
  "Return true if there is a service at LINE or current line."
  (not (null (prodigy-service-at-line line))))

(defun prodigy-goto-next-line ()
  "Go to next line."
  (prodigy-goto-line (1+ (line-number-at-pos))))

(defun prodigy-goto-prev-line ()
  "Go to previous line."
  (prodigy-goto-line (1- (line-number-at-pos))))

(defun prodigy-goto-line (line)
  "Go to LINE."
  (cond ((prodigy-service-at-line-p line)
         (when (prodigy-service-at-line-p)
           (prodigy-service-set (prodigy-service-at-line) :highlighted nil))
         (goto-char (point-min))
         (forward-line (1- line))
         (prodigy-service-set (prodigy-service-at-line) :highlighted t))
        (t
         (error "No service at line %s" line))))

(defun prodigy-write-service-at-line (service)
  "Remove service at line and insert SERVICE."
  (let ((inhibit-read-only t) (service-name (ht-get service :name)))
    (delete-region (line-beginning-position) (line-end-position))
    (if (ht-get service :marked)
        (insert "* ")
      (insert "  "))
    (insert service-name)
    (-when-let (process (ht-get service :process))
      (when (process-live-p process)
        (insert " Running")))
    (-when-let (tags (ht-get service :tags))
      (insert " [" (s-join ", " (-map 'symbol-name tags)) "]"))
    (put-text-property (line-beginning-position) (line-end-position) 'service-name service-name)
    (if (ht-get service :highlighted)
        (put-text-property (line-beginning-position) (line-beginning-position 2) 'face 'prodigy-line-face)
      (put-text-property (line-beginning-position) (line-beginning-position 2) 'face nil))))

(defun prodigy-service-set (service key value)
  "Set SERVICE KEY to VALUE.

This will update the SERVICE object, but also update the line
representing SERVICE."
  (ht-set service key value)
  (save-excursion
    (goto-char (point-min))
    (while (not (eq (prodigy-service-at-line) service))
      (forward-line 1))
    (prodigy-write-service-at-line service)))

(defun prodigy-repaint ()
  "Clear buffer and repaint all services."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (-each
     (prodigy-sorted-services)
     (lambda (service)
       (prodigy-write-service-at-line service)
       (insert "\n")))
    (ignore-errors
      (prodigy-goto-line 1))))

(defun prodigy-tags ()
  "Return uniq list of tags."
  (-uniq
   (-flatten
    (-map
     (lambda (service)
       (ht-get service :tags))
     (ht-values prodigy-services)))))

(defun prodigy-service-tagged-with? (service tag)
  "Return true if SERVICE is tagged with TAG."
  (-contains? (ht-get service :tags) tag))

(defun prodigy-services-tagged-with (tag)
  "Return list of services tagged with TAG."
  (let (services)
    (ht-each
     (lambda (name service)
       (if (prodigy-service-tagged-with? service tag)
           (push service services)))
     prodigy-services)
    services))

(defun prodigy-marked-services ()
  "Return list of services that are marked."
  (let (services)
    (ht-each
     (lambda (name service)
       (if (ht-get service :marked)
           (push service services)))
     prodigy-services)
    services))

(defun prodigy-completing-read (prompt collection)
  "Read a string in the minibuffer, with completion.

PROMPT is a string to prompt with.
COLLECTION is the list of strings that the user will be asked to
select between.

The completion system used is determined by
`prodigy-completion-system'."
  (let ((args `(,prompt ,collection nil 'require-match)))
    (cond ((eq prodigy-completion-system 'ido)
           (apply 'ido-completing-read args))
          ((eq prodigy-completion-system 'default)
           (apply 'completing-read args)))))

(defun prodigy-read-tag ()
  "Read tag from list of all possible tags."
  (let ((tag-names (-map 'symbol-name (prodigy-tags))))
    (intern (prodigy-completing-read "tag: " tag-names))))

(defun prodigy-buffer-name (service)
  "Return name of process buffer for SERVICE."
  (concat "*prodigy-" (s-dashed-words (s-downcase (ht-get service :name))) "*"))

(defun prodigy-start-service (service)
  "Start SERVICE."
  (let* ((name (ht-get service :name))
         (cwd (ht-get service :cwd))
         (command (ht-get service :command))
         (args (ht-get service :args))
         (buffer (prodigy-buffer-name service))
         (default-directory (f-full cwd))
         (process (apply 'start-process (append (list name buffer command) args))))
    (prodigy-service-set service :process process)))

(defun prodigy-stop-service (service)
  "Stop SERVICE."
  (-when-let (process (ht-get service :process))
    (when (process-live-p process)
      (kill-process process))
    (prodigy-service-set service :process nil)))

(defun prodigy-apply (fn)
  "Apply FN to service at line or marked services."
  (let ((services (prodigy-marked-services)))
    (if services
        (-each services fn)
      (-when-let (service (prodigy-service-at-line))
        (funcall fn service)))))

(defun prodigy-service-port (service)
  "Find something that look like a port in SERVICE arguments."
  (or
   (ht-get service :port)
   (-when-let (port (-first
                     (lambda (arg)
                       (s-matches? "^\\([0-9]\\)\\{4,5\\}$" arg))
                     (ht-get service :args)))
     (string-to-number port))))


;;;; User functions

(defun prodigy-quit ()
  "Quit prodigy."
  (interactive)
  (kill-buffer (buffer-name)))

(defun prodigy-next ()
  "Go to next service."
  (interactive)
  (condition-case err
      (prodigy-goto-next-line)
    (error
     (message "Cannot move further down"))))

(defun prodigy-prev ()
  "Go to previous service."
  (interactive)
  (condition-case err
      (prodigy-goto-prev-line)
    (error
     (message "Cannot move further up"))))

(defun prodigy-mark (mark-tag)
  "Mark service at point.

With prefix argument, mark all services with tag."
  (interactive "P")
  (if mark-tag
      (let ((tag (prodigy-read-tag)))
        (-each
         (prodigy-services-tagged-with tag)
         (lambda (service)
           (prodigy-service-set service :marked t))))
    (-when-let (service (prodigy-service-at-line))
      (prodigy-service-set service :marked t)
      (ignore-errors
        (prodigy-goto-next-line)))))

(defun prodigy-mark-all ()
  "Mark all services."
  (interactive)
  (ht-each
   (lambda (name service)
     (prodigy-service-set service :marked t))
   prodigy-services))

(defun prodigy-unmark (mark-tag)
  "Unmark service at point.

With prefix argument, unmark all services with tag."
  (interactive "P")
  (if mark-tag
      (let ((tag (prodigy-read-tag)))
        (-each
         (prodigy-services-tagged-with tag)
         (lambda (service)
           (prodigy-service-set service :marked nil))))
    (-when-let (service (prodigy-service-at-line))
      (prodigy-service-set service :marked nil)
      (ignore-errors
        (prodigy-goto-next-line)))))

(defun prodigy-unmark-all ()
  "Unmark all services."
  (interactive)
  (ht-each
   (lambda (name service)
     (prodigy-service-set service :marked nil))
   prodigy-services))

(defun prodigy-start ()
  "Start service at line or marked services."
  (interactive)
  (prodigy-apply 'prodigy-start-service))

(defun prodigy-stop ()
  "Stop service at line or marked services."
  (interactive)
  (prodigy-apply 'prodigy-stop-service))

(defun prodigy-restart ()
  "Restart service at line or marked services."
  (interactive)
  (prodigy-apply 'prodigy-stop-service)
  (prodigy-apply 'prodigy-start-service))

(defun prodigy-switch-to-buffer ()
  "Switch to buffer associated with service process."
  (interactive)
  (-when-let (service (prodigy-service-at-line))
    (when (ht-get service :process)
      (switch-to-buffer (prodigy-buffer-name service)))))

(defun prodigy-browse ()
  "Browse service url at point if possible to figure out."
  (interactive)
  (-when-let (service (prodigy-service-at-line))
    (-if-let (port (prodigy-service-port service))
        (browse-url (format "http://localhost:%d" port))
      (message "Could not determine port"))))

(defun prodigy-define-service (&rest args)
  "Define a new service.

ARGS is a plist with support for the following keys:

name - Name of the service"
  (when (eq (type-of (car args)) 'string)
    (pop args))
  (ht-set prodigy-services (plist-get args :name) (ht-from-plist args)))

;;;###autoload
(put 'prodigy-define-service 'lisp-indent-function 'defun)

;;;###autoload
(put 'prodigy-define-service 'doc-string-elt 1)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("(\\(\\<prodigy-define-service\\)\\>"
                (1 font-lock-keyword-face nil t))))))


;;;###autoload
(defun prodigy ()
  "Manage processes from within Emacs."
  (interactive)
  (switch-to-buffer (get-buffer-create prodigy-buffer-name))
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq mode-name "Prodigy")
  (setq major-mode 'prodigy-mode)
  (use-local-map prodigy-mode-map)
  (prodigy-repaint)
  (run-mode-hooks 'prodigy-mode-hook))

(provide 'prodigy)

;;; prodigy.el ends here
