;;; prodigy.el --- Manage processes from within Emacs

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; URL: http://github.com/rejeep/prodigy.el
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (ht "1.5"))

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

(defvar prodigy-mode-hook nil
  "Mode hook for `prodigy-mode'.")

(defvar prodigy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'prodigy-quit)
    (define-key map (kbd "n") 'prodigy-next)
    (define-key map (kbd "p") 'prodigy-prev)
    (define-key map (kbd "g") 'prodigy-refresh)
    (define-key map (kbd "m") 'prodigy-mark)
    (define-key map (kbd "u") 'prodigy-unmark)
    map)
  "Keymap for `prodigy-mode'.")

(defvar prodigy-services (ht)
  "All registered services.

Keys is the name of a service and the value is a hash table per
service.")

(defun prodigy--sorted-services ()
  (--sort (string< it other) (ht-keys prodigy-services)))

(defun prodigy--service-at-line (&optional line)
  (unless line
    (setq line (line-number-at-pos)))
  (let ((point
         (save-excursion
           (goto-char (point-min))
           (forward-line (1- line))
           (line-beginning-position))))
    (let ((service-name (get-text-property point 'service-name)))
      (ht-get prodigy-services service-name))))

(defun prodigy--service-at-line-p (&optional line)
  (not (null (prodigy--service-at-line line))))

(defun prodigy--goto-next-line ()
  (prodigy--goto-line (1+ (line-number-at-pos))))

(defun prodigy--goto-prev-line ()
  (prodigy--goto-line (1- (line-number-at-pos))))

(defun prodigy--goto-line (line)
  (cond ((prodigy--service-at-line-p line)
         (when (prodigy--service-at-line-p)
           (prodigy--service-set (prodigy--service-at-line) :highlighted nil))
         (goto-char (point-min))
         (forward-line (1- line))
         (prodigy--service-set (prodigy--service-at-line) :highlighted t))
        (t
         (error "No service at line %s" line))))

(defun prodigy--service-set (service key value)
  (ht-set service key value)
  ;; TODO: Perhaps move to a repaint function?
  (save-excursion
    (prodigy--goto-service service)
    (let ((inhibit-read-only t) (service-name (ht-get service :name)))
      (delete-region (line-beginning-position) (line-end-position))
      (if (ht-get service :marked)
          (insert "* ")
        (insert "  "))
      (insert service-name)
      (put-text-property (line-beginning-position) (line-end-position) 'service-name service-name)
      (if (ht-get service :highlighted)
          (put-text-property (line-beginning-position) (line-beginning-position 2) 'face 'prodigy-line-face)
        (put-text-property (line-beginning-position) (line-beginning-position 2) 'face nil)))))

(defun prodigy--goto-service (service)
  (goto-char (point-min))
  (while (not (eq (prodigy--service-at-line) service))
    (forward-line 1)))

(defun prodigy-quit ()
  "Quit prodigy."
  (interactive)
  (kill-buffer (buffer-name)))

(defun prodigy-next ()
  "Go to next service."
  (interactive)
  (condition-case err
      (prodigy--goto-next-line)
    (error
     (message "Cannot move further down"))))

(defun prodigy-prev ()
  "Go to previous service."
  (interactive)
  (condition-case err
      (prodigy--goto-prev-line)
    (error
     (message "Cannot move further up"))))

(defun prodigy-mark ()
  "Mark service at point."
  (interactive)
  (-when-let (service (prodigy--service-at-line))
    (prodigy--service-set service :marked t)
    (ignore-errors
      (prodigy--goto-next-line))))


(defun prodigy-unmark ()
  "Unmark service at point."
  (interactive)
  (-when-let (service (prodigy--service-at-line))
    (prodigy--service-set service :marked nil)
    (ignore-errors
      (prodigy--goto-next-line))))


(defun prodigy-refresh ()
  "Refresh UI by clearing the screen and adding the services."
  (interactive)
  (let ((inhibit-read-only t))
    (let ((line (line-number-at-pos (point))))
      (erase-buffer)
      (-each
       (prodigy--sorted-services)
       (lambda (service-name)
         (insert "  " service-name)
         (put-text-property (line-beginning-position) (line-end-position) 'service-name service-name)
         (insert "\n")))
      (unless (zerop (length (ht-keys prodigy-services))) ; TODO: Use ht-empty-p once merged
        (prodigy--goto-line line)))))

(defun prodigy-define-service (&rest args)
  "Define a new service.

ARGS is a plist with support for the following keys:

name - Name of the service"
  (ht-set prodigy-services (plist-get args :name) (ht-from-plist args)))

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
  (prodigy-refresh)
  (run-mode-hooks 'prodigy-mode-hook))

(provide 'prodigy)

;;; prodigy.el ends here
