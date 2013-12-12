;;; prodigy.el --- Process manager

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
  "..."
  :prefix "prodigy-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/rejeep/prodigy.el"))

(defface prodigy-line-face
  '((((class color)) :background "#4A708B"))
  "..."
  :group 'prodigy)

(defconst prodigy-buffer-name "*prodigy*"
  "...")

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
  "...")

(defmacro prodigy-with-modify-line (&rest body)
  "..."
  `(let ((inhibit-read-only t))
     (save-excursion
       (goto-char (line-beginning-position))
       (let ((service-name (get-text-property (point) 'service-name)))
         ,@body
         (put-text-property (line-beginning-position)
                            (line-end-position)
                            'service-name service-name)))))

(defun prodigy-sorted-services ()
  "..."
  (--sort (string< it other) (ht-keys prodigy-services)))

(defun prodigy-set-marker (marker)
  "..."
  (when (prodigy-service-at-line-p)
    (prodigy-with-modify-line
     (delete-region (line-beginning-position) (1+ (line-beginning-position)))
     (insert marker))
    (ignore-errors
      (prodigy-goto-next-line))))

(defun prodigy-highlight-line ()
  "..."
  (prodigy-color-line 'prodigy-line-face))

(defun prodigy-lowlight-line ()
  "..."
  (prodigy-color-line))

(defun prodigy-service-at-line-p (&optional line)
  "..."
  (unless line
    (setq line (line-number-at-pos)))
  (let ((point
         (save-excursion
           (goto-char (point-min))
           (forward-line (1- line))
           (line-beginning-position))))
    (not (null (get-text-property point 'service-name)))))

(defun prodigy-goto-next-line ()
  "..."
  (prodigy-goto-line (1+ (line-number-at-pos))))

(defun prodigy-goto-prev-line ()
  "..."
  (prodigy-goto-line (1- (line-number-at-pos))))

(defun prodigy-goto-line (line)
  "..."
  (cond ((prodigy-service-at-line-p line)
         (let ((inhibit-read-only t))
           (prodigy-lowlight-line)
           (goto-char (point-min))
           (forward-line (1- line))
           (prodigy-highlight-line)))
        (t
         (error "No service at line %s" line))))

(defun prodigy-quit ()
  "..."
  (interactive)
  (kill-buffer (buffer-name)))

(defun prodigy-color-line (&optional face)
  "..."
  (put-text-property (line-beginning-position)
                     (line-beginning-position 2)
                     'face face))

(defun prodigy-next ()
  "..."
  (interactive)
  (condition-case err
      (prodigy-goto-next-line)
    (error
     (message "Cannot move further down"))))

(defun prodigy-prev ()
  "..."
  (interactive)
  (condition-case err
      (prodigy-goto-prev-line)
    (error
     (message "Cannot move further up"))))

(defun prodigy-mark ()
  "..."
  (interactive)
  (prodigy-set-marker "*"))

(defun prodigy-unmark ()
  "..."
  (interactive)
  (prodigy-set-marker " "))

(defun prodigy-refresh ()
  "..."
  (interactive)
  (let ((inhibit-read-only t))
    (let ((line (line-number-at-pos (point))))
      (erase-buffer)
      (-each
       (prodigy-sorted-services)
       (lambda (service-name)
         (insert "  " service-name)
         (put-text-property (line-beginning-position) (line-end-position) 'service-name service-name)
         (insert "\n")))
      (unless (zerop (length (ht-keys prodigy-services))) ; TODO: Use ht-empty-p once merged
        (prodigy-goto-line line)))))

(defun prodigy-define-service (&rest args)
  "..."
  (ht-set prodigy-services (plist-get args :name) (ht-from-plist args)))

;;;###autoload
(defun prodigy ()
  "..."
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
