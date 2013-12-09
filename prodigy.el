;;; prodigy.el --- Process manager

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; URL: http://github.com/rejeep/prodigy.el
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (cl-lib "0.3"))

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
(require 'cl-lib)

(defgroup wrap-region nil
  "..."
  :prefix "prodigy-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/rejeep/prodigy.el"))

(defcustom prodigy-buffer-name "*prodigy*"
  "..."
  :group 'prodigy)

(defvar prodigy-mode-hook nil
  "Mode hook for `prodigy-mode'.")

(defvar prodigy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'prodigy-quit)
    (define-key map (kbd "n") 'prodigy-next)
    (define-key map (kbd "p") 'prodigy-prev)
    (define-key map (kbd "g") 'prodigy-refresh)
    map)
  "Keymap for `prodigy-mode'.")

(defvar prodigy-services nil
  "...")

(defun prodigy-quit ()
  "..."
  (interactive)
  (kill-buffer (buffer-name)))

(defun prodigy-next ()
  "..."
  (interactive)
  (when (< (1+ (line-number-at-pos (point)))
           (line-number-at-pos (point-max)))
    (forward-line 1)))

(defun prodigy-prev ()
  "..."
  (interactive)
  (when (> (line-number-at-pos (point)) 1)
    (forward-line -1)))

(defun prodigy-define (&rest args)
  "..."
  (push args prodigy-services))

(defun prodigy-sorted-services ()
  "..."
  (-sort
   (lambda (service-1 service-2)
     (string<
      (plist-get service-1 :name)
      (plist-get service-2 :name)))
   prodigy-services))

(defun prodigy-refresh ()
  "..."
  (interactive)
  (let (buffer-read-only)
    (let ((line (line-number-at-pos (point))))
      (erase-buffer)
      (dolist (service (prodigy-sorted-services))
        (insert (plist-get service :name) "\n"))
      (goto-char (point-min))
      (forward-line (1- line)))))

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
