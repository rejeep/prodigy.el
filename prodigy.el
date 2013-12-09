;;; prodigy.el --- Process manager

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; URL: http://github.com/rejeep/prodigy.el

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

(defgroup wrap-region nil
  "Process manager."
  :prefix "prodigy-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/rejeep/prodigy.el"))

(defcustom prodigy-buffer-name "*prodigy*"
  "Name of Prodigy buffer."
  :group 'prodigy)

(defvar prodigy-mode-hook nil
  "Mode hook for `prodigy-mode'.")

;;;###autoload
(defun prodigy ()
  "Start Prodigy mode."
  (interactive)
  (switch-to-buffer (get-buffer-create prodigy-buffer-name))
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq mode-name "Prodigy")
  (setq major-mode 'prodigy-mode)
  (run-mode-hooks 'prodigy-mode-hook))

(provide 'prodigy)

;;; prodigy.el ends here
