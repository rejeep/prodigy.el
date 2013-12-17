;;; prodigy.el --- Manage external services from within Emacs

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; URL: http://github.com/rejeep/prodigy.el
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (f "0.14.0"))

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
(require 'f)
(require 'ansi-color)

(defgroup prodigy nil
  "Manage external services from within Emacs."
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

(defcustom prodigy-init-async-timeout 10
  "Seconds to wait for init async callback before failing."
  :group 'prodigy
  :type 'number)

(defvar prodigy-mode-hook nil
  "Mode hook for `prodigy-mode'.")

(defvar prodigy-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'prodigy-log-quit)
    map)
  "Keymap for `prodigy-log-mode'.")

(define-derived-mode prodigy-mode special-mode "Prodigy mode"
  "Special mode for prodigy buffers"
  (put 'funny-mode 'mode-class 'special)
  (prodigy-reset)
  (prodigy-repaint)
  (set (make-local-variable 'revert-buffer-function) 'prodigy-refresh))


(defvar prodigy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'prodigy-quit)
    (define-key map (kbd "n") 'prodigy-next)
    (define-key map (kbd "p") 'prodigy-prev)
    (define-key map (kbd "m") 'prodigy-mark)
    (define-key map (kbd "t") 'prodigy-mark-tag)
    (define-key map (kbd "M") 'prodigy-mark-all)
    (define-key map (kbd "u") 'prodigy-unmark)
    (define-key map (kbd "T") 'prodigy-unmark-tag)
    (define-key map (kbd "U") 'prodigy-unmark-all)
    (define-key map (kbd "s") 'prodigy-start)
    (define-key map (kbd "S") 'prodigy-stop)
    (define-key map (kbd "r") 'prodigy-restart)
    (define-key map (kbd "$") 'prodigy-display-process)
    (define-key map (kbd "o") 'prodigy-browse)
    ;; (define-key map (kbd "g") 'prodigy-refresh)
    map)
  "Keymap for `prodigy-mode'.")

(defvar prodigy-services nil
  "List of services.

:name         - Name of service
:command      - Command to run
:args         - Arguments passed to command
:cwd          - Run command with this as `default-directory'
:port         - Specify service port for use with open function
:tags         - List of tags
:init         - Function called before process is started
:init-async   - Function called before process is started with async callback
:stop-signal  - Signal to send to processes to stop (defaults to 'int)
:path         - List of directories added to PATH when command runs")


;;;; Internal functions

(defun prodigy-sorted-services ()
  "Return list of services sorted by name."
  (-sort
   (lambda (service-1 service-2)
     (string<
      (plist-get service-1 :name)
      (plist-get service-2 :name)))
   prodigy-services))

(defun prodigy-service-at-line (&optional line)
  "Return service at LINE or current line."
  (unless line
    (setq line (line-number-at-pos)))
  (when (and (> line 0)
             (< line (line-number-at-pos (point-max))))
    (let ((point
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- line))
             (line-beginning-position))))
      (let ((service-name (get-text-property point 'service-name)))
        (prodigy-find-service service-name)))))

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

(defun prodigy-status-name (process)
  "Return PROCESS status name."
  (let ((status (process-status process)))
    (cond ((eq status 'run) "Running")
          ((eq status 'stop) "Stopped")
          ((eq status 'exit) "Exit")
          ((eq status 'signal) "Signal")
          ((eq status 'open) "Open")
          ((eq status 'closed) "Closed")
          ((eq status 'connect) "Connect")
          ((eq status 'failed) "Failed")
          ((eq status 'listen) "Listen")
          (t "Unknown"))))

(defun prodigy-write-service-at-line (service)
  "Remove service at line and insert SERVICE."
  (let ((inhibit-read-only t) (service-name (plist-get service :name)))
    (delete-region (line-beginning-position) (line-end-position))
    (if (plist-get service :marked)
        (insert "* ")
      (insert "  "))
    (insert service-name)
    (move-to-column 30 t)
    (-when-let (process (plist-get service :process))
      (insert " " (prodigy-status-name process)))
    (move-to-column 60 t)
    (-when-let (tags (plist-get service :tags))
      (insert " [" (s-join ", " (-map 'symbol-name tags)) "]"))
    (put-text-property (line-beginning-position) (line-end-position) 'service-name service-name)
    (if (plist-get service :highlighted)
        (put-text-property (line-beginning-position) (line-beginning-position 2) 'face 'prodigy-line-face)
      (put-text-property (line-beginning-position) (line-beginning-position 2) 'face nil))))

(defun prodigy-service-set (service key value)
  "Set SERVICE KEY to VALUE.

This will update the SERVICE object, but also update the line
representing SERVICE."
  (plist-put service key value)
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
       (insert "\n")))))

(defun prodigy-reset ()
  "Reset state such as marked and highlighted for all services."
  (-each
   prodigy-services
   (lambda (service)
     (plist-put service :marked nil)
     (plist-put service :highlighted nil))))

(defun prodigy-tags ()
  "Return uniq list of tags."
  (-uniq (-flatten (--map (plist-get it :tags) prodigy-services))))

(defun prodigy-service-tagged-with? (service tag)
  "Return true if SERVICE is tagged with TAG."
  (-contains? (plist-get service :tags) tag))

(defun prodigy-services-tagged-with (tag)
  "Return list of services tagged with TAG."
  (--filter (prodigy-service-tagged-with? it tag) prodigy-services))

(defun prodigy-marked-services ()
  "Return list of services that are marked."
  (--filter (plist-get it :marked) prodigy-services))

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
  (concat "*prodigy-" (s-dashed-words (s-downcase (plist-get service :name))) "*"))

(defun prodigy-start-service (service)
  "Start process associated with SERVICE."
  (let ((process (plist-get service :process)))
    (unless (and process (process-live-p process))
      (let* ((name (plist-get service :name))
             (command (plist-get service :command))
             (args (plist-get service :args))
             (default-directory (f-full (plist-get service :cwd)))
             (exec-path (append (plist-get service :path) exec-path))
             (process nil)
             (create-process
              (lambda ()
                (unless process
                  (setq process (apply 'start-process (append (list name nil command) args)))))))
        (-when-let (init (plist-get service :init))
          (funcall init))
        (-when-let (init-async (plist-get service :init-async))
          (let (callbacked)
            (funcall
             init-async
             (lambda ()
               (setq callbacked t)
               (funcall create-process)))
            (with-timeout
                (prodigy-init-async-timeout
                 (error "Did not callback async callback within %s seconds"
                        prodigy-init-async-timeout))
              (while (not callbacked) (accept-process-output nil 0.005)))))
        (funcall create-process)
        (set-process-filter process 'prodigy-process-filter)
        (set-process-query-on-exit-flag process nil)
        (prodigy-service-set service :process process)))))

(defun prodigy-stop-service (service)
  "Stop process associated with SERVICE."
  (-when-let (process (plist-get service :process))
    (when (process-live-p process)
      (signal-process process (or (plist-get service :stop-signal) 'int)))
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
   (plist-get service :port)
   (-when-let (port (-first
                     (lambda (arg)
                       (s-matches? "^\\([0-9]\\)\\{4,5\\}$" arg))
                     (plist-get service :args)))
     (string-to-number port))))

(defun prodigy-process-filter (process output)
  "Process filter for service processes.

PROCESS is the service process that the OUTPUT is associated to."
  (-when-let (service
              (-first
               (lambda (service)
                 (eq (plist-get service :process) process))
               prodigy-services))
    (let ((buffer (get-buffer-create (prodigy-buffer-name service))))
      (with-current-buffer buffer
        (insert (ansi-color-apply output))))))

(defun prodigy-find-service (name)
  "Find service with NAME."
  (-first
   (lambda (service)
     (equal (plist-get service :name) name))
   prodigy-services))


;;;; User functions

(defun prodigy-quit ()
  "Quit prodigy."
  (interactive)
  (quit-window))

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

(defun prodigy-mark ()
  "Mark service at point."
  (interactive)
  (-when-let (service (prodigy-service-at-line))
    (prodigy-service-set service :marked t)
    (ignore-errors
      (prodigy-goto-next-line))))

(defun prodigy-mark-tag ()
  "Mark all services with tag."
  (interactive)
  (let ((tag (prodigy-read-tag)))
    (-each
     (prodigy-services-tagged-with tag)
     (lambda (service)
       (prodigy-service-set service :marked t)))))

(defun prodigy-mark-all ()
  "Mark all services."
  (interactive)
  (-each
   prodigy-services
   (lambda (service)
     (prodigy-service-set service :marked t))))

(defun prodigy-unmark ()
  "Unmark service at point."
  (interactive)
  (-when-let (service (prodigy-service-at-line))
    (prodigy-service-set service :marked nil)
    (ignore-errors
      (prodigy-goto-next-line))))

(defun prodigy-unmark-tag ()
  "Unmark all services with tag."
  (interactive)
  (let ((tag (prodigy-read-tag)))
    (-each
     (prodigy-services-tagged-with tag)
     (lambda (service)
       (prodigy-service-set service :marked nil)))))

(defun prodigy-unmark-all ()
  "Unmark all services."
  (interactive)
  (-each
   prodigy-services
   (lambda (service)
     (prodigy-service-set service :marked nil))))

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

(defun prodigy-display-process ()
  "Switch to process buffer for service at current line."
  (interactive)
  (-when-let (service (prodigy-service-at-line))
    (when (plist-get service :process)
      (prodigy-log-mode service))))

(defun prodigy-browse ()
  "Browse service url at point if possible to figure out."
  (interactive)
  (-when-let (service (prodigy-service-at-line))
    (-if-let (port (prodigy-service-port service))
        (browse-url (format "http://localhost:%d" port))
      (message "Could not determine port"))))

(defun prodigy-refresh ()
  "Refresh GUI."
  (interactive)
  (let ((line (line-number-at-pos)))
    (prodigy-repaint)
    (prodigy-goto-line line)))

(defun prodigy-log-quit ()
  "Quit window and bury buffer."
  (interactive)
  (quit-window))

(defun prodigy-define-service (&rest args)
  "Define a new service."
  (let ((compare-fn
         (lambda (plist-a plist-b)
           (equal
            (plist-get plist-a :name)
            (plist-get plist-b :name)))))
    (add-to-list 'prodigy-services args 'append compare-fn)))

;;;###autoload
(put 'prodigy-define-service 'lisp-indent-function 'defun)

;;;###autoload
(put 'prodigy-define-service 'doc-string-elt 1)

;;;###autoload
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("(\\(\\<prodigy-define-service\\)\\>"
                (1 font-lock-keyword-face nil t))))))

;;;###autoload
(defun prodigy ()
  "Manage external services from within Emacs."
  (interactive)
  (let ((buffer-p (get-buffer prodigy-buffer-name))
        (buffer (get-buffer-create prodigy-buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (kill-all-local-variables)
      (prodigy-mode))))

;;;###autoload
(defun prodigy-log-mode (service)
  "Open log mode for SERVICE."
  (interactive)
  (pop-to-buffer (prodigy-buffer-name service))
  (kill-all-local-variables)
  (setq mode-name "Prodigy Log")
  (setq major-mode 'prodigy-log-mode)
  (use-local-map prodigy-log-mode-map))

(provide 'prodigy)

;;; prodigy.el ends here
