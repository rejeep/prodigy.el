;;; prodigy.el --- Manage external services from within Emacs

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.2.0
;; URL: http://github.com/rejeep/prodigy.el
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (f "0.14.0") (emacs "24"))

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
(require 'tabulated-list)
(require 'easymenu)

(eval-when-compile
  (declare-function discover-add-context-menu "discover")
  (declare-function magit-status "magit"))

(defgroup prodigy nil
  "Manage external services from within Emacs."
  :prefix "prodigy-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/rejeep/prodigy.el"))

(defface prodigy-line-face
  '((((class color)) :background "#4A708B"))
  "Color of current line."
  :group 'prodigy)

(defface prodigy-fail-face
  '((((class color)) :foreground "firebrick"))
  "Face to indicate starting service failed."
  :group 'prodigy)

(defface prodigy-success-face
  '((((class color)) :foreground "SeaGreen"))
  "Face to indicate starting service was successful."
  :group 'prodigy)

(defcustom prodigy-completion-system 'ido
  "The completion system to be used by Prodigy."
  :group 'prodigy
  :type 'symbol
  :options '(ido default))

(defcustom prodigy-init-async-timeout 10
  "Seconds to wait for init async callback before failing."
  :group 'prodigy
  :type 'number)

(defcustom prodigy-kill-process-buffer-on-stop nil
  "Will kill process buffer on stop if this is true."
  :group 'prodigy
  :type 'boolean)

(defvar prodigy-mode-hook nil
  "Mode hook for `prodigy-mode'.")

(defvar prodigy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'prodigy-next)
    (define-key map (kbd "p") 'prodigy-prev)
    (define-key map (kbd "M-<") 'prodigy-first)
    (define-key map (kbd "M->") 'prodigy-last)
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
    (define-key map (kbd "f t") 'prodigy-add-tag-filter)
    (define-key map (kbd "f n") 'prodigy-add-name-filter)
    (define-key map (kbd "F") 'prodigy-clear-filters)
    (define-key map (kbd "j m") 'prodigy-jump-magit)
    (define-key map (kbd "j d") 'prodigy-jump-dired)
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
:path         - List of directories added to PATH when command runs
:env          - List of lists (with two items).  First item is the name
                of an environment variable and second item is the
                value of the variable
:url          - Url to use for browsing")

(defvar prodigy-filters nil
  "List of filters.

Each filter is a list of two elements where the first item is the
type of filter and the value is what should be filtered.

Supported filters:

:tag  - name of tag that service must include
:name - string that service name must contain")

(defconst prodigy-buffer-name "*prodigy*"
  "Name of Prodigy mode buffer.")

(defconst prodigy-list-format
  [("Marked" 6 t :right-align t)
   ("Name" 40 t)
   ("Status" 10 t)
   ("Tags" 30 nil)]
  "List format.")

(defconst prodigy-list-sort-key
  '("Name" . nil)
  "Sort table on this key.")

(defconst prodigy-discover-context-menu
  '(prodigy
    (actions
     ("Navigation"
      ("n" "next service" prodigy-next)
      ("p" "prev service" prodigy-prev)
      ("M-<" "first service" prodigy-first)
      ("M->" "last service" prodigy-last))
     ("Marking"
      ("m" "mark service" prodigy-mark)
      ("t" "mark services with tag" prodigy-mark-tag)
      ("M" "mark all services" prodigy-mark-all)
      ("u" "unmark service" prodigy-unmark)
      ("T" "unmark services with tag" prodigy-unmark-tag)
      ("U" "unmark all services" prodigy-unmark-all))
     ("Process"
      ("s" "start service" prodigy-start)
      ("S" "stop service" prodigy-stop)
      ("r" "restart service" prodigy-restart)
      ("$" "display service process buffer" prodigy-display-process))
     ("Filters"
      ("f t" "add tag filter" prodigy-add-tag-filter)
      ("f n" "add name filter" prodigy-add-name-filter)
      ("F" "clear all filters" prodigy-clear-filters))
     ("Misc"
      ("o" "open in browser" prodigy-browse))))
  "The discover context menu.")

(easy-menu-define prodigy-mode-menu prodigy-mode-map
  "Prodigy menu"
  '("Prodigy"
    ["Next service" prodigy-next t]
    ["Previous service" prodigy-prev t]
    ["First service" prodigy-first t]
    ["Last service" prodigy-last t]
    "---"
    ["Mark service" prodigy-mark]
    ["Mark services with tag" prodigy-mark-tag]
    ["Mark all services" prodigy-mark-all]
    ["Unmark service" prodigy-unmark]
    ["Unmark services with tag" prodigy-unmark-tag]
    ["Unmark all services" prodigy-unmark-all]
    "---"
    ["Start service" prodigy-start]
    ["Stop service" prodigy-stop]
    ["Restart service" prodigy-restart]
    ["Display service process buffer" prodigy-display-process]
    "---"
    ["Add tag filter" prodigy-add-tag-filter]
    ["Add name filter" prodigy-add-name-filter]
    ["Clear all filters" prodigy-clear-filters]
    "---"
    ["Open in browser" prodigy-browse]))


;;;; Internal functions

(defun prodigy-services ()
  "Return list of services, with applied filters."
  (let ((services (-clone prodigy-services)))
    (-each
     prodigy-filters
     (lambda (filter)
       (let ((type (-first-item filter))
             (value (-last-item filter)))
         (cond ((eq type :tag)
                (setq services (-select
                                (lambda (service)
                                  (prodigy-service-tagged-with? service value))
                                services)))
               ((eq type :name)
                (setq services (-select
                                (lambda (service)
                                  (s-contains? value (plist-get service :name) 'ignore-case))
                                services)))))))
    services))

(defun prodigy-service-at-pos (&optional pos)
  "Return service at POS or current position."
  (prodigy-find-by-id (tabulated-list-get-id pos)))

(defun prodigy-service-at-pos-p (&optional pos)
  "Return true if there is a service at POS or current position."
  (not (null (prodigy-service-at-pos pos))))

(defun prodigy-goto-next-line ()
  "Go to next line."
  (prodigy-goto-pos (line-beginning-position 2)))

(defun prodigy-goto-prev-line ()
  "Go to previous line."
  (prodigy-goto-pos (line-beginning-position 0)))

(defun prodigy-goto-first-line ()
  "Go to first line."
  (prodigy-goto-pos (point-min)))

(defun prodigy-goto-last-line ()
  "Go to first line."
  (prodigy-goto-pos
   (save-excursion
     (goto-char (point-max))
     (line-beginning-position 0))))

(defun prodigy-goto-pos (pos)
  "Go to POS."
  (if (prodigy-service-at-pos-p pos)
      (goto-char pos)
    (error "No service at point %s" pos)))

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

(defun prodigy-status-face (process)
  "Return face depending on PROCESS state."
  (let ((status (process-status process)))
    (if (eq status 'run)
        'prodigy-success-face
      'prodigy-fail-face)))

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
             (env (--map (s-join "=" it) (plist-get service :env)))
             (process-environment (append env process-environment))
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
        (plist-put service :process process)))))

(defun prodigy-stop-service (service)
  "Stop process associated with SERVICE."
  (-when-let (process (plist-get service :process))
    (when (process-live-p process)
      (signal-process process (or (plist-get service :stop-signal) 'int)))
    (plist-put service :process nil))
  (let ((kill-process-buffer-on-stop (plist-get service :kill-process-buffer-on-stop)))
    (when (or kill-process-buffer-on-stop prodigy-kill-process-buffer-on-stop)
      (-when-let (buffer (get-buffer (prodigy-buffer-name service)))
        (kill-buffer buffer)))))

(defun prodigy-apply (fn)
  "Apply FN to service at line or marked services."
  (let ((services (prodigy-marked-services)))
    (if services
        (-each services fn)
      (-when-let (service (prodigy-service-at-pos))
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
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert (ansi-color-apply output))))))))

(defun prodigy-find-service (name)
  "Find service with NAME."
  (-first
   (lambda (service)
     (equal (plist-get service :name) name))
   prodigy-services))

(defun prodigy-highlight ()
  "Highlight current line."
  (when (eq major-mode 'prodigy-mode)
    (let ((beg (line-beginning-position))
          (end (line-beginning-position 2))
          (inhibit-read-only t))
      (overlay-put (make-overlay beg end) 'face 'prodigy-line-face))))

(defun prodigy-unhighlight ()
  "Unhighlight current line."
  (when (eq major-mode 'prodigy-mode)
    (let ((inhibit-read-only t))
      (remove-overlays (line-beginning-position) (line-beginning-position 2)))))

(defun prodigy-service-id (service)
  "Return SERVICE identifier."
  (let* ((name (plist-get service :name))
         (name (s-downcase name))
         (name (s-replace " " "-" name)))
    (intern name)))

(defun prodigy-marked-col (service)
  "Return SERVICE marked column."
  (if (plist-get service :marked) "*" ""))

(defun prodigy-name-col (service)
  "Return SERVICE name column."
  (plist-get service :name))

(defun prodigy-status-col (service)
  "Return SERVICE status column."
  (-if-let (process (plist-get service :process))
      (propertize (prodigy-status-name process) 'face (prodigy-status-face process))
    ""))

(defun prodigy-tags-col (service)
  "Return SERVICE tags column."
  (s-join ", " (-map 'symbol-name (plist-get service :tags))))

(defun prodigy-list-entries ()
  "Create the entries for the service list."
  (-map
   (lambda (service)
     (list
      (prodigy-service-id service)
      (apply 'vector
             (--map
              (funcall it service)
              '(prodigy-marked-col
                prodigy-name-col
                prodigy-status-col
                prodigy-tags-col)))))
   (prodigy-services)))

(defun prodigy-find-by-id (id)
  "Find service by identifier ID."
  (--first (eq (prodigy-service-id it) id) prodigy-services))

(defun prodigy-url (service)
  "Return SERVICE url."
  (or
   (plist-get service :url)
   (-when-let (port (prodigy-service-port service))
     (format "http://localhost:%d" port))))

(defmacro prodigy-with-refresh (&rest body)
  "Execute BODY and then refresh."
  `(progn ,@body (prodigy-refresh)))

(defun prodigy-discover-initialize ()
  "Initialize discover by adding prodigy context menu."
  (discover-add-context-menu
   :context-menu prodigy-discover-context-menu
   :bind "?"
   :mode 'prodigy-mode
   :mode-hook 'prodigy-mode-hook))


;;;; User functions

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

(defun prodigy-first ()
  "Go to first service."
  (interactive)
  (prodigy-goto-first-line))

(defun prodigy-last ()
  "Go to lsat service."
  (interactive)
  (prodigy-goto-last-line))

(defun prodigy-mark ()
  "Mark service at point."
  (interactive)
  (prodigy-with-refresh
   (-when-let (service (prodigy-service-at-pos))
     (plist-put service :marked t)
     (ignore-errors
       (prodigy-goto-next-line)))))

(defun prodigy-mark-tag ()
  "Mark all services with tag."
  (interactive)
  (prodigy-with-refresh
   (let ((tag (prodigy-read-tag)))
     (-each
      (prodigy-services-tagged-with tag)
      (lambda (service)
        (plist-put service :marked t))))))

(defun prodigy-mark-all ()
  "Mark all services."
  (interactive)
  (prodigy-with-refresh
   (-each
    prodigy-services
    (lambda (service)
      (plist-put service :marked t)))))

(defun prodigy-unmark ()
  "Unmark service at point."
  (interactive)
  (-when-let (service (prodigy-service-at-pos))
    (prodigy-with-refresh
     (plist-put service :marked nil)
     (ignore-errors
       (prodigy-goto-next-line)))))

(defun prodigy-unmark-tag ()
  "Unmark all services with tag."
  (interactive)
  (prodigy-with-refresh
   (let ((tag (prodigy-read-tag)))
     (-each
      (prodigy-services-tagged-with tag)
      (lambda (service)
        (plist-put service :marked nil))))))

(defun prodigy-unmark-all ()
  "Unmark all services."
  (interactive)
  (prodigy-with-refresh
   (-each
    prodigy-services
    (lambda (service)
      (plist-put service :marked nil)))))

(defun prodigy-start ()
  "Start service at line or marked services."
  (interactive)
  (prodigy-with-refresh
   (prodigy-apply 'prodigy-start-service)))

(defun prodigy-stop ()
  "Stop service at line or marked services."
  (interactive)
  (prodigy-with-refresh
   (prodigy-apply 'prodigy-stop-service)))

(defun prodigy-restart ()
  "Restart service at line or marked services."
  (interactive)
  (prodigy-with-refresh
   (prodigy-apply 'prodigy-stop-service)
   (prodigy-apply 'prodigy-start-service)))

(defun prodigy-display-process ()
  "Switch to process buffer for service at current line."
  (interactive)
  (-when-let (service (prodigy-service-at-pos))
    (-if-let (buffer (get-buffer (prodigy-buffer-name service)))
        (progn (pop-to-buffer buffer) (view-mode 1))
      (message "Nothing to show for %s" (plist-get service :name)))))

(defun prodigy-browse ()
  "Browse service url at point if possible to figure out."
  (interactive)
  (-when-let (service (prodigy-service-at-pos))
    (-if-let (url (prodigy-url service))
        (browse-url url)
      (message "Could not determine port"))))

(defun prodigy-refresh ()
  "Refresh list of services."
  (interactive)
  (tabulated-list-print :remember-pos))

(defun prodigy-add-tag-filter ()
  "Read tag and add filter so that only services with that tag show."
  (interactive)
  (prodigy-with-refresh
   (let ((tag (prodigy-read-tag)))
     (prodigy-add-filter :tag tag)))
  (ignore-errors
    (prodigy-goto-first-line)))

(defun prodigy-add-name-filter ()
  "Read string and add filter for name."
  (interactive)
  (prodigy-with-refresh
   (let ((string (read-string "string: ")))
     (prodigy-add-filter :name string))
   (ignore-errors
     (prodigy-goto-first-line))))

(defun prodigy-clear-filters ()
  "Clear all filters."
  (interactive)
  (prodigy-with-refresh
   (setq prodigy-filters nil))
  (prodigy-goto-first-line))

(defun prodigy-jump-magit ()
  "Jump to magit status mode for service at point."
  (interactive)
  (-when-let (service (prodigy-service-at-pos))
    (magit-status (plist-get service :cwd))))

(defun prodigy-jump-dired ()
  "Jump to dired mode for service at point."
  (interactive)
  (-when-let (service (prodigy-service-at-pos))
    (dired (plist-get service :cwd))))


;;;; Misc

(defun prodigy-add-filter (type value)
  "Add filter TYPE, that filters for VALUE."
  (push (list type value) prodigy-filters))

(defun prodigy-define-service (&rest args)
  "Define a new service with ARGS."
  (-when-let (service-name (plist-get args :name))
    (setq
     prodigy-services
     (-reject
      (lambda (service)
        (string= (plist-get service :name) service-name))
      prodigy-services)))
  (push args prodigy-services)
  nil)

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
(define-derived-mode prodigy-mode tabulated-list-mode "Prodigy"
  "Special mode for prodigy buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Prodigy")
  (setq major-mode 'prodigy-mode)
  (use-local-map prodigy-mode-map)
  (add-hook 'pre-command-hook 'prodigy-unhighlight)
  (add-hook 'post-command-hook 'prodigy-highlight)
  (setq tabulated-list-format prodigy-list-format)
  (setq tabulated-list-entries 'prodigy-list-entries)
  (setq tabulated-list-sort-key prodigy-list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (when (featurep 'discover)
    (prodigy-discover-initialize))
  (run-mode-hooks 'prodigy-mode-hook))

;;;###autoload
(defun prodigy ()
  "Manage external services from within Emacs."
  (interactive)
  (let ((buffer-p (get-buffer prodigy-buffer-name))
        (buffer (get-buffer-create prodigy-buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (prodigy-mode))
    (prodigy-highlight)))

(provide 'prodigy)

;;; prodigy.el ends here
