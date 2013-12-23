;; NOTE:
;;
;; Using Curl here because for some weird reason, using
;; `url-retrieve-synchronously' fails with the message "Emacs is not
;; compiled with network support". That however only is an issue
;; inside the step.

(Given "^I start prodigy$"
  (lambda ()
    (call-interactively 'prodigy)))

(Then "^I should be in prodigy mode$"
  (lambda ()
    (should (equal major-mode 'prodigy-mode))
    (should (equal mode-name "Prodigy"))))

(Then "^the buffer should be read only$"
  (lambda ()
    (should buffer-read-only)))

(Then "^the variable \"\\([^\"]+\\)\" should be undefined$"
  (lambda (variable-name)
    (should-not (boundp (intern variable-name)))))

(Then "^the variable \"\\([^\"]+\\)\" should have value \"\\([^\"]+\\)\"$"
  (lambda (variable-name value)
    (should (equal (eval (intern variable-name)) value))))

(Then "^I should not be in prodigy mode$"
  (lambda ()
    (should-not (equal major-mode 'prodigy-mode))))

(Given "^I add the following services:$"
  (lambda (table)
    (let* ((head (car table))
           (rows (cdr table))
           (name-index (-elem-index "name" head))
           (tags-index (-elem-index "tags" head))
           (cwd-index (-elem-index "cwd" head))
           (command-index (-elem-index "command" head))
           (args-index (-elem-index "args" head))
           (init-index (-elem-index "init" head))
           (init-async-index (-elem-index "init-async" head))
           (path-index (-elem-index "path" head))
           (env-index (-elem-index "env" head)))
      (mapc
       (lambda (row)
         (prodigy-define-service
           :name (nth name-index row)
           :tags (and tags-index (read (nth tags-index row)))
           :cwd (or (and cwd-index (f-expand (nth cwd-index row) prodigy-servers-path)) "cwd")
           :command (or (and command-index (nth command-index row)) "command")
           :args (and args-index (read (nth args-index row)))
           :init (and init-index (read (nth init-index row)))
           :init-async (and init-async-index (read (nth init-async-index row)))
           :path (and path-index (--map (f-expand it prodigy-servers-path) (read (nth path-index row))))
           :env (and env-index (read (nth env-index row)))))
       rows))))

(Then "^I should see services:$"
  (lambda (table)
    (save-excursion
      (let* ((overlays (overlays-in (line-beginning-position) (line-end-position)))
             (highlighted-line
              (and
               (eq 'prodigy-line-face (car (--map (overlay-get it 'face) overlays)))
               (line-number-at-pos)))
             (head (car table))
             (rows (cdr table))
             (name-index (-elem-index "name" head))
             (marked-index (-elem-index "marked" head))
             (highlighted-index (-elem-index "highlighted" head))
             (started-index (-elem-index "started" head))
             (tags-index (-elem-index "tags" head)))
        (goto-char (point-min))
        (-each
         rows
         (lambda (row)
           (let* ((expected-name        (and name-index (nth name-index row)))
                  (expected-marked      (and marked-index (read (nth marked-index row))))
                  (expected-highlighted (and highlighted-index (read (nth highlighted-index row))))
                  (expected-started     (and started-index (read (nth started-index row))))
                  (expected-tags        (and tags-index (read (nth tags-index row))))
                  (entry (tabulated-list-get-entry))
                  (actual-name (aref entry 1))
                  (actual-marked (aref entry 0))
                  (actual-started (aref entry 2))
                  (actual-tags
                   (-map 'intern (s-split ", " (aref entry 3) 'omit-nulls))))
             (when expected-name
               (should (string= expected-name actual-name)))
             (when expected-marked
               (should (string= "*" actual-marked)))
             (when expected-highlighted
               (should (= (line-number-at-pos) highlighted-line)))
             (when expected-started
               (should actual-started))
             (when expected-tags
               (should (equal expected-tags actual-tags))))
           (forward-line 1)))))))

(Then "^requesting \"\\([^\"]+\\)\" should respond with \"\\([^\"]+\\)\"$"
  (lambda (url response callback)
    (let ((finish-func
           `(lambda (process)
              (with-current-buffer (process-buffer process)
                (when (string= (buffer-string) ,response)
                  (funcall ,callback))))))
      (async-start-process "curl" "curl" finish-func "-s" url))))

(Then "^requesting \"\\([^\"]+\\)\" should not respond$"
  (lambda (url callback)
    (async-start
     `(lambda ()
        (with-temp-buffer
          (call-process "curl" nil t nil "-s" "-S" ,url)
          (buffer-string)))
     `(lambda (response)
        (when (or (s-contains? "Connection refused" response)
                  (s-contains? "couldn't connect to host" response))
          (funcall ,callback))))))

(When "^I request \"\\([^\"]+\\)\"$"
  (lambda (url callback)
    (async-start-process "curl" "curl" callback url)))

(When "^I start services?$"
  (lambda (callback)
    (When "I press \"s\"")
    (async-start-process "sleep" "sleep" callback "2")))

(When "^I stop services?$"
  (lambda (callback)
    (When "I press \"S\"")
    (async-start-process "sleep" "sleep" callback "1")))

(When "^I restart services?$"
  (lambda (callback)
    (When "I press \"r\"")
    (async-start-process "sleep" "sleep" callback "1")))

(Then "^the buffer \"\\([^\"]+\\)\" should exist$"
  (lambda (buffer-name)
    (should (get-buffer buffer-name))))

(When "^I kill the prodigy buffer$"
  (lambda ()
    (kill-buffer prodigy-buffer-name)))

(Then "^view mode should be enabled$"
  (lambda ()
    (should view-mode)))

(When "^I filter by tag \"\\([^\"]+\\)\"$"
  (lambda (tag)
    (When "I start an action chain")
    (And "I press \"f\"")
    (And "I press \"t\"")
    (And "I type \"%s\"" tag)
    (And "I press \"RET\"")
    (And "I execute the action chain")))

(When "^I filter by name \"\\([^\"]+\\)\"$"
  (lambda (name)
    (When "I start an action chain")
    (And "I press \"f\"")
    (And "I press \"n\"")
    (And "I type \"%s\"" name)
    (And "I press \"RET\"")
    (And "I execute the action chain")))

(Then "^I should see services no services$"
  (lambda ()
    (should (string= (buffer-string) ""))))

(Then "^I should be on line \"\\([^\"]+\\)\"$"
  (lambda (line)
    (should (= (line-number-at-pos) (read line)))))
