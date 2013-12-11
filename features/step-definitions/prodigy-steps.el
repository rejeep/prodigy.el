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
    (let ((head (car table))
          (rows (cdr table)))
      (dolist (row rows)
        (prodigy-define-service
          :name (nth 0 row))))))

(Then "^I should see services:$"
  (lambda (table)
    (let* ((head (car table))
           (name-index (-elem-index "name" head))
           (highlighted-index (-elem-index "highlighted" head))
           (marked-index (-elem-index "marked" head))
           (rows (cdr table))
           )
      (save-excursion
        (goto-char (point-min))
        (-each
         rows
         (lambda (row)
           (let ((name (nth name-index row))
                 (highlighted
                  (when highlighted-index
                    (read (nth highlighted-index row))))
                 (marked
                  (when marked-index
                    (read (nth marked-index row)))))
             (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                         (line-end-position))))
               (if marked
                   (should (s-starts-with? (concat "* " name) line))
                 (should (s-starts-with? (concat "  " name) line))))
             (when highlighted
               (should (eq (get-text-property (1+ (line-beginning-position)) 'face) 'prodigy-line-face))
               (should (eq (get-text-property (line-end-position) 'face) 'prodigy-line-face)))
             (forward-line 1))))))))
