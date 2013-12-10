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

(Then "^I should be on line \"\\([^\"]+\\)\"$"
  (lambda (line)
    (should (= (line-number-at-pos (point)) (string-to-number line)))))

(Then "^I should be on service line \"\\([^\"]+\\)\"$"
  (lambda (line)
    (should (= (current-column) 0))
    (should (= (line-number-at-pos (point)) (string-to-number line)))
    (should (eq (get-text-property (line-beginning-position) 'face) 'prodigy-line-face))
    (should (eq (get-text-property (line-end-position) 'face) 'prodigy-line-face))))

(Given "^I add the following processes:$"
  (lambda (table)
    (let ((head (car table))
          (rows (cdr table)))
      (dolist (row rows)
        (prodigy-define-service
         :name (nth 0 row))))))

(Then "^I should see the following processes:$"
  (lambda (table)
    (let ((head (car table))
          (rows (cdr table)))
      (should
       (= (- (line-number-at-pos (point-max))
             (line-number-at-pos (point-min)))
          (length rows)))
      (save-excursion
        (goto-char (point-min))
        (dolist (row rows)
          (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))))
            (should (equal (nth 0 row) line))
            (forward-line 1)))))))
