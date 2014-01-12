(require 'f)

(defvar prodigy-support-path
  (f-dirname load-file-name))

(defvar prodigy-features-path
  (f-parent prodigy-support-path))

(defvar prodigy-root-path
  (f-parent prodigy-features-path))

(defvar prodigy-buffer-list (buffer-list))

(require 'prodigy (f-expand "prodigy" prodigy-root-path))
(require 'espuds)
(require 'async)
(require 'ert)
(require 'magit)
(require 'dired)

(Before
 (setq prodigy-services nil)
 (setq prodigy-filters nil)
 
 (-each (buffer-list)
        (lambda (buffer)
          (unless (-contains? prodigy-buffer-list buffer)
            (kill-buffer buffer))))
 (delete-other-windows))
