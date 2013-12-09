(require 'f)

(defvar prodigy-support-path
  (f-dirname load-file-name))

(defvar prodigy-features-path
  (f-parent prodigy-support-path))

(defvar prodigy-root-path
  (f-parent prodigy-features-path))

(require 'prodigy (f-expand "prodigy" prodigy-root-path))
(require 'espuds)
(require 'ert)

(Before
 (setq prodigy-services nil)
 (makunbound 'foo))
