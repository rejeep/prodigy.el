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
 (-when-let (buffer (get-buffer prodigy-buffer-name))
   (kill-buffer buffer))
 (ht-clear prodigy-services)
 (makunbound 'foo))
