;;; prodigy service example ssh tunnels
;; This example uses `prodigy-callback' in a tag definition to make it
;; easy to define new tunnels: The generic :ARGS of the tag access the
;; service definition to get the tunel specific data (propery :TUNNEL)
;; as property list and pass that to the helper
;; `my-build-tunnel-args'.
;; 
(defun my-build-tunnel-args (args)
  "Assemble the ssh tunnel argument list."
  `("-v" ;; allows us to parse for the ready message
    "-N" ;; don't start an interactive shell remotely
    "-L" ,(concat (getf args :localport) ;; the tunnel spec
                  ":"
                  (getf args :tunnel-ip)
                  ":"
                  (getf args :tunnel-port))
    "-l" ,(getf args :user) ;; the username
    "-p" ,(getf args :port) ;; the remote port
    ,(getf args :host)))    ;; the remote host

(prodigy-define-tag
  :name 'ssh-tunnel
  :command "ssh"
  :cwd (getenv "HOME")
  :args (prodigy-callback (service)
                          (my-build-tunnel-args
                           (getf service :tunnel)))
  :ready-message "debug1: Entering interactive session.")

(prodigy-define-service
  :name "remote end"
  :tags '(ssh-tunnel)
  :tunnel (list 
           :localport  "34343"
           :tunnel-ip  "192.168.87.112"
           :tunnel-port  "22"
           :user  "user"
           :host  "host.to-connect.to"
           :port  "22"))
