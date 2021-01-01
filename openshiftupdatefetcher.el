;;; openshiftupdatechecker.el -- get openshift update paths

;; My first "proper" elisp stuff, be gentle.

(require 'seq) ;; needed for seq-filter
(require 'ido) ;; needed for completion

(defun openshiftupdatechecker ()
  "Fetch possible updates for OpenShift "
  (interactive)
  (openshiftupdatechecker-get-channel-json)
  (openshiftupdatechecker-find-paths))

(defvar *json-output* nil)
(defvar *version* nil)

(defun openshiftupdatechecker-prompt-version ()
  "Get version from user"
  (interactive)
  (read-string "Enter version (4.4.17): "))

(defun openshiftupdatechecker-prompt-channel ()
  "Ask update channel"
  (interactive)
  (let ((choices (list "stable-4.5" "stable-4.6" "candidate-4.6")))
    (ido-completing-read "Update channel:" choices)))

(defun openshiftupdatechecker-get-channel-json ()
  "Get output from curl"
  (let ((channel (openshiftupdatechecker-prompt-channel))
        (version (openshiftupdatechecker-prompt-version)))
    (setq *version* version)
    (message "version to be searched: %s" *version*)
    (setq *json-output*
          (json-parse-string
           (shell-command-to-string
            (concat "curl -sqH 'Accept:application/json' 'https://api.openshift.com/api/upgrades_info/v1/graph?channel='" channel))))))

(defun openshiftupdatefetcher-flatten (x)
  "Flatten function"
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun openshiftupdatechecker-find-paths ()
  "Find upgrade paths and print it"
  (interactive)
  (let* ((nodes (gethash "nodes" *json-output*))
         (original-edges (gethash "edges" *json-output*))
         (versions (mapcar (lambda (x) (gethash "version" x)) nodes))
         (edges (mapcar (lambda (x)
                          (list (nth (elt x 0) versions) (nth (elt x 1) versions)))
                        original-edges))
         (matching-edges (seq-filter (lambda (x)
                                       (string= *version* (elt x 0))
                                       ) edges))
         (only-updates (mapcar 'cdr matching-edges)))
    (message "Possible versions to upgrade: %S" (sort (openshiftupdatefetcher-flatten only-updates) #'string<))
    ))
