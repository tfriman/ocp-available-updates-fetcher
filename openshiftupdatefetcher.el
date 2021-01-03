;;; -*- lexical-binding: t; -*-
;;; openshiftupdatechecker.el -- get openshift update paths

;; Searches OpenShift's possible version updates via REST api. Asks channel and version as input.
;; My first "proper" elisp stuff, be gentle.
;;
;; Usage: M-x eval-buffer and after that M-x openshiftupdatechecker
;; "UI" version: M-x openshiftupdatechecker-widget-version
;; Dependencies: expects curl to be in $PATH.
;;
;; Timo Friman, Licence: MIT

(require 'seq) ;; needed for seq-filter
(require 'ido) ;; needed for completion

(require 'widget)
(eval-when-compile
 (require 'wid-edit))

(defvar version-widget)

(defun openshiftupdatechecker-widget-version ()
  "Widget based search"
  (interactive)
  (switch-to-buffer "*OCP UPDATE*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Openshift update checker\n\n")
  (widget-insert "Select update channel\n")
  (widget-create 'radio-button-choice
                 :notify (lambda (widget &rest ignore)
                                 (let* ((channel (widget-value widget))
                                        (json (openshiftupdatechecker-channel-json channel))
                                        (version (openshiftupdatechecker-prompt-version
                                                  (openshiftupdatechecker-find-versions json))))
                                   ;;(message "Selected channel %s" channel)
                                   (widget-value-set version-widget
                                                     (openshiftupdatechecker-find-paths json version))                                  ))
                 '(item "stable-4.5")
                 '(item "stable-4.6")
                 '(item "candidate-4.6"))
  (widget-insert "And results")
  (setq version-widget
        (widget-create 'text
                       :))
  (use-local-map widget-keymap)
  (widget-setup))

(defun openshiftupdatechecker ()
  "Fetch possible updates for OpenShift "
  (interactive)
  (let* ((json (openshiftupdatechecker-get-channel-json))
         (versions (openshiftupdatechecker-find-versions json))
         (version (openshiftupdatechecker-prompt-version versions)))
    (openshiftupdatechecker-find-paths json version)))

(defun openshiftupdatechecker-find-versions (json)
  "Get versions from json"
  (mapcar (lambda (x) (gethash "version" x)) (gethash "nodes" json)))

(defun openshiftupdatechecker-prompt-version (versions)
  "Prompt version"
  (interactive)
  (let ((version (ido-completing-read "Enter version: " versions)))
    (message "version to be searched: %s" version)
    version))

(defun openshiftupdatechecker-prompt-channel ()
  "Prompt update channel"
  (interactive)
  (let ((choices (list "stable-4.5" "stable-4.6" "candidate-4.6")))
    (ido-completing-read "Update channel:" choices)))

(defun openshiftupdatechecker-get-channel-json ()
  "Get output from curl as JSON"
  (let ((channel (openshiftupdatechecker-prompt-channel)))
    (openshiftupdatechecker-channel-json channel)))

(defun openshiftupdatechecker-channel-json (channel)
  "Get output from curl as JSON"
  (json-parse-string
   (shell-command-to-string
    (concat "curl -sqH 'Accept:application/json' "
            "'https://api.openshift.com/api/upgrades_info/v1/graph?channel='"
            channel))))

(defun openshiftupdatefetcher-flatten (x)
  "Flatten function"
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec (car x) (rec (cdr x) acc))))))
             (rec x nil)))

(defun openshiftupdatechecker-find-paths (json version)
  "Find upgrade paths and print them."
  (interactive)
  (let* ((nodes (gethash "nodes" json))
         (original-edges (gethash "edges" json))
         (versions (mapcar (lambda (x) (gethash "version" x)) nodes))
         (edges (mapcar (lambda (x)
                                (list (nth (elt x 0) versions) (nth (elt x 1) versions)))
                        original-edges))
         (matching-edges (seq-filter (lambda (x)
                                             (string= version (elt x 0))
                                             ) edges))
         (only-updates (mapcar 'cdr matching-edges))
         (updates (sort (openshiftupdatefetcher-flatten only-updates) #'string<)))
    (message "Possible versions to upgrade: %S" updates)))
