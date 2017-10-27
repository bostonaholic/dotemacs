;;; helm.el

(use-package helm
  :defer 1
  :preface
  (defun msb/helm-find-files (arg)
    "Custom implementation for calling helm-find-files-1.
Removes the automatic guessing of the initial value based on thing at point. "
    (interactive "P")
    (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
           (default-input hist)
           (input (cond ((and (eq major-mode 'dired-mode) default-input)
                         (file-name-directory default-input))
                        ((and (not (string= default-input ""))
                              default-input))
                        (t (expand-file-name (helm-current-directory))))))
      (set-text-properties 0 (length input) nil input)
      (helm-find-files-1 input)))
  (defun msb//helm-cleanup ()
    "Cleanup some helm related states when quitting."
    ;; deactivate any running transient map (transient-state)
    (setq overriding-terminal-local-map nil))
  (defun msb//helm-find-files-edit (candidate)
    "Opens a dired buffer and immediately switches to editable mode."
    (dired (file-name-directory candidate))
    (dired-goto-file candidate)
    (dired-toggle-read-only))
  (defun msb/helm-find-files-edit ()
    "Exits helm, opens a dired buffer and immediately switches to editable mode."
    (interactive)
    (helm-exit-and-execute-action 'msb//helm-find-files-edit))
  :commands (msb/helm-find-files)
  :init
  (add-hook 'helm-cleanup-hook #'msb//helm-cleanup)
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  :config
  (helm-mode)
  (helm-locate-set-command)
  (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))
  :bind
  (("C-x C-f" . msb/helm-find-files)
   ("C-x b" . helm-buffers-list)))

;;; helm.el
