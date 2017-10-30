;;; packages.el

(use-package helm
  :defer 1
  :preface (load-file (expand-file-name "lisp/helm/funcs.el" user-emacs-directory))
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

(use-package helm-ag
  :after helm
  :preface (defvar search-tools '("ag" "pt" "ack" "grep"))

  :bind (("C-c /" . msb/helm-project-smart-do-search)
         ("C-c *" . msb/helm-project-smart-do-search-region-or-symbol))

  :init
  ;; This overrides the default C-s action in helm-projectile-switch-project
  ;; to search using ag/pt/whatever instead of just grep
  (with-eval-after-load 'helm-projectile
    (defun msb/helm-project-smart-do-search-in-dir (dir)
      (interactive)
      (let ((default-directory dir))
        (msb/helm-project-smart-do-search)))
    (define-key helm-projectile-projects-map
      (kbd "C-s")
      (lambda ()
        (interactive)
        (helm-exit-and-execute-action
         'msb/helm-project-smart-do-search-in-dir)))))

;;; packages.el ends here
