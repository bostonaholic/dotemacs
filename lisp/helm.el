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

(use-package helm-ag
  :after helm
  :preface
  (defvar search-tools '("ag" "pt" "ack" "grep"))
  (defun msb//helm-do-ag-region-or-symbol (func &optional dir)
    "Search with `ag' with a default input."
    (require 'helm-ag)
    (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
               ;; make thing-at-point choosing the active region first
               ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
               ((symbol-function 'thing-at-point)
                (lambda (thing)
                  (let ((res (if (region-active-p)
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))
                               (this-fn thing))))
                    (when res (rxt-quote-pcre res))))))
      (funcall func dir)))
  (defun msb//helm-do-search-find-tool (base tools default-inputp)
    "Create a cond form given a TOOLS string list and evaluate it."
    (eval
     `(cond
       ,@(mapcar
          (lambda (x)
            `((executable-find ,x)
              ',(let ((func
                       (intern
                        (format (if default-inputp
                                    "msb/%s-%s-region-or-symbol"
                                  "msb/%s-%s")
                                base x))))
                  (if (fboundp func)
                      func
                    (intern (format "%s-%s"  base x))))))
          tools)
       (t 'helm-do-grep))))
  (defun msb/helm-file-do-ag (&optional _)
    "Wrapper to execute `helm-ag-this-file.'"
    (interactive)
    (helm-do-ag-this-file))
  (defun msb/helm-file-do-ag-region-or-symbol ()
    "Search in current file with `ag' using a default input."
    (interactive)
    (msb//helm-do-ag-region-or-symbol 'msb/helm-file-do-ag))
  (defun msb/helm-file-smart-do-search (&optional default-inputp)
    "Search in current file using `search-tools'.
Search for a search tool in the order provided by `search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
    (interactive)
    (call-interactively
     (msb//helm-do-search-find-tool "helm-file-do"
                                    search-tools
                                    default-inputp)))

  (defun msb/helm-file-smart-do-search-region-or-symbol ()
    "Search in current file using `search-tools' with
 default input.
Search for a search tool in the order provided by `search-tools'."
    (interactive)
    (msb/helm-file-smart-do-search t))

  ;; Search in files -----------------------------------------------------


  (defun msb/helm-files-do-ag (&optional dir)
    "Search in files with `ag' using a default input."
    (interactive)
    (helm-do-ag dir))

  (defun msb/helm-files-do-ag-region-or-symbol ()
    "Search in files with `ag' using a default input."
    (interactive)
    (msb//helm-do-ag-region-or-symbol 'msb/helm-files-do-ag))

  (defun msb/helm-files-do-ack (&optional dir)
    "Search in files with `ack'."
    (interactive)
    (let ((helm-ag-base-command "ack --nocolor --nogroup"))
      (helm-do-ag dir)))

  (defun msb/helm-files-do-ack-region-or-symbol ()
    "Search in files with `ack' using a default input."
    (interactive)
    (msb//helm-do-ag-region-or-symbol 'msb/helm-files-do-ack))

  (defun msb/helm-files-do-pt (&optional dir)
    "Search in files with `pt'."
    (interactive)
    (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
      (helm-do-ag dir)))

  (defun msb/helm-files-do-pt-region-or-symbol ()
    "Search in files with `pt' using a default input."
    (interactive)
    (msb//helm-do-ag-region-or-symbol 'msb/helm-files-do-pt))

  (defun msb/helm-files-smart-do-search (&optional default-inputp)
    "Search in opened buffers using `search-tools'.
Search for a search tool in the order provided by `search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
    (interactive)
    (call-interactively
     (msb//helm-do-search-find-tool "helm-files-do"
                                    search-tools
                                    default-inputp)))

  (defun msb/helm-files-smart-do-search-region-or-symbol ()
    "Search in opened buffers using `search-tools'.
with default input.
Search for a search tool in the order provided by `search-tools'."
    (interactive)
    (msb/helm-files-smart-do-search t))

  ;; Search in buffers ---------------------------------------------------


  (defun msb/helm-buffers-do-ag (&optional _)
    "Wrapper to execute `helm-ag-buffers.'"
    (interactive)
    (helm-do-ag-buffers))

  (defun msb/helm-buffers-do-ag-region-or-symbol ()
    "Search in opened buffers with `ag' with a default input."
    (interactive)
    (msb//helm-do-ag-region-or-symbol 'msb/helm-buffers-do-ag))

  (defun msb/helm-buffers-do-ack (&optional _)
    "Search in opened buffers with `ack'."
    (interactive)
    (let ((helm-ag-base-command "ack --nocolor --nogroup"))
      (helm-do-ag-buffers)))

  (defun msb/helm-buffers-do-ack-region-or-symbol ()
    "Search in opened buffers with `ack' with a default input."
    (interactive)
    (msb//helm-do-ag-region-or-symbol 'msb/helm-buffers-do-ack))

  (defun msb/helm-buffers-do-pt (&optional _)
    "Search in opened buffers with `pt'."
    (interactive)
    (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
      (helm-do-ag-buffers)))

  (defun msb/helm-buffers-do-pt-region-or-symbol ()
    "Search in opened buffers with `pt' using a default input."
    (interactive)
    (msb//helm-do-ag-region-or-symbol 'msb/helm-buffers-do-pt))

  (defun msb/helm-buffers-smart-do-search (&optional default-inputp)
    "Search in opened buffers using `search-tools'.
Search for a search tool in the order provided by `search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
    (interactive)
    (call-interactively
     (msb//helm-do-search-find-tool "helm-buffers-do"
                                    search-tools
                                    default-inputp)))

  (defun msb/helm-buffers-smart-do-search-region-or-symbol ()
    "Search in opened buffers using `search-tools' with
default input.
Search for a search tool in the order provided by `search-tools'."
    (interactive)
    (msb/helm-buffers-smart-do-search t))

  ;; Search in project ---------------------------------------------------


  (defun msb/helm-project-do-ag ()
    "Search in current project with `ag'."
    (interactive)
    (let ((dir (projectile-project-root)))
      (if dir
          (helm-do-ag dir)
        (message "error: Not in a project."))))

  (defun msb/helm-project-do-ag-region-or-symbol ()
    "Search in current project with `ag' using a default input."
    (interactive)
    (let ((dir (projectile-project-root)))
      (if dir
          (msb//helm-do-ag-region-or-symbol 'helm-do-ag dir)
        (message "error: Not in a project."))))

  (defun msb/helm-project-do-ack ()
    "Search in current project with `ack'."
    (interactive)
    (let ((dir (projectile-project-root)))
      (if dir
          (msb/helm-files-do-ack dir)
        (message "error: Not in a project."))))

  (defun msb/helm-project-do-ack-region-or-symbol ()
    "Search in current project with `ack' using a default input."
    (interactive)
    (let ((dir (projectile-project-root)))
      (if dir
          (msb//helm-do-ag-region-or-symbol
           'msb/helm-files-do-ack dir)
        (message "error: Not in a project."))))

  (defun msb/helm-project-do-pt ()
    "Search in current project with `pt'."
    (interactive)
    (let ((dir (projectile-project-root)))
      (if dir
          (msb/helm-files-do-pt dir)
        (message "error: Not in a project."))))

  (defun msb/helm-project-do-pt-region-or-symbol ()
    "Search in current project with `pt' using a default input."
    (interactive)
    (let ((dir (projectile-project-root)))
      (if dir
          (msb//helm-do-ag-region-or-symbol
           'msb/helm-files-do-pt dir)
        (message "error: Not in a project."))))

  (defun msb/helm-project-smart-do-search (&optional default-inputp)
    "Search in current project using `search-tools'.
Search for a search tool in the order provided by `search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
    (interactive)
    (let ((projectile-require-project-root nil))
      (call-interactively
       (msb//helm-do-search-find-tool "helm-project-do"
                                      search-tools
                                      default-inputp))))

  (defun msb/helm-project-smart-do-search-region-or-symbol ()
    "Search in current project using `search-tools' with
 default input.
Search for a search tool in the order provided by `search-tools'."
    (interactive)
    (msb/helm-project-smart-do-search t))

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

;;; helm.el
