;;; init.el

;; Copyright (C) 2018 Matthew Boston
;; Created: October 24, 2017
;; Homepage: https://github.com/bostonaholic/dotemacs

;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))

  (require 'package)
  (setq-default load-prefer-newer t
                package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (package-initialize)

  ;; Install dependencies
  (unless (and (package-installed-p 'delight)
               (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'delight t)
    (package-install 'use-package t))
  (setq-default use-package-always-defer t
                use-package-always-ensure t)

  ;; Use latest Org
  (use-package org
    :ensure org-plus-contrib
    :pin org)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
  (garbage-collect))

;;; init.el ends here
