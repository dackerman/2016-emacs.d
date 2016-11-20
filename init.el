;; Initialize package system
(setq package-enable-at-startup nil)
(setq package-user-dir "~/.emacs.d/packages")
(setq default-directory "~/.emacs.d/")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(setq systems '(("recursion" . (linux unix home))
		("st-dackerman1.local" . (mac unix work))
		("bottom" . (mac unix home))
		("?" . (windows home))))

(defun get-system ()
  (cdr (assoc system-name systems)))

(defun system-is (prop)
  (member prop (get-system)))

(defmacro if-system (prop &rest body)
    (when (system-is prop)
      `(progn ,@body)))

;; -----------------------------------------------------------------------------
;;				    Settings
;; -----------------------------------------------------------------------------
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(global-linum-mode 1)

(setq-default indent-tabs-mode nil) ; tabs to spaces
(set-auto-saves)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(show-paren-mode 1)
(setq tab-width 2)
(setq scroll-step 1)

(add-to-list 'default-frame-alist
	     '(font . "Droid Sans Mono-12"))

(use-package exec-path-from-shell
  :if (system-is 'mac)
  :config
  (exec-path-from-shell-initialize))

(use-package darktooth-theme
  :config
  (load-theme 'darktooth t))

(use-package paredit
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode))

(use-package projectile
  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-use-git-grep t)
  (setq helm-projectile-fuzzy-match nil)
  
  :config
  (projectile-global-mode))

(use-package helm
  :bind (("M-x" . helm-M-x))
  :config
  (require 'helm-config)
  (helm-mode 1))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package magit
  :defer t
  :bind (("C-c m s" . magit-status)))

(use-package company
  :defer t
  :config (global-company-mode))

;; Emacs backups
;; * Don't clobber symlinks
;; * Put them in one folder
;; * use versioned backups
(setq
   backup-by-copying t
   backup-directory-alist '(("." . "~/.emacs-backup-files"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;(add-to-list 'backup-directory-alist
;             (cons tramp-file-name-regexp nil))

;; -----------------------------------------------------------------------------
;;			     Programming Languages
;; -----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.el" . emacs-lisp-mode))

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'")

(use-package purescript-mode
  :defer t
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package haskell-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode)))

(use-package web-mode
  :defer t
  :init
  (defun web-mode-customization ()
    "Customization for web-mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-coloriztion t))
  (add-hook 'web-mode-hook 'web-mode-customization)
  
  :config
  (add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.less" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.sass" . web-mode)))

;;(if-system linux
;; (require 'xcscope)
;;
;; (cscope-setup)
;; (setq cscope-option-do-not-update-database t)
;; (setq cscope-option-use-inverted-index t))

;; -----------------------------------------------------------------------------
;;				Custom Functions
;; -----------------------------------------------------------------------------
(defun find-tag-lucky ()
  "Find tag in project, pick first option."
  (interactive)
  (projectile-visit-project-tags-table)
  ;; Auto-discover the user's preference for tags
  (let ((find-tag-fn (projectile-determine-find-tag-fn)))
    (funcall find-tag-fn (thing-at-point 'word))))

(global-set-key (kbd "M-.") 'find-tag-lucky)


;; -----------------------------------------------------------------------------
;;				     Custom
;; -----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell web-mode markdown-mode haskell-mode purescript-mode rust-mode company magit helm-projectile helm projectile paredit darktooth-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

