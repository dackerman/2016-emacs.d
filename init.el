;; Initialize package system
(setq package-enable-at-startup nil)
(setq package-user-dir "~/.emacs.d/packages")
(setq default-directory "~/.emacs.d/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq use-package-always-ensure t)


;; -----------------------------------------------------------------------------
;;				    Settings
;; -----------------------------------------------------------------------------
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(column-number-mode t)
(global-linum-mode 1)

(add-to-list 'default-frame-alist
	     '(font . "Droid Sans Mono-12"))

(load-theme 'darktooth t)


;; Helm & Projectile
(require 'helm-config)
(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-use-git-grep t)
(helm-mode 1)
(setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-projectile-on)

(global-set-key (kbd "C-c m s") 'magit-status)

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

;; Languages

;; web mode --------------------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass" . web-mode))

(defun web-mode-customization ()
  "Customization for web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-coloriztion t)
  )

(add-hook 'web-mode-hook 'web-mode-customization)

;; cscope --------------------------------------------------
(require 'xcscope)

(cscope-setup)
(setq cscope-option-do-not-update-database t)
(setq cscope-option-use-inverted-index t)

(add-to-list 'auto-mode-alist '("\\.el" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))


;; -----------------------------------------------------------------------------
;;			     Programming Languages
;; -----------------------------------------------------------------------------
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package purescript-mode
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package haskell-mode)


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
    (haskell-mode purescript-mode rust-mode use-package xcscope web-mode markdown-mode magit helm-projectile flx darktooth-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
