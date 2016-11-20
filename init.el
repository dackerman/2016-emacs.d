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
		("st-dackerman1" . (mac unix work))
		("bottom" . (mac unix home))
		("HULKING-BEAST" . (windows home))))

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
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(show-paren-mode 1)
(setq tab-width 2)
(setq scroll-step 1)

(if-system
 work
 (setq org-agenda-files '("~/Dropbox (Stripe)/work/todo.org")))


(if-system
 linux
 (add-to-list 'default-frame-alist
              '(font . "Droid Sans Mono-12")))
(if-system
 windows
 (add-to-list 'default-frame-alist
              '(font . "Courier New-12")))

;; Don't GC as often, we got memory
(setq gc-cons-threshold 20000000)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package darktooth-theme
  :config
  (load-theme 'darktooth t)
  (if-system
   mac
   (set-frame-font "Inconsolata-12")))

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
  (setq projectile-tags-command "/usr/local/bin/ctags -Re -f \"%s\" %s")

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

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package flycheck
  :init
  (setq flycheck-ruby-rubocop-executable "/Users/dackerman/.rbenv/shims/rubocop")
  (setq flycheck-ruby-executable "/Users/dackerman/.rbenv/shims/ruby")
  ;;(setq flycheck-javascript-eslint-executable "npm lint")
  
  :config  
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)
                        '(ruby-rubylint)
                        '(json-jsonlist)))
  
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (global-flycheck-mode))

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

(use-package yaml-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode)))

(use-package ruby-mode
  :config
  (defun my-ruby-mode-hook ()
    (set-fill-column 80)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)
    (setq ruby-insert-encoding-magic-comment nil))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

(use-package web-mode
  :init
  (defun web-mode-customization ()
    "Customization for web-mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-coloriztion t)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (add-hook 'web-mode-hook 'web-mode-customization)

  :mode ("\\.html?\\'" "\\.erb\\'" "\\.hbs\\'"
         "\\.jsx?\\'" "\\.coffee\\'" "\\.json\\'"
         "\\.s?css\\'" "\\.less\\'" "\\.sass\\'"))

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
  "Find tag in project, picking the first possible."
  (interactive)
  (projectile-visit-project-tags-table)
  ;; Auto-discover the user's preference for tags
  (let ((find-tag-fn (projectile-determine-find-tag-fn)))
    (funcall find-tag-fn (thing-at-point 'symbol))))

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p j") 'find-tag-lucky)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(defun daves-backward-kill-word ()
  "Behaves like normal backward-kill-word, except:
    - Killing while in whitespace only kills the whitespace.
    - Killing while in special chars only kills the special chars."
  (interactive)
  (if (bolp) (backward-delete-char 1)
    (if (string-match "[\]\[()*+\\-]+$"
		      (buffer-substring (point-at-bol) (point)))
	(kill-region (+ (point-at-bol) (match-beginning 0)) (point))
      (if (string-match "[[:blank:]]+$"
			(buffer-substring (point-at-bol) (point)))
	  (kill-region (+ (point-at-bol) (match-beginning 0)) (point))
	(backward-kill-word 1)))))

(global-set-key [C-backspace] 'daves-backward-kill-word)

(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defalias 'insert-today 'insert-current-date)

(defun atlaseng (jiranum)
  (interactive "MATLASENG-")
  (insert (format "https://jira.corp.stripe.com/browse/ATLASENG-%s"
                  jiranum)))

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

