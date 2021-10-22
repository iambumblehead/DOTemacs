;; bumblehead's .emacs file
;;
;; C-x C-e to reload this from running emacs instance
;; M-x load-file RET ~/.emacs RET


;; backup
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . "~/.emacs.d/saves/")))
(setq create-lockfiles nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
             ;;'("melpa" . "http://melpa.milkbox.net/packages/") t)


;;(load-theme 'tsdh-dark t)
;;(load-theme 'zenburn t)
(if (display-graphic-p)
    (load-theme 'doom-peacock t)
  ;;  (load-theme 'doom-city-lights t)
  (load-theme 'doom-peacock t)) ;; shell

(setq hl-line-face 'hl-line)
(global-hl-line-mode t)

(set-face-background 'hl-line "#1c1c1c")

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))

;; indent with spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default truncate-lines t)

(line-number-mode   1)
(column-number-mode 1)
(show-paren-mode    1)


(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.m?js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))
(add-hook 'typescript-mode-hook 'eglot-ensure)

(defun markdown-mode-hook ()
  ;;(gfm-mode)
  (visual-line-mode 1))

(add-hook 'markdown-mode-hook 'markdown-mode-hook)

;;(load-file "./DOTemacs-eslint-local.el")
;;(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'eglot-ensure)
(add-hook 'js2-mode-hook 'company-mode)

(defun split-horizontally-for-temp-buffers ()
  "Split the window horizontally for temp buffers."
  (when (one-window-p t) (split-window-horizontally)))

;; steve yegge
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

;; terminal-like history scroll in emacs shell
(defun bind-shell-history-keys ()
  (global-set-key (kbd "C-p") 'comint-previous-input)
  (global-set-key (kbd "C-n") 'comint-next-input))
(add-hook 'shell-mode-hook 'bind-shell-history-keys)

;; javascript and jsx indent
(setq js-indent-level 2)
(setq sgml-basic-offset 2)

(autoload 'dirtree "dirtree" "Add directory to tree view" t)
