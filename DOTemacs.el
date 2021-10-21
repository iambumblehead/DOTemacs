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

(require 'ansi-color)

(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))

(defun colorize-compilation-buffer ()
  "Process ANSI color codes in compilation output."
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; appearance/theme
;; (require 'linum)
;; (require 'hlinum)
;; (hlinum-activate)

;(require 'sublimity)
;(require 'sublimity-scroll)
;(require 'sublimity-map)

;; (require 'solaire-mode)
;; (require 'clojure-mode)

;; (defun figwheel-repl ()
;;   (interactive)
;;   (inf-clojure "lein figwheel"))

;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; brighten buffers (that represent real files)
;; (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)

;;(load-theme 'tsdh-dark t)
;;(load-theme 'zenburn t)
(if (display-graphic-p)
    (load-theme 'doom-peacock t)
  ;;  (load-theme 'doom-city-lights t)
  (load-theme 'doom-peacock t)) ;; shell


(when (member "Menlo" (font-family-list))
  (set-face-attribute
   'default nil
   :family "Menlo"
   :height 180))

(when (member "Ubuntu Mono" (font-family-list))
  (set-face-attribute
   'default nil
   :family "Ubuntu Mono"
   :height 180))


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

(setq tramp-default-method "ssh")


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

;; locate the file which exists at path inside root
;; recursively traversing 'up' root until path is found no parent exists
;;
;; useful for mono-repos that locate eslint to a parent project directory
(defun findupward (root path)
  (when (file-directory-p root)
    (let* ((fullroot (expand-file-name root))
           (eslint (and root (expand-file-name path root))))
      (if (or (file-exists-p eslint)
              (string-equal fullroot "/")) eslint
        (findupward (concat root "../../") path)))))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory) "node_modules"))
         (eslint (findupward root "node_modules/eslint/bin/eslint.js")))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)
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

;; allow mouse at emacs -nw
(unless (window-system)
  (xterm-mouse-mode)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1))))

;; allow copy-paste to and from emacs -nw
;; (require 'xclip)
;; (xclip-mode 1)

;; http://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

(autoload 'dirtree "dirtree" "Add directory to tree view" t)



(defun my-copy-to-xclipboard(arg)
  (interactive "P")
  (cond
    ((not (use-region-p))
      (message "Nothing to yank to X-clipboard"))
    ((and (not (display-graphic-p))
         (/= 0 (shell-command-on-region
                 (region-beginning) (region-end) "xsel -i -b")))
      (error "Is program `xsel' installed?"))
    (t
      (when (display-graphic-p)
        (call-interactively 'clipboard-kill-ring-save))
      (message "Yanked region to X-clipboard")
      (when arg
        (kill-region  (region-beginning) (region-end)))
      (deactivate-mark))))

(defun my-cut-to-xclipboard()
  (interactive)
  (my-copy-to-xclipboard t))

(defun my-paste-from-xclipboard()
  "Uses shell command `xsel -o' to paste from x-clipboard. With
one prefix arg, pastes from X-PRIMARY, and with two prefix args,
pastes from X-SECONDARY."
  (interactive)
  (if (display-graphic-p)
    (clipboard-yank)
   (let*
     ((opt (prefix-numeric-value current-prefix-arg))
      (opt (cond
       ((=  1 opt) "b")
       ((=  4 opt) "p")
       ((= 16 opt) "s"))))
    (insert (shell-command-to-string (concat "xsel -o -" opt))))))

(global-set-key (kbd "C-c C-w") 'my-cut-to-xclipboard)
(global-set-key (kbd "C-c M-w") 'my-copy-to-xclipboard)
(global-set-key (kbd "C-c C-y") 'my-paste-from-xclipboard)
