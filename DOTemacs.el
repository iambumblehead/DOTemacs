;; bumblehead's .emacs file
;;
;; C-x C-e to reload this from running emacs instance
;; M-x load-file RET ~/.emacs RET

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

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
(require 'linum)
(require 'hlinum)
(hlinum-activate)

;(require 'sublimity)
;(require 'sublimity-scroll)
;(require 'sublimity-map)

(require 'solaire-mode)

;; brighten buffers (that represent real files)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)

(load-theme 'doom-city-lights t)
;;(load-theme 'tsdh-dark t)
;;(load-theme 'zenburn t)


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


;; highlight the current line; set a custom face
;;(defface hl-line '((t (:background "Gray10")))
;;(defface hl-line '((t (:background "Gray10")))
;;(defface hl-line '((t (:background "red" :bold t)))
(defface hl-line '((t (:background "Gray10")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t)

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

;;(autoload 'json-mode "json-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$"  . gfm-mode))
;;(add-to-list 'auto-mode-alist '("\\.js$"  . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$"  . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode))

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


(defun split-horizontally-for-temp-buffers ()
  "Split the window horizontally for temp buffers."
  (when (one-window-p t) (split-window-horizontally)))

;; steve yegge
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)


(defun string-replace (substr newstr fullstr)
  "replace SUBSTR with NEWSTR' in the string FULLSTR"
  (with-temp-buffer
    (insert fullstr)
    (goto-char (point-min))
    (while (search-forward substr nil t)
      (replace-match newstr nil t))
    (buffer-substring (point-min) (point-max))))


;; backup 
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

;; terminal-like history scroll in emacs shell
(defun bind-shell-history-keys ()
  (global-set-key (kbd "C-p") 'comint-previous-input)
  (global-set-key (kbd "C-n") 'comint-next-input))
(add-hook 'shell-mode-hook 'bind-shell-history-keys)

(setq js-indent-level 4)
(setq sgml-basic-offset 4)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-default-display-inline-images 't)

(unless (window-system) (xterm-mouse-mode))

;; http://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;;(global-set-key (kbd "S-C-b") 'shrink-window-horizontally)
;;(global-set-key (kbd "S-C-n") 'enlarge-window-horizontally)
;;(global-set-key (kbd "S-C-p") 'shrink-window)
;;(global-set-key (kbd "S-C-n") 'enlarge-window)

(autoload 'dirtree "dirtree" "Add directory to tree view" t)

