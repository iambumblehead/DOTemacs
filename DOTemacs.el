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
(load-theme 'tsdh-dark t)
;;(set-default-font "Liberation Mono-12")
(set-default-font "UbuntuMono-16")

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
(setq tab-width 4)
(setq-default truncate-lines t)

(line-number-mode   1)
(column-number-mode 1)
(show-paren-mode    1)

(setq tramp-default-method "ssh")

;;(autoload 'json-mode "json-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$"  . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.js$"  . js2-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode))

(defun markdown-mode-hook ()
  (gfm-mode)
  (visual-line-mode 1))

(add-hook 'markdown-mode-hook 'markdown-mode-hook)

;; Use visual-line-mode in gfm-mode
(defun gfm-mode-hook ()
  (visual-line-mode 1))

(add-hook 'gfm-mode-hook 'gfm-mode-hook)


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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 ;;'(js2-bounce-indent-p t)
 '(js2-mirror-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

;; terminal-like history scroll in emacs shell
(defun bind-shell-history-keys ()
  (global-set-key (kbd "C-p") 'comint-previous-input)
  (global-set-key (kbd "C-n") 'comint-next-input))
(add-hook 'shell-mode-hook 'bind-shell-history-keys)

(setq js-indent-level 2)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-default-display-inline-images 't)

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

