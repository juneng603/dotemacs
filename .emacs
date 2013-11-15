;;;
;;; add a default load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;;
;;; NO SPLASH

(setq inhibit-startup-message t)

;;;
;;; NO BACKUP
(setq make-backup-files nil)

;;;
;;; FRAME SIZE
(setq initial-frame-alist '((width . 130) (height . 80)))
;;;(setq default-frame-alist '((width . 50) (height . 20)))

;;;
;;; TAB

;;(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100))
(setq-default indent-tabs-mode nil)

;;;
;;; MOVE FRAME
(windmove-default-keybindings 'meta)

;;;
;;; EDITING

(setq kill-whole-line t)
(normal-erase-is-backspace-mode 1)
;;(delete-selection-mode t)

;;;
;;; JSON MODE

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;
;;; LINE WRAP

(setq truncate-partial-width-windows nil)

;;;
;;; MAC

(when (string-equal "mac" window-system)
  (setq osx-key-mode nil)
  (setq mac-command-key-is-meta nil)
  (setq mac-option-modifier 'meta)
  (setq mac-allow-anti-aliasing nil)
  )

(setq mac-command-modifier 'meta)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-line)

;;(setq mac-command-modifier 'alt)
(windmove-default-keybindings 'alt)

;;;
;;; DISPLAY

;;(setq font-lock-maximum-decoration t)
(setq-default show-trailing-whitespace t)

;;(global-font-lock-mode 1 t)
(transient-mark-mode t)
(show-paren-mode t)

(column-number-mode t)

(when (> emacs-major-version 20)
  (tool-bar-mode nil)
  )

(unless (string-equal "mac" window-system)
  (menu-bar-mode nil)
  )

;;;
;;; GNU GLOBAL

;;(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (gtags-mode 1)))
(add-hook 'gtags-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'gtags-find-tag)
            (local-set-key (kbd "M-,") 'gtags-find-rtag)))

(setq load-path (cons "/usr/local/bin/gtags" load-path))
(autoload 'gtags-mode "gtags" "" t)

;;;
;;; SCALA
(add-to-list 'load-path "/opt/scala-mode2/")
(require 'scala-mode)

;;;
;;; JSP
(defun jsp-mode () (interactive)
  (multi-mode 1
              'html-mode
              '("<%--" indented-text-mode)
              '("<%@" indented-text-mode)
              '("<%=" html-mode)
              '("<%" java-mode)
              '("%>" html-mode)
              '("<script" java-mode)
              '("</script" html-mode)
              )
  )

;;;
;;; GLOBAL KEY MAP

(define-key global-map "\C-m" 'newline-and-indent)
(define-key global-map "\C-c\C-c" 'comment-or-uncomment-region)
(define-key global-map [f4] 'compare-windows)
(define-key global-map [f5] 'grep)
(define-key global-map [f6] 'compile)
(define-key global-map [f7] 'previous-error)
(define-key global-map [f8] 'next-error)

(when (< emacs-major-version 22)
  (define-key global-map "\M-g\M-g" 'goto-line)
  )

;;;
;;; ISEARCH MODE HOOK

(defun my-isearch-toggle-word ()
  "Toggle word searching on or off."
  (interactive)
  (setq isearch-word (not isearch-word))
  (setq isearch-success t isearch-adjusted t)
  ;; Work-around for isearch lazy highlight routine does not check word mode change
  (setq isearch-lazy-highlight-last-string "")
  (isearch-update)
  )

(defun my-isearch-mode-hook ()
  (define-key isearch-mode-map "\M-w" 'my-isearch-toggle-word)
  )
(add-hook 'isearch-mode-hook 'my-isearch-mode-hook)

;;;
;;; SHELL MODE HOOK

(defun my-shell-mode-hook ()
  (ansi-color-for-comint-mode-on)
  (setq show-trailing-whitespace nil)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-input-ignoredups t)
  )
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;;;
;;; TEXT MODE HOOK

(defun my-text-mode-hook ()
  (define-key text-mode-map "\t" 'tab-to-tab-stop)
  )
(add-hook 'text-mode-hook 'my-text-mode-hook)

;;;
;;; C MODE HOOK

(defun my-c-mode-common-hook ()
  (c-set-style "stroustrup")
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open '+)
  (c-set-offset 'inline-open 0)
  (set-variable 'c-backslash-max-column 79)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (define-key c-mode-base-map "\C-c\C-c" 'comment-or-uncomment-region)
  (gtags-mode 1)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;
;;; AUTO MODE

(setq auto-mode-alist (append '(("\.outline$" . outline-mode)) auto-mode-alist))

(cond
 ((string-match "darwin" system-configuration)
  (progn
    (setq auto-mode-alist (append '(("\.h$" . c++-mode)) auto-mode-alist))
    ))
 (t
  (progn
    (setq auto-mode-alist (append '(("\.i$" . c++-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.l$" . c++-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.y$" . c++-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.sc$" . c++-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.re$" . c-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.ly$" . c-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.msg$" . c-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.conf$" . conf-unix-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.sql$" . sql-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.sh$" . shell-script-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.jsp$" . java-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.java$" . android-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.go$" . go-mode)) auto-mode-alist))
    ))
 )

;;;
;;; DISABLE RING BELL

(defun my-dummy-ring-bell-function () nil)
(setq ring-bell-function 'my-dummy-ring-bell-function)

;;;
;;; HANGUL
;;(setq default-korean-keyboard "3fd")
(set-language-environment 'korean)

;;;
;;; CODING SYSTEM

(set-default-coding-systems 'euc-kr-unix)
(set-buffer-file-coding-system 'euc-kr-unix)
(set-clipboard-coding-system 'x-ctext)
;;(set-selection-coding-system 'euc-kr-unix)

(set-keyboard-coding-system 'euc-kr-unix)

(when (string-match "darwin" system-configuration)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  ;;(require 'mac-utf)
  )

(when (string-match "junyoung" system-name)
  (set-file-name-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  )

;;;
;;; GREP

(setq-default grep-command "grep -nrHI -e ")
(setq grep-highlight-matches t)

;;;
;;; WINDOW SYSTEM

(when window-system
  ;;(mwheel-install)
  (cond
   ((string-equal "mac" window-system)
    (progn
     (add-to-list 'default-frame-alist '(font . "-apple-monaco-medium-r-normal--10-100-72-72-m-100-iso10646-1"))
      (add-to-list 'default-frame-alist '(width . 120))
      (add-to-list 'default-frame-alist '(height . 60))
      ))
   ((string-equal "w32" window-system)
    (progn
      (create-fontset-from-fontset-spec
       "-outline-Monaco-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1,korean-ksc5601:-outline-µ¸¿òÃ¼-normal-r-normal-normal-12-90-96-96-c-*-ksc5601.1987*-*")
      (add-to-list 'default-frame-alist '(font . "-outline-Monaco-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1"))
      ))
   )
  )

;;
;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")
;;(add-to-list tramp-default-proxy-alist ("srch14" nil "/ssh:junyoung@spb-ws2-srch14"))
;; /sudo:srch14:path
