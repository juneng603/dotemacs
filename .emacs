;;;
;;; PACKAGE SYSTEM (melpa)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize))
(dolist (sources '(("melpa" . "http://melpa.milkbox.net/packages/")
                   ("marmalade" . "http://marmalade-repo.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives sources t))

;;;
;;; add a default load-path
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;;;
;;; LOAD PATH

(setenv "PATH" "~/bin:/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/opt/X11/bin")

;;;
;;; NO SPLASH

(setq inhibit-startup-message t)

;;;
;;; VERTICAL SPLITTING

(setq split-height-threshold 999)
;; (setq split-width-threshold 0)

;;;
;;; BUFFER MOVE

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;;
;;; NO BACKUP
(setq make-backup-files nil)

;;;
;;; HIDE HIDDEN FILES

(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))


;;;
;;; REGION TEXT MOVE
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [\M-\S-up] 'move-text-up)
(global-set-key [\M-\S-down] 'move-text-down)

;;;
;;; MAGIT
(setq magit-diff-options '("-b"))

;;;
;;; GNU GLOBAL

(setq load-path (cons "/usr/local/bin/gtags" load-path))
(autoload 'gtags-mode "gtags" "" t)

(add-hook 'gtags-mode-hook
          (lambda ()
            (local-set-key (kbd "M-*") 'gtags-pop-stack)
            (local-set-key (kbd "M-.") 'gtags-find-tag)
            (local-set-key (kbd "M-p") 'gtags-find-pattern)
            (local-set-key (kbd "M-s") 'gtags-find-symbol)
            (local-set-key (kbd "M-,") 'gtags-find-rtag)))


;;;
;;; GDB-DEBUG

(global-set-key [f9] 'gud-break)
(global-set-key [f10] 'gud-next)
(global-set-key [f11] 'gud-step)
(global-set-key [(shift f11)] 'gud-finish)
(global-set-key [(shift f10)] '(lambda ()
                                 (interactive)
                                 (call-interactively 'gud-tbreak)
                                 (call-interactively 'gud-cont)))

;;;
;;; FRAME SIZE
(setq initial-frame-alist '((width . 130) (height . 80)))
;;;(setq default-frame-alist '((width . 50) (height . 20)))


;;;
;;; GIT
(require 'git)

;;;
;;; C++ MEMBER FUNCTION
(require 'member-functions)

;;;
;;; TAB

(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100))
(setq-default indent-tabs-mode nil)

;;;
;;; IGNORE NAMESPACE INDENTATION
;; Basic indentation
(setq c-basic-offset 4)

(defun my-c-setup ()
  (c-set-offset 'innamespace 0))
(add-hook 'c++-mode-hook 'my-c-setup)
;; (defconst my-cc-style
;;   '("cc-mode"
;;     (c-offsets-alist . ((innamespace . [0])))))

;; (c-add-style "my-cc-mode" my-cc-style)

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

;;; AUTO SAVE
(setq auto-save-interval 5
      auto-save-timeout 5)

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
;;; SCALA
;;;(add-to-list 'load-path "/opt/scala-mode2/")
;;;(require 'scala-mode)

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
    (setq auto-mode-alist (append '(("\.java$" . java-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.py$" . python-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.go$" . go-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.js$" . javascript-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.sh$" . sh-mode)) auto-mode-alist))
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

;;(setq-default grep-command "grep -nrHI -e ")
(setq grep-command "grep -nrHI --exclude='.svn' -e ")
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

;;;
;;; uniq-lines()
