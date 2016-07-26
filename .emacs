;;;
;;; PACKAGE SYSTEM (melpa)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;;
;;; add a default load-path
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))


;;;
;;; LOAD PATH

(setenv "PATH" "~/bin:/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/opt/X11/bin")

;;;
;;; NO SPLASH

(setq inhibit-startup-message t)

;;;
;;; HELM

(helm-mode t)

;;;
;;; MARK REGION BACKGROUND COLOR

(set-face-attribute 'region nil :background "#666")

;;;
;;; HELM PROJECTILE

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;;
;;; disable ctrl z to hide an emacs app in mac os x

(global-unset-key (kbd "C-z"))

;;;
;;; VERTICAL SPLITTING

(setq split-height-threshold 999)

;;;
;;; BUFFER MOVE

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;;
;;; NO BACKUP

(setq backup-inhibited t)

;;;
;;; NO AUTOSAVE

(setq auto-save-default nil)

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
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

;;;
;;; GNU GLOBAL WITH HELM

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)

(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-prefix-key "C-t")
 '(helm-gtags-suggested-key-mapping t))

(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

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

;;(require 'member-functions)

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
;;; ROBOT MODE
;; (load-file "~/.emacs.d/site-lisp/robot-mode.el")
;; (setq auto-mode-alist (append '(("\.robot$" . robot-mode)) auto-mode-alist))

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
    (setq auto-mode-alist (append '(("\.go$" . go-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.i$" . c++-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.l$" . c++-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.y$" . c++-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.sc$" . c++-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.re$" . c-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.ly$" . c-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.msg$" . c-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.robot$" . robot-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.conf$" . conf-unix-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.sql$" . sql-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.sh$" . shell-script-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.jsp$" . java-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.java$" . java-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.py$" . python-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.go$" . go-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.js$" . js-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\.clj$" . clojure-mode)) auto-mode-alist))
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

(setq grep-command "grep -nrHI --exclude={.git,.svn,Makefile*} --exclude=*.{am,Po,Plo,la,lo,lai,json,tag} -e ")
(setq grep-highlight-matches t)

;;;
;;; WINDOW SYSTEM
(set-default-font "D2Coding 14")

;;
;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-chunksize 500)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;;;
;;; THEME

(load-theme 'cyberpunk t)
