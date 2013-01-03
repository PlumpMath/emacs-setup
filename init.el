(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Adds all the sub-directories in /elpa/ to the load path

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/custom/")

;;;; Settings specific to Windows

;; Files deleted via Emacs get moved to trash bin
(setq delete-by-moving-to-trash t)
;; Need this to open files into a running Emacs client
(server-start)

;; Setting default font
(set-default-font "Consolas-10")

(setq cua-enable-cua-keys nil) ;; only for rectangles
(cua-mode t)

(require 'pastebin)

(require 'four-clj)

;; Trying a new theme
(load-theme 'wombat t)

(global-linum-mode 1)
(setq linum-format "%3d")

(desktop-save-mode t)

;; Enables using arrow-keys to fetch latest commands in Python shell
(require 'comint)
;; (define-key comint-mode-map (kbd "M-") 'comint-next-input)
;; (define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

;; Auth stuff
(require 'auth-stuff)

;; Org-mode
(require 'my-org-init)

;; Org-protocol
(server-start)
(require 'org-protocol)

;; Remember mode, works well with org-mode.
(require 'remember)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/elpa/yasnippet-0.8.0/snippets")

(require 'auto-complete)
;; (require 'auto-complete-python)
(global-auto-complete-mode t)

(require 'auto-complete-config)
(ac-config-default)

;; ;; Enables rcirc minor-mode only when rcirc is running
;; (add-hook 'rcirc-mode-hook
;;                (lambda ()
;;                  (rcirc-track-minor-mode 1)))

(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
      (lambda ()
        (add-to-list 'ac-sources 'ac-source-ropemacs)))

;; Uses winnermode to switch buffers
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; auto-complete with nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrel-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;; stop nREPL from spewing off on errors
(setq nrepl-popup-stacktraces nil)

;; Pymacs stuff
(add-to-list 'load-path "~/.emacs.d/pymacs/")

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
(setq mouse-wheel-progressive-speed  nil)

(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-full-name
                      channels))))

;; (eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; If you plan to use a special directory to hold your own Pymacs code
;; in Python, which should be searched prior to the usual Python
;; import search path, then uncomment the last two lines (by removing
;; the semi-colons) and replace YOUR-PYMACS-DIRECTORY by the name of
;; your special directory.

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
          (lambda ()
	    (add-to-list 'ac-sources 'ac-source-ropemacs)))


;; ===== PyFlakes
;; code checking via pyflakes+flymake
(when (load "flymake" t)
 (defun flymake-pyflakes-init ()
 (let* ((temp-file (flymake-init-create-temp-buffer-copy
 'flymake-create-temp-inplace))
 (local-file (file-relative-name
 temp-file
 (file-name-directory buffer-file-name))))
 (list "pyflakes" (list local-file))))
 
 (add-to-list 'flymake-allowed-file-name-masks
 '("\\.py\\'" flymake-pyflakes-init)))
 
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Python-mode

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
(add-hook 'python-mode-hook
      (lambda ()
	(set-variable 'py-indent-offset 4)
	;(set-variable 'py-smart-indentation nil)
	(set-variable 'indent-tabs-mode nil)
	(define-key py-mode-map (kbd "RET") 'newline-and-indent)
	;(define-key py-mode-map [tab] 'yas/expand)
	;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
	(smart-operator-mode-on)
        (ropemacs-mode 1)
	))

(define-key python-mode-map (kbd "C-c }") 'py-execute-def-python)
(define-key python-mode-map (kbd "C-c a") 'py-beginning-of-def-or-class)
(define-key python-mode-map (kbd "C-c s") 'py-end-of-def-or-class)

(global-set-key (kbd "C-x c") 'delete-region)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-c C-j") 'nrepl-jack-in)


(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; some guy on #rcirc said this would help with the buffer growing
;; too large, causing emacs to start lagging
(defun youngfrog/rcirc-truncate-buffer (&optional lines)
  "Truncate a buffer to display only the last LINES lines.
If LINES is missing and ´rcirc-buffer-maximum-lines´ is non-nil,
then its value is used. If both are nil, use 200."
  (interactive "P")
  (let ((rcirc-buffer-maximum-lines (or lines rcirc-buffer-maximum-lines 200))
        (inhibit-read-only t))
    (save-excursion
      (when (= (forward-line (- rcirc-buffer-maximum-lines)) 0)
        (delete-region (point-min) (point))))))

;; My font/face customizations
(set-face-background 'fringe "gray16")
(set-face-background 'highlight "#454545")
(set-face-foreground 'highlight nil)
(set-face-underline 'highlight nil)
(set-face-foreground 'org-hide "gray14")
(set-face-foreground 'outline-2 "deep sky blue")
(set-face-foreground 'outline-3 "deep sky blue")
(set-face-foreground 'outline-4 "gold")
(set-face-foreground 'outline-5 "spring green")
(set-face-foreground 'outline-6 "orchid")

(setq fci-rule-color "#383838")

;; For scrolling
(setq next-screen-context-lines 28)
(setq scroll-conservatively 5)


;; Settings for pop-win mode
(setq popwin:special-display-config '(("*Help*")
                                      ("*nREPL error*" :noselect t)
                                      ("*nREPL doc*" :noselect t)
                                      ("*Completions*" :noselect t)
                                      ("*compilation*" :noselect t)
                                      ("*Occur*" :noselect t)))

;; Makes it so that any variables customized from the customization
;; menu get saved up in a different file to not clutter up the init.el
;; file 
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

