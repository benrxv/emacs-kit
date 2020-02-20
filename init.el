(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package)))


;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Change me to only do this in *scratch* buffer ;)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (save-excursion(animate-string ";; Emacs time!!"
(/ (frame-height) 2))))))

(require 'use-package)

(desktop-save-mode 1)
(global-set-key "\M-[1;5C"    'forward-word)      ; Ctrl+right   => forward word
(global-set-key "\M-[1;5D"    'backward-word)    ; Ctrl+left    => backward word

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))

(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package js2-mode
  :ensure
  :init (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?ƒ) prettify-symbols-alist)))

(use-package color-theme
  :ensure
  :config (progn 
	    (color-theme-initialize)
	    (load-theme 'misterioso)
	    (set-cursor-color "#ffffff")))
	    ;; (load-theme 'solarized-dark))) 
	    
(use-package ibuffer
  :ensure
  :bind ("C-x C-b" . ibuffer)
  :config (add-hook 'ibuffer-hook
		    (lambda ()
		      (ibuffer-vc-set-filter-groups-by-vc-root)
		      (ibuffer-do-sort-by-alphabetic))))

(use-package uniquify
  :config
  (setq
   uniquify-buffer-name-style 'reverse
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t      ; rename after killing dupes
   uniquify-ignore-buffers-re "^\\*"; don't muck with special buffers
   ))

(use-package django-mode
  :ensure)

(use-package pony-mode
  :ensure)

(use-package yaml-mode
  :ensure)

(use-package yasnippet
  :ensure
  :config (yas/global-mode 1))

(use-package elpy
  :ensure
  :init (with-eval-after-load
	    (progn
	      (elpy-enable)))
	  
  ;; Monkey patch to not tell me which function I'm in always
  (defun elpy-eldoc-documentation ()
    "Return a call tip for the python call at point."
    (elpy-rpc-get-calltip
     (lambda (calltip)
       (eldoc-message
        (cond
         ((not calltip)
          (let ((current-defun (python-info-current-defun)))
            (when current-defun
              nil)))
         ((stringp calltip)
          calltip)
         (t
          (let ((name (cdr (assq 'name calltip)))
                (index (cdr (assq 'index calltip)))
                (params (cdr (assq 'params calltip))))
            (when index
              (setf (nth index params)
                    (propertize (nth index params)
                                'face
                                'eldoc-highlight-function-argument)))
            (format "%s(%s)"
                    name
                    (mapconcat #'identity params ", "))))))))
    ;; Return the last message until we're done
    eldoc-last-message)
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
	            python-shell-interpreter-args "-i --simple-prompt"))
  )

(use-package flymake-cursor
  :ensure)

(use-package smooth-scrolling
  :ensure
  :config (setq smooth-scroll-margin 5
                scroll-conservatively 9999
                scroll-preserve-screen-position t))

(use-package tramp
  :ensure)

;; (use-package jade
;;   :ensure)

(use-package beacon
  :ensure
  :config
  (beacon-mode 1)
  (setq beacon-blink-delay 0.2)
  (setq beacon-blink-duration 0.2)
  (setq beacon-blink-when-point-moves 7)
  ;; (setq beacon-blink-when-window-changes nil)
  ;; (setq beacon-blink-when-window-scrolls nil)
  (setq beacon-color "white")
  (setq beacon-push-mark 5)
  (setq beacon-size 25))
