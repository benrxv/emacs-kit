(package-initialize)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

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


(use-package magit
  :bind ("C-c g" . magit-status)
  :init (add-to-list 'load-path "~/.emacs.d/magit-1.2.0"))

(use-package js2-mode
  :ensure
  :init (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(use-package color-theme
  :ensure
  :config (progn 
	    (color-theme-initialize)
	    (load-theme 'misterioso))) 
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

(use-package yasnippet
  :ensure
  :config (yas/global-mode 1))

(use-package elpy
  :ensure
  :config (elpy-enable))

(use-package flymake-cursor
  :ensure)
