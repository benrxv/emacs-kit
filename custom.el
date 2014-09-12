(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(require 'use-package)

(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))


(add-to-list 'load-path "~/.emacs.d/elpa/bookmark+-20140829.325")
(require 'bookmark+)

(desktop-save-mode 1)

(global-set-key "\M-[1;5C"    'forward-word)      ; Ctrl+right   => forward word
(global-set-key "\M-[1;5D"    'backward-word)    ; Ctrl+left    => backward word

(add-to-list 'load-path "~/.emacs.d/magit-1.2.0")
(require 'magit)

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(desktop-path (quote ("./")))
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'color-theme)
(color-theme-initialize)
(load-theme 'solarized-dark)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; Group the ibuffer list by vc-root
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (ibuffer-do-sort-by-alphabetic)))


(use-package uniquify
             :config
             (setq
              uniquify-buffer-name-style 'reverse
              uniquify-separator ":"
              uniquify-after-kill-buffer-p t      ; rename after killing dupes
              uniquify-ignore-buffers-re "^\\*"; don't muck with special buffers
              ))

(require 'django-mode)
(require 'pony-mode)

;; Change me to only do this in *scratch* buffer ;)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string ";; Emacs time!!"
(/ (frame-height) 2)))))
