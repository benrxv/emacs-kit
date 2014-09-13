(package-initialize)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
(use-package magit
  :bind ("C-c g" . magit-status))

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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
              (save-excursion(animate-string ";; Emacs time!!"
(/ (frame-height) 2))))))

(require 'yasnippet)
(yas/global-mode 1)
