(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)

(setq-default indent-tabs-mode nil)
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 80)
(global-whitespace-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq kill-whole-line t)
(global-set-key (kbd "<f11>") 'global-linum-mode)
(global-set-key (kbd "M-g M-f") 'first-error)
(column-number-mode 1)
(setq-default fill-column 80)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq ghc-location-prefix "~/")
(setq ghc-location "ghc")

(add-to-list 'exec-path "~/.cabal/bin")


(defun rgrep-ghc (regexp)
  (interactive (list (progn (grep-compute-defaults) (grep-read-regexp))))
  (rgrep regexp "*hs" (concat ghc-location-prefix ghc-location "/compiler/")))

(defun compile-ghc ()
  (interactive)
  (let* ((full-ghc-loc (concat ghc-location-prefix ghc-location))
     (compile-command (if (boundp 'ghc-compile)
                             (concat "cd " full-ghc-loc "; " ghc-compile)
                             (concat "cd "full-ghc-loc "/ghc; make -j3 2"))))
    (compile compile-command)
    (set-buffer "*compilation*")
    (setq default-directory full-ghc-loc)))

(defun set-compile-ghc ()
  (local-set-key (kbd "C-q") 'compile-ghc))


(global-set-key (kbd "M-c") 'rgrep-ghc)

(add-hook 'haskell-mode-hook 'set-compile-ghc)

(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)

(custom-set-variables
 '(cua-mode t nil (cua-base))
 '(show-paren-mode t))

(setq x-select-enable-clipboard t)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(kill-buffer "*scratch*")

(custom-set-faces)
