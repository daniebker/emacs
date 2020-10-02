
(require 'package)

;; Add melpa package source when using package list
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Load emacs packages and activate them
;; This must come before configurations of installed packages.
;; Don't delete this line.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(
    ;; Makes writing lisp much easier.
    paredit
    
    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    markdown-mode

    prettier-js

    neotree

    ;; Sidebars!
    org-sidebar

    ;; Better window switching
    ace-window
    
    ;; auto complete 
    ac-helm

    ;; org mode roam research tooling
    org-roam

    org-roam-server
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "misc.el")
(load "navigation.el")
(load "ui.el")
;; Custom Languages
(load "setup-js.el")
(load "org-mode.el")
(load "org-roam.el")
(load "editor.el")
(load "elisp-editing.el")
;; Set ace window to override other window
(global-set-key (kbd "C-x o") 'ace-window)
(setq org-agenda-files '("~/zettlekasten"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("698d072bc75860ae449ac55c138e9a0d0e125c3cb58149238470e598ab9fae0d" "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" default))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(paredit ace-window org-sidebar prettier-js markdown-mode quelpa ac-helm neotree magit tagedit rainbow-delimiters projectile smex rjsx-mode dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
