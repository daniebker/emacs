;;;;
;; ORG-MODE
;;;;
 
(require 'org)
(require 'org-habit)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(add-to-list 'org-modules 'org-habit)

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "IN-PROGRESS" "WAITING" | "DONE" "CANCELLED")))

;; Set attachment directory
(setq org-attach-directory "~/zettlekasten/assets/")

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/zettlekasten/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/zettlekasten/tickler.org" "Tickler")
                               "* %i%? \n %U")))
;;;;
;; KEY MAPPINGS
;;;;

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

;;;;
;; REFILE
;;;;

(setq org-refile-targets
      '((nil :maxlevel . 9)
	(org-agenda-files :maxlevel . 8)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;;;
;; AGENDA
;;;;

(setq org-agenda-custom-commands
      '(("w" "WORK" tags-todo "@work"
         ((org-agenda-overriding-header "work")))
        ("l" "LUNCHTIME" tags-todo "LUNCHTIME"
         ((org-agenda-overriding-header "Lunch")))
        ("h" "HOME" tags-todo "HOME"
         ((org-agenda-overriding-header "Home")))))

;; Show {x} days total
;; Starting from today
;; With {x} days look back
(setq org-agenda-span 4
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-1d")
