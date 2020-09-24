 ;;;;
 ;; ORG-MODE
 ;;;;
 
(require 'org)
(require 'org-habit)

(add-to-list 'org-modules 'org-habit)

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "IN-PROGRESS" "WAITING" "DONE")))


;;;;
;; KEY MAPPINGS
;;;;

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

(setq org-refile-targets
      '((nil :maxlevel . 9)
        (org-agenda-files :maxlevel . 9)))


(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;;;
;; AGENDA
;;;;


;; Show {x} days total
;; Starting from today
;; With {x} days look back
(setq org-agenda-span 4
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-1d")
