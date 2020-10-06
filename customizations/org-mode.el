;;;;
;; ORG-MODE
;;;;
 
(require 'org)
(require 'org-habit)

(add-to-list 'org-modules 'org-habit)
(add-hook 'org-mode-hook #'visual-line-mode)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Set attachment directory
(setq org-attach-directory "~/zettlekasten/assets/")

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(load-file "~/.emacs.d/customizations/org-mode/archive.el")
 

;;;;;;;;
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
        (org-agenda-files :maxlevel . 9)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;;;
;; AGENDA
;;;;

(setq org-agenda-custom-commands
      '(
	("n" "Next Actions"
	 ((tags "project"
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "Next Actions:")))))
	("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("TODO" "DONE" "CANCELLED")))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))

;;;;
;; Add ids to Headings
;;;;

;; Functions are provided by: https://writequit.org/articles/emacs-org-mode-generate-ids.html

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(defun eos/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun eos/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
      (org-map-entries (lambda () (eos/org-custom-id-get (point) 'create))))))

(defun org-id-new (&optional prefix)
  "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.

So a typical ID could look like \"Org-4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
                     ""
                   (concat (or prefix org-id-prefix) "-")))
         unique)
    (if (equal prefix "-") (setq prefix ""))
    (cond
     ((memq org-id-method '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
        (setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-reverse-string (org-id-time-to-b36)))
             (postfix (if org-id-include-domain
                          (progn
                            (require 'message)
                            (concat "@" (message-make-fqdn))))))
        (setq unique (concat etime postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix unique)))


;; automatically add ids to saved org-mode headlines
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (and (eq major-mode 'org-mode)
                                   (eq buffer-read-only nil))
                          (eos/org-add-ids-to-headlines-in-file))))))

;;;;;
;; Tag Alignment
;;;;;
;; From: https://stackoverflow.com/questions/6210840/tag-position-in-org-mode

(add-hook 'focus-in-hook 
  (lambda () (progn 
    (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))

(add-hook 'focus-out-hook 
  (lambda () (progn 
    (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))

;;;;;
;; ORG-ROAM
;;;;;
(require 'org-roam)
(require 'org-roam-protocol)
(setq org-roam-directory "~/org-roam")
(add-hook 'after-init-hook 'org-roam-mode)

(cond ((string-equal system-type "windows-nt")
       (setq org-roam-graph-viewer "C:/Program Files/Google/Chrome/Application/chrome.exe"))

      (load-file "~/.emacs.d/customizations/org-mode/org-protocol-check-filename-for-protocol.el")
      (advice-add 'org-protocol-check-filename-for-protocol :override '+org-protocol-check-filename-for-protocol)

      (require 'org-roam-server)
      (setq org-roam-server-host "127.0.0.1"
	    org-roam-server-port 8118
	    org-roam-server-export-inline-images t
	    org-roam-server-authenticate nil
	    org-roam-server-network-poll t
	    org-roam-server-network-arrows nil
	    org-roam-server-network-label-truncate t
	    org-roam-server-network-label-truncate-length 60
	    org-roam-server-network-label-wrap-length 20))

;;;;;
;; ORG-ROAM BIBTEX
;;;;;

;; Org-ref
;; Set up bibliography
(setq org-ref-default-bibliography '("~/org-roam/My Library.bib"))
(setq bibtex-completion-bibliography "~/org-roam/My Library.bib")
(global-set-key (kbd "<f6>") #'org-ref-helm-insert-cite-link)

;; Org-roam-bibtex
(require `org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)
(define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)

(setq org-roam-capture-templates
      '(("d" "default" plain
         (function org-roam-capture--get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+ROAM_TAGS:\n\n"
         :unnarrowed t)
        ))

;; COMPANY

(require 'company-org-roam)
(push 'company-org-roam company-backends)

(smartparens-global-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.25)
(setq company-backends '(company-capf))
;; This enables candidates matching to be case-insensitive
(setq completion-ignore-case t)
(setq org-roam-link-auto-replace nil)

;;;;;
;; ORG-ROAM Keybindings
;;;;;

(global-set-key (kbd "C-c r r") 'org-roam)
(global-set-key (kbd "C-c r f") 'org-roam-find-file)
(global-set-key (kbd "C-c r g") 'org-roam-graph)
(global-set-key (kbd "C-c r i") 'org-roam-insert)
(global-set-key (kbd "C-c r S") 'org-roam-server-mode)
