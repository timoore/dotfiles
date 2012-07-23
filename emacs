;;; Tim Moore's .emacs file.
(defconst running-emacs19 (if (string-match "Emacs 19" (emacs-version))
			      t
			    nil))
(defconst running-lucid (if (string-match "Lucid" (emacs-version)) t  nil))
(defconst running-emacs19-fsf (and running-emacs19 (not running-lucid)))
(defconst running-emacs19-20 (or running-emacs19
				 (if (string-match "Emacs 2[01]"
						   (emacs-version))
				     t
				   nil)))

(require 'compile)
(if running-lucid
    (require 'highlight-headers))

(put 'eval-expression 'disabled nil)

;;; I like this behavior much better
(setq dabbrev-case-replace nil)

;;; Customize the load path for my own functions.
(setq load-path (append '("~/gnu/ecb-2.40" "/usr/share/maxima/5.27.0/emacs"
			  "~/gnu" "~/gnu/egg" "~/gnu/imaxima-imath-1.0")
			load-path))

(setq multi-line-comment t)
(setq comment-multi-line t)
(setq-default indent-tabs-mode nil)

(defun common-c-mode-hook ()
  (auto-fill-mode 1)
  (local-set-key "\r" 'newline-and-indent))

;;; This makes much more sense and agrees better with tools like git diff.
(setq c-offsets-alist '((namespace-open . 0)
                        (namespace-close . 0)
                        (innamespace . 0)))

;;; Stroustrup, with the namespace changes above and different inline open

(c-add-style "PERSONAL-C++"
             '("stroustrup"
               (c-offsets-alist
                (inline-open . 0))))

(c-add-style "my-gnu"
             '("gnu"
               (c-offsets-alist
                (innamespace . 0))))

(c-add-style "my-linux"
             '("linux"
               (indent-tabs-mode t)))

(add-hook 'c-mode-hook
          '(lambda ()
             (common-c-mode-hook)
             (if (and buffer-file-name
                      (string-match "systemtap/" buffer-file-name))
                 (c-set-style "my-linux" 'dont-override)
               (c-set-style "gnu" 'dont-override))))

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (common-c-mode-hook)
             (if (and buffer-file-name
                      (string-match "systemtap/" buffer-file-name))
                 (c-set-style "my-gnu" 'dont-override)
               (c-set-style "PERSONAL-C++" 'dont-override))))

(add-hook 'java-mode-hook
	  '(lambda ()
	     (common-c-mode-hook)
	     (c-set-style "java")))

;;; We hates it!
(setq parens-require-spaces nil)

(setq add-log-mailing-address "timoore33@gmail.com")

(add-hook 'change-log-mode-hook
	  '(lambda ()
	     (auto-fill-mode 1)))

;(add-to-list 'vc-handled-backends 'GIT)

;;; Get the actual cpp command invoked by gcc.

(defun gcc-cppcommand ()
  (let ((outbuf (get-buffer-create " *cpp command*"))
	(temp-file-name (concat (make-temp-name "/tmp/emacs") ".c")))
    ;; A file for gcc to chew on.
    (make-symbolic-link "/dev/null" temp-file-name)
    (unwind-protect
	(save-excursion
	  (set-buffer outbuf)
	  (erase-buffer)
	  ;; -v output goes to stderr.
	  (call-process "sh" nil t t "-c" (concat "gcc -v -E "
						  temp-file-name
						  " > /dev/null"))
	  (goto-char (point-min))
	  (re-search-forward "/.*cpp " nil)
	  (let ((start (match-beginning 0)))
	    (search-forward "-v ")
	    (replace-match "")
	    (search-forward temp-file-name)
	    (concat (buffer-substring start (match-beginning 0))
		    "-C")))
      (delete-file temp-file-name)
      (kill-buffer outbuf))))

(if running-lucid
    (setq c-macro-preprocessor (gcc-cppcommand)))

(defun my-blink ()
  "interactive version of blink-matching-open"
  (interactive)
  (save-excursion
    (goto-char (+ (point) 1))
    (blink-matching-open)))

(defun copy-sexp-as-kill (arg)
  "Save the sexp as if killed, but don't kill it"
  (interactive "p")
  (save-excursion
    (let ((opoint (point)))
      (forward-sexp arg)
      (copy-region-as-kill opoint (point)))))

(setq *author* "Tim Moore")

;;; Need to set this before hyperspec.el is sucked in.
(setq common-lisp-hyperspec-root "file:///home/moore/lisp/HyperSpec/")

(setq common-lisp-hyperspec-symbol-table
      "/home/moore/lisp/HyperSpec/Data/Map_Sym.txt")

(require 'slime)
;(slime-setup '(slime-repl))

(defun restore-slime-translations ()
  (setq slime-translate-from-lisp-filename-function
	'identity)
  (setq slime-translate-to-lisp-filename-function
	'identity))

(setq inferior-lisp-program
      "sbcl")

(defun sbcl ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (slime))

(defun acl8064 ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((inferior-lisp-program "/home/moore/lisp/acl80.64/mlisp"))
    (slime)))

(defun alisp ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((inferior-lisp-program "/home/moore/lisp/acl80.64/alisp"))
    (slime)))

(defun acl7064 ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((inferior-lisp-program "/home/moore/lisp/acl70.64/mlisp"))
    (slime)))

(defun acl7032 ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((inferior-lisp-program "dchroot -d /home/moore/lisp/acl70/mlisp"))
    (slime)))

(defun pdist-8-64 ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((inferior-lisp-program
	 "/home/moore/lisp/pdist/linux_2_4_x86_64_64/src/lispi -I mlisp.dxl"))
    (slime)))

(defun pdist-7-64 ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((inferior-lisp-program
	 "/home/moore/lisp/pdist7/linux_2_4_x86_64_64/src/lispi -I mlisp.dxl"))
    (slime)))

(defun pdist-8-32 ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((inferior-lisp-program
	 "dchroot -d /home/moore/lisp/pdist/linux_2_4_x86/src/lispi -I mlisp.dxl
"))
    (slime)))

(defun pdist-7-32 ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((inferior-lisp-program
	 "dchroot -d /home/moore/lisp/pdist7/linux_2_4_x86/src/lispi -I mlisp.dxl
"))
    (slime)))

(add-hook 'lisp-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (local-set-key "\r" 'newline-and-indent)
	    (local-set-key "\C-c\C-b" 'my-blink)
	    (setq lisp-indent-function 'common-lisp-indent-function)
	    (setq comment-column 40)
	    (set-fill-column 79)
	    (font-lock-mode)))

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () inferior-slime-mode t))

(setq auto-mode-alist (cons '("\\.cl\\'" . lisp-mode) auto-mode-alist))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (local-set-key "\r" 'newline-and-indent)
	    (setq comment-column 40)
	    (set-fill-column 79)
	    (font-lock-mode)))

(defun common-lisp-mode () (my-cl-mode))
; Make -, ., * and _ letters.
(modify-syntax-entry ?- "w" lisp-mode-syntax-table)
(modify-syntax-entry ?. "w" lisp-mode-syntax-table)
(modify-syntax-entry ?* "w" lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" lisp-mode-syntax-table)

;;; Indent some things differently
(put 'collect
     'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 1 2)) &body))
(put 'once-only
     'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 1 2)) &body))
(put 'pseudo-atomic
     'common-lisp-indent-function
     0)
(put 'sc-case
     'common-lisp-indent-function
     '(4 &rest (&whole 2 &rest 1)))

(put 'defopen
     'common-lisp-indent-hook
     1)

(put 'defopenp
     'common-lisp-indent-hook
     3)

(put 'updating-output
     'common-lisp-indent-function
     '(&lambda &body))

(require 'tramp)

;;; Remote slime hackery
(defvar *my-box-tramp-path*
  "/ssh:moore@10.0.1.3:")
 
(defvar *current-tramp-path* nil)

(defun connect-to-host (path)
  (setq *current-tramp-path* path)
  (setq slime-translate-from-lisp-filename-function
    (lambda (f)
      (concat *current-tramp-path* f)))
  (setq slime-translate-to-lisp-filename-function
    (lambda (f)
      (substring f (length *current-tramp-path*))))
  (slime-connect "localhost" 4005))
 
(defun mac-slime ()
  (interactive)
  (connect-to-host *my-box-tramp-path*))
 
(defun mac-homedir ()
  (interactive)
  (find-file (concat *my-box-tramp-path* "/Users/moore/")))


(require 'tex-site "tex-site.el" t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

(require 'skeleton)

(define-skeleton my-slide-block
  "Prosper slide body"
  nil
  \n "\\begin{slide}"
  ?\{ (skeleton-read "{title}: ") & ?\} | -1
  > \n _ \n
  "\\end{slide}" > \n)

(define-skeleton my-frame-block
  "Beamer slide body"
  nil
  \n "\\begin{frame}"
  \n "\\frametitle" ?\{ (skeleton-read "{title}: ") & ?\} | -1
  > \n _ \n
  "\\end{frame}" > \n)

(setq tex-mode-hook
      '(lambda ()
	 (define-key latex-mode-map "\C-c\C-s" 'my-slide-block)
	 (auto-fill-mode 1)))
(setq latex-mode-hook tex-mode-hook)

(defun make ()
  (interactive)
  (compile "make"))

(add-hook 'TeX-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)))

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)))

;;; mail-setup-hook from rlk's lecture, plus my own stuff
;(defvar my-reply-to "moore@wolfenet.com")

(setq mail-setup-hook			;when sending mail
     '(lambda ()
	 (if to				;if reply, point is below seperator
	     (forward-line -1)
	   (forward-line 2))		;skip To: and Subject: 
	 (if (not cc) (insert "Cc: \n"))
	 (insert "Reply-to: " my-reply-to "\n")
	 (insert
	  "Full-Name: Timothy B. Moore\n")
	 ;; New rmail-reply doesn't insert "Re: ", but I like it.
	 (if (and in-reply-to subject (not (string-match "\\`Re: " subject)))
	     (progn
	       (goto-char (point-min))
	       (re-search-forward "^Subject: ")
	       (insert "Re: ")))
	 (if to			;if a reply move below headers
	     (goto-char (point-max))
	   (goto-char (point-min))	;else go to the To: line
	   (re-search-forward "^To: "))
	 (auto-fill-mode 1)
	 (local-set-key "\^c\^w" 'honig-signature)
	 (if running-lucid
	     (progn
	       (highlight-headers (point-min) (point-max) nil)))))
;;; bind "R" in rmail mode to reply to sender only.

(setq rmail-mode-hook
      '(lambda ()
	 (setq rmail-dont-reply-to-names "moore[%@]?\\|tim@morgan")
	 (setq rmail-ignored-headers
	       (concat "^[xX]-[^ ]*:\\|^precedence:\\|" rmail-ignored-headers))
	 (define-key rmail-mode-map "R" '(lambda ()
					   (interactive)
					   (rmail-reply t)))))

(setq rmail-enable-mime t)

(setq electric-command-history-hook
      '(lambda ()
	 (local-set-key "\C-s" 'isearch-forward)
	 (local-set-key "\C-r" 'isearch-backward)))

;;; Honig says....
(autoload 'honig-says "~/gnu/honig.el"
	  "Inserts what Dave Honig would say at the current point"
	  t nil)
(autoload 'honig-signature "~/gnu/honig.el"
	  "Sign letter with a quote from Dave Honig plus the contents of
~/.signature file"
	  t 'nil)


;;;
;;; some new key bindings
(define-key esc-map "G" 'goto-line)
(define-key esc-map "M" 'compile)
(define-key esc-map "s" 'spell-word)
(define-key esc-map "S" 'spell-buffer)
(define-key global-map "\^cw" 'copy-sexp-as-kill)

;;; map \^h to delete
(setq keyboard-translate-table "\0\1\2\3\4\5\6\7\177")
(define-key global-map "\M-?" 'help-command)
(define-key global-map "\M-?a" 'apropos)

(defvar news-inews-program "/usr/new/lib/news/inews")

(cond (t nil)
      (running-lucid
       (defun egrep (command-args)
	 (interactive
	  (list (read-shell-command "Run egrep (with args):")))
	 (let ((grep-command "egrep -n"))
	   (grep command-args))))
      (running-emacs19-20
       (defun egrep (command-args)
	 (interactive
	  (list (read-from-minibuffer "Run egrep (like this): "
				      "egrep -n " nil nil 'grep-history)))
	 (compile-internal (concat command-args " /dev/null")
			   "No more grep hits" "grep"
			   ;; Give it a simpler regexp to match.
			   nil grep-regexp-alist)))
      ((string-match "Emacs 22" (emacs-version))
       (setq grep-program "egrep")
       (defalias 'egrep 'grep))
      (t
       (defun egrep (command)
	 "Run egrep, with user-specified args, and collect output in a buffer.
While egrep runs asynchronously, you can use the \\[next-error]
command to find the text that ngrep hits refer to"
	 (interactive "sRun egrep (with args): ")
	 (compile1 (concat "egrep -n " command " /dev/null")
		   "No more egrep hits" "grep"))))

(require 'grep)

(setq grep-files-aliases (cons '("j" . "*.java") grep-files-aliases))

;;; The scheme program
(defvar scheme-program-name "guile")

;Sometimes you want to save the *Help* buffer for later examination,
;e.g., when you do an apropos.  save-help will rename the *Help* buffer
;*Help<1>*, *Help<2>*, etc., so the information won't get clobbered by
;further help requests.

;Dale

(defun save-help ()
  (interactive)
  (save-excursion
    (let ((i 1) 
	  (buffer (get-buffer "*Help*"))
	  name)
      (if (not buffer)
	  (ding)
	(while
	    (progn
	      (setq name (concat "*Help<" (int-to-string i) ">*"))
	      (get-buffer name))
	  (setq i (1+ i)))
	(set-buffer buffer)
	(rename-buffer name)
	(message (concat "Help buffer renamed " name))))))

(setq spell-filter
      '(lambda ()
	 (let ((end-of-orig (point-max)))
	   (call-process-region (point-min) end-of-orig "detex"
				t (current-buffer)))))


(global-set-key "\e\$" 'ispell-word)

(global-set-key "\C-x\e" 'electric-command-history)

;;; For broken terminal emulators...
(global-set-key "\^c " 'set-mark-command)

(defun k-and-r-c ()
  (interactive)
  (setq c-indent-level 4)
  (setq c-continued-statement-offset 4)
  (setq c-argdecl-indent 0)
  (setq c-label-offset -4))

(defun gnu-c ()
  (interactive)
  (setq c-indent-level 2)
  (setq c-continued-statement-offset 2)
  (setq c-argdecl-indent 5)
  (setq c-label-offset -2))

(defun bsd-c ()
  (interactive)
  (setq c-indent-level 8)
  (setq c-continued-statement-offset 8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8))

(defun nps-c ()
  (interactive)
  (setq c-continued-statement-offset 3)
  (setq c-brace-offset 0)
  (setq c-indent-level 0))

(defun bryant-c ()
  (interactive)
  (setq c-continued-statement-offset 2)
  (setq c-brace-offset 0)
  (setq c-indent-level 0))

(defun my-new-screen (&optional screen-name)
  "Creates a new emacs screen with the same buffer as the current one."
  (interactive)
  (let ((buffer (current-buffer)))
    (select-screen (x-create-screen
		  (append (if screen-name
			      (list (cons 'name screen-name))
			    nil)
			  screen-default-alist)))
    (switch-to-buffer buffer)))

(global-set-key "\C-c5" 'my-new-screen)

;;; irc fun.
(autoload 'irc "~/gnu/Kiwi.4.30.elc"
  "Internet Relay Chat user interface." t nil)

(setq irc-server "irc.colorado.edu"	;Which server to use.
      irc-msg-public "%s%9s/%s:  "	;How to display public msgs
      ;; Give some information about yourself, whatever you want.
      irc-userinfo "Late night VR and robotics fun"
      ;; Commands to be executed when seeing the Welcome message
      ;; from a server we just connected to.
      irc-startup-hook '(lambda ()
			  (irc-execute-command "alias wc who *")
			  (irc-execute-command "mode moore -i")))

(autoload 'mud "~/gnu/mud.el"
	  "Connect to MUD, asking for site to connect to.
With optional argument, look in mud-entry-file 
for name to connect with and attempt connect."
	  t nil)

(setq visible-bell t)

(autoload 'erc-select "~/gnu/erc-3.0/erc.el"
  "Interactively select connection parameters and run ERC.
Optional argument SERVER uses server as default for the input query.
Optional argument PORT uses passed port as default for the input query.
Optional argument NICK uses the passed nick as default for the input
 query."
  t nil)

(defun erc-lisp ()
  (interactive)
  (erc-select "irc.freenode.net" 6667 "tbmoore"))

(setq org-remember-templates
      '(("Tasks" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/organizer.org" "Tasks") ;; (2)
        ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a"
         "~/organizer.org")
        ("Book" ?b "** %^{Head Line} %^g\n%i%?"  "~/book/book.org" 'bottom)
        ("Ideas" ?i "* %?\n  %i" "~/solo/ideas.org")))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
  '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember) ;; (3)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))           ;; (4)
(global-set-key (kbd "C-c a") 'org-agenda)                       ;; (5)

(setq org-agenda-custom-commands
      '(("P" "Projects"   
         ((tags "PROJECT")))
        ("H" "Office and Home Lists"
         ((agenda)
          (tags-todo "AWAY")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "ERRAND")))
        ("D" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)))))))

(autoload 'maxima-mode "maxima" "Maxima editing mode" t)
(autoload 'maxima "maxima" "Running Maxima interactively" t)

(require 'egg)
(load-library "egg-grep")

(autoload 'imaxima "imaxima" "Maxima frontend" t)
(autoload 'imath "imath" "Interactive Math mode" t)

(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(egg-enable-tooltip t)
 '(org-agenda-files (quote ("~/solo/ideas.org" "~/solo/brico.org" "~/organizer.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-repeating-timestamp-show-all nil)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up) (todo tag-up))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-todo-ignore-deadlines t)
 '(org-agenda-todo-ignore-scheduled t)
 '(org-agenda-todo-ignore-with-date t)
 '(org-agenda-window-setup (quote other-window))
 '(org-deadline-warning-days 7)
 '(org-fast-tag-selection-single-key nil)
 '(org-format-latex-header "\\documentclass{article}
\\usepackage{fullpage}         % do not remove
\\usepackage{amssymb}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage{latexsym}
\\usepackage[mathscr]{eucal}
\\usepackage{lmodern}
\\pagestyle{empty}             % do not remove")
 '(org-log-done (quote (done)))
 '(org-refile-targets (quote (("organizer.org" :maxlevel . 1) ("someday.org" :level . 2))))
 '(org-reverse-note-order nil)
 '(org-tags-column -78)
 '(org-tags-match-list-sublevels nil)
 '(org-use-fast-todo-selection t)
 '(org-use-tag-inheritance nil)
 '(safe-local-variable-values (quote ((package . user) (Package . JAPANESE-GRAPHICS-EDITOR) (TeX-master . "these") (Package . USER) (TeX-master . t) (Package . GOATEE) (Package . TOOL) (Package . ACL-CLIM) (Syntax . ANSI-Common-lisp) (Package . CLIM-DEFSYSTEM) (Package . CLIM-UTILS) (package . tk) \.\.\.)))
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(server-start)
