(setq load-path (append '("~/gnu" "~/gnu/egg" "~/gnu/glsl-mode") load-path))

;;; .h files are likely to be c++
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))

(require 'egg)
(load-library "egg-grep")

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(require 'gtags)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (gtags-mode t)))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (c-set-style "stroustrup")
	    (c-set-offset 'innamespace 0)
	    (c-set-offset 'inline-open 0)))

(setq parens-require-spaces nil)

(global-set-key [(control c) (m)] 'compile-again)
(setq compilation-last-buffer nil)

(defun compile-again (pfx)
  """Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile.
"""
  (interactive "p")
  (if (and (eq pfx 1)
	   compilation-last-buffer)
      (progn
	(set-buffer compilation-last-buffer)
	(revert-buffer t t))
    (call-interactively 'compile)))

(require 'glsl-mode)

(global-set-key (kbd "C-M-g") 'goto-line)

(org-remember-insinuate)

(setq org-directory "~/orgfiles/")
(setq org-default-notes-file "~/orgfiles/notes.org")
(setq org-remember-templates 
  '(("Clipboard" ?c "* %T %^{Description}\n %x" nil "Interesting")
    ("ToDo" ?t "* TODO %T %^{Summary}" nil "Todo")
    ("Bookmark" ?b "* Bookmark %?\n %i\n %a" nil "Bookmark")))

(global-set-key (kbd "C-S-r") 'org-remember)

;; from http://emacs-fu.blogspot.fr/2009/04/remember.html
;;
;; you might also want to set:
;;   (setq org-agenda-skip-unavailable-files t)
;; so these warnings won't annoy the little remember-frame
;; also: I have noted infrequent problems when using ElScreen
;;  (the wrong frame might be chosen for Remember in about 10% of the cases)

(defun make-remember-frame ()
  "turn the current frame into a small popup frame for remember mode;
this is meant to be called with 
     emacsclient -c -e '(djcb-remember-frame)'"
  (modify-frame-parameters nil
    '( (name . "*Remember*") ;; must be same as in mode-hook below  
       (width .  80)
       (height . 10)
       (vertical-scroll-bars . nil)
       (menu-bar-lines . nil)
       (tool-bar-lines . nil)))
  (org-remember)
  (when (fboundp 'x-focus-frame) (x-focus-frame nil)) ;; X only....

  (delete-other-windows)) 

;; when we're in such a remember-frame, close it when done.
(add-hook 'org-remember-mode-hook
  (lambda()
    (define-key org-remember-mode-map (kbd "C-c C-c")
      '(lambda()(interactive)
         (let ((remember-frame-p 
                 (string= (frame-parameter nil 'name) "*Remember*")))
           (when remember-frame-p (make-frame-invisible))  ;; hide quickly

           (org-remember-finalize)
           (when remember-frame-p (delete-frame)))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (whiteboard)))
 '(custom-safe-themes (quote ("60a65134827577812cab9974a7c368f8ad15746fb032ea4a39d0768eafb9e6e2" default)))
 '(safe-local-variable-values (quote ((eval progn (c-set-offset (quote innamespace) (quote 0)) (c-set-offset (quote inline-open) (quote 0))))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
