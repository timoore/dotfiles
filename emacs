
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)

(load-file "~/.emacs.d/init.el")
(load-file "~/.emacs.d/org.el")
(unless (eq system-type 'windows-nt)
  (load-file "~/.emacs.d/lsp.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(calendar-date-style 'iso)
 '(custom-safe-themes
   '("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "89cc8d0ddb02b9528fe5f9f07bbe575865ae0e3e5d822b1e85f23bf50056e00a" "60a65134827577812cab9974a7c368f8ad15746fb032ea4a39d0768eafb9e6e2" default))
 '(ecb-options-version "2.40")
 '(package-archives
   '(("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("org" . "http://orgmode.org/elpa/")))
 '(package-selected-packages
   '(julia-snail keyword-search vterm glsl-mode julia-mode json-mode org-ref org-roam deft htmlize org-plus-contrib helm use-package pdf-tools magit magic-latex-buffer diminish bind-key))
 '(safe-local-variable-values
   '((Base . 10)
     (Syntax . ANSI-Common-Lisp)
     (eval progn
           (c-set-offset 'innamespace '0)
           (c-set-offset 'inline-open '0))
     (package . user)
     (Package . JAPANESE-GRAPHICS-EDITOR)
     (TeX-master . "these")
     (Package . USER)
     (TeX-master . t)
     (Package . GOATEE)
     (Package . TOOL)
     (Package . ACL-CLIM)
     (Syntax . ANSI-Common-lisp)
     (Package . CLIM-DEFSYSTEM)
     (Package . CLIM-UTILS)
     (package . tk)
     \.\.\.))
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((comp) (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JuliaMono" :foundry "UKWN" :slant normal :weight normal :height 113 :width normal))))
 '(magit-item-highlight ((t nil))))
