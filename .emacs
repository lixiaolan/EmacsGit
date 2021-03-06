;; To use aspell
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary "english")
 '(ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
 '(org-agenda-files (quote ("c:/Users/ljefferi/Documents/todo.org"))))

;; For using mumps mode :)
;to use mumps-shell-ssh, instead of mumps-shell-win (which needs ntelnet)

;; ;; settup for tramp mode
;; (setq tramp-default-method "plink")


;;mump (M) mode setup
(setq auto-mode-alist
      (append
       '(("\\.\\(mump\\|ROU\\)\\(\\.\\sw*\\)?$" . mumps-mode)
         ("ARD-.[^_]+-.+\\.txt$" . mumps-mode)
	 ) auto-mode-alist))

(autoload 'mumps-mode "mumps-mode")

;; mumps-shell mode setup
;(autoload 'mumps-shell "mumps-shell" "mumps-shell-mode" t)
;just temporary replaced by following for mumps-shell-win development
(autoload 'mumps-shell-ssh "mumps-shell-win" "\
Start a mump shell process using ssh from a MS window system."
  t)
(autoload 'mumps-shell-win "mumps-shell-win" "\
Start a mump shell process from a MS window system."
  t)
(autoload 'mumps-shell-set-fnd "mumps-shell" "\
Set mumps-shell for FND dev"
  t)
(autoload 'mumps-shell-set-cde "mumps-shell" "\
Set mumps-shell for CDE"
  t)

;; To add all files
(add-to-list 'load-path
	     "~/.emacs.d/WS/Work/emacs/")

;; Add mumps mode
(require 'mumps-mode)

;; csharp mode
(add-to-list 'load-path
	     "~/.emacs.d/csharp")

;; Add csharp mode
(require 'csharp-mode)

;; allows me to do the upcase and downcase region commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; gets rid of start up message menu bars scroll bars and blinking.
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

;; Load my own prefered theme:
(load-theme 'tango-dark t)

;; Allow <shift>-<arrow> to switch buffers
;; (require 'windmove)
;; (windmove-default-keybindings 'shift)

;; Switching buffers using shift instead
(global-set-key (kbd "<S-left>") 'previous-buffer)                                  
(global-set-key (kbd "<S-right>") 'next-buffer)                                     


;;to turn off alarms:
(setq ring-bell-function 'ignore)

;; ;;For Auctex stuff:
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

;; (require 'package)
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa.milkbox.net/packages/")))
;; ;; Apparently needed for the package auto-complete (why?)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(setq url-http-attempt-keepalives nil)

;; Truncated lines by default:
;; (set-default 'truncate-lines t)



;; Org-mode stuff
;; The following lines are always needed.  Choose your own keys.
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; not needed since Emacs 22.2
    (add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cb" 'org-iswitchb)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Makes makes <RET> follow a link when in org mode
(setq org-return-follows-link t)

;; Nice little function to replace lisp eval with the result
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)

;; What does this do?
(put 'dired-find-alternate-file 'disabled nil)

;; Don't use tabs ever!
(setq-default indent-tabs-mode nil)

;; This changes the yas-expand function to bind to C-tab. I did this
;; becuase I was tired of mistakenly tab completing things while using
;; yas mode. C-tab is never bound to anything else as far as I can
;; tell so it is a safe choice.
(add-hook 'yas-global-mode-hook
          (lambda ()
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
            (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/snippets/")))
            ))
(add-hook 'yas-minor-mode-hook
          (lambda ()
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
            (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/snippets/")))
            ))




;; My custom hook for editing xml comments!
(defun ljj-xml-comments-hook ()
  (defun ljj-open-selected-in-buffer () 
    "This takes selected text and opens it in a new buffer at point."
    (interactive)
    (setq ljj-child-buffer "ljj-child-buffer")		; Default scratch buffer
    (setq ljj-text (buffer-substring (mark) (point)))
    (get-buffer-create ljj-child-buffer)
    (setq ljj-parrent-buffer (current-buffer))
    (set-buffer ljj-child-buffer)
    (insert ljj-text)
    (js-mode)
    (uncomment-region (point-min) (point-max))
    (nxml-mode)
    (local-set-key "\C-c'" 'ljj-replace-selected-with-buffer)
    (make-local-variable 'ljj-parrent-buffer)
    (switch-to-buffer ljj-child-buffer)
    t)

  (defun ljj-replace-selected-with-buffer ()
    "This takes all the text from a buffer and replaces selected region with that text"
    (interactive)
    (setq ljj-child-buffer "ljj-child-buffer")	       	; Default scratch buffer
    (set-buffer ljj-child-buffer)
    (js-mode)
    (setq comment-start "///")
    (comment-region (point-min) (point-max))
    (setq ljj-text (buffer-substring (point-min) (point-max)))
    (set-buffer ljj-parrent-buffer)
    (delete-region (mark) (point))
    (insert ljj-text)
    (switch-to-buffer ljj-parrent-buffer)
    (kill-buffer ljj-child-buffer)
    t)  

  (local-set-key "\C-c'" 'ljj-open-selected-in-buffer)

  t)

;; Add the hook to both js-mode and csharp-mode
(add-hook 'js-mode-hook 'ljj-xml-comments-hook)
(add-hook 'csharp-mode-hook 'ljj-xml-comments-hook)







