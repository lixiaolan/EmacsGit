;;File: mumps-shell-win.el
;;
;;Created: Fri Feb 26 22:16:21 1999 Cheng-Wei Wu -cwwu-
;;Time-stamp: <2012-09-26 10:07:28 cwwu>
;;Purpose: MUMPS shell interface for connecting from NT-emacs to 
;;         unix cache server
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; $RCSfile: mumps-shell-win.el,v $ $Revision: 1.5 $
;;
;; $Log: mumps-shell-win.el,v $
;; Revision 1.5  2012/08/15 20:22:29  cwwu
;; update
;;
;; Revision 1.4  2011/12/23 21:47:40  cwwu
;; after making ssh/plink works. The key problem is it seems I should use \r
;; instead or \n when sending respond from interaction, at lease for the
;; cache's Username and Password prompts. I just switch to use \r for
;; telnet-new-line when using ssh. It solves the Username prompt problem and
;; also still works for others prompt. I found this by finally I examined
;; Reflection script and noticed the recorded script use \n for most respond
;; but \r for Username prompt.
;;
;; Still like to do more cleanup if I got more chance.
;;
;; It is recommended to use the new `mumps-shell-ssh' that using tramps/ssh 
;;  since most Caché servers only allow ssh connection but not telnet anymore. 
;;  `mumps-shell-win' which uses telnet is obsolute
;;
;; I also un-comment out the Username prompts for mumps-shell-win using
;; telnet. But I sill have to enter "dev" for Password manually (not like it
;; just worked for mumps-shell-sst).
;;
;; Revision 1.3  2011/12/22 19:41:50  cwwu
;; before try support using SSH
;;
;; Revision 1.2  2011/03/10 15:58:59  cwwu
;; before trying to make mumps-shell environment aware so I can have multiple sessions.
;;
;; Revision 1.1  2002/12/11 18:26:08  cwwu
;; Initial revision
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup:
;;
;; 1. If you use `mumps-shell-ssh' you can skip this step.
;;    You need another telnet program that can take a stdio input
;;    (by Naftali Ramati <naftali@harmonic.co.il>), other than the one come with windows), 
;;    check GNU-emasc FAQ.  You may also need telent-mode.el that customized by me.
;;    Then make sure emacs will use that telent program.  The way I achieve this is 
;;    by saving the telnet.exe in one of my path and rename it as `ntelent', 
;;    then customize the variable telnet-program with the following line in your .emacs file:
;;
;;    (setq telnet-program "ntelnet")  ;need to use Naftali's telnet.
;; 2. To use `mumps-shell-ssh' which required `tramp', customize 
;;    `tramp-default-method' to "plink".
;;
;; 3. Customize variable mumps-shell-user to the user login name on the 
;;    unix server that running cache if necessary, the default is the 
;;    current logged in user on windows.
;;
;; 4. Customize variable mum-shell-win-unix-promp-regexp if necessary.
;;
;; 5. Customize variable mumps-shell-server if necessary, the default is
;;    "epic-devel".
;;
;; 6. Customize variables to your working environment:
;;      mumps-shell-file-server-directory,
;;      mumps-shell-file-local-directory, 
;;      mumps-shell-file-network-directory
;;
;; 6. The default login process will automatically default in the user/cache user ID/
;;    cache logical directory).  This is controled by 
;;    `mumps-shell-user-auto', `mumps-shell-default-userID-auto',
;;    and `mumps-shell-default-dir-auto'.  The latter two variables are defined in 
;;    mumps-shell.  If you don't want this default feature, customize
;;    the corresponding variable to nil.  Currently, this automatic default feature does
;;    not work with mumps-shell alone (cache server is on the local server).

;;
;; To start a mumps-shell: `M-x mumps-shell-ssh'
;;

(require 'mumps-shell)  ;the main mumps-shell
(require 'tramp)        ;using ssh
(require 'telnet)      ;require telnet package  ;TODO - drop support for telnet or make it conditionally required if not using ssh
;(require 'telnet-mode) ;*CW 12/2002 no need anymore
;(require 'ange-ftp)    ;require ange-ftp

;some customizable variables for mumps-shell-win

(defvar mumps-shell-user-auto nil
  "*When set to t it will default in the unix server user name from
`mumps-shell-user' during the server login process")

(defvar mumps-shell-user user-login-name
  "*The user name to logon to the unix server running cache. The default is 
user name for the current logged in user.")

(defvar mumps-shell-win-unix-prompt-regexp
  (concat "^" mumps-shell-user " ")
  "*Regexp matching prompts for unix prompt.")

;Customized values defined in mumps-shell

(set-variable 'mumps-shell-server "epic-trn101") ;the unix server running cache: override the one in mumps-shell

;(set-variable 'mumps-shell-file-network-directory (concat "/" mumps-shell-user "@" mumps-shell-server ":/")) ;when used with ange-ftp
(set-variable 'mumps-shell-file-network-directory (concat "//epic-nfs/")) ;*CW+2 12/2002 now samba is readable/writable now
(set-variable 'mumps-shell-file-server-directory "/net_home/ljefferi/")
(set-variable 'mumps-shell-file-local-directory "/net_home/ljefferi/") ; replace “cwwu” with your unix user and create folders work/dev in your home directory

;functions for mumps-shell-win

;This is the one used to start the mumps-shell from emacs on windows
;;;###autoload
(defun mumps-shell-win ()
  "Run mumps-shell on a unix cache server from a MS window system."
  (interactive)
  (require 'telnet)
  (if (not (comint-check-proc (concat "*" mumps-shell-process-name "*")))
      (mumps-shell-win-start-process)
    ;(pop-to-buffer (process-buffer (get-process mumps-shell-process-name)))
    (mumps-shell-display-shell t))
  )

;;;###autoload
(defun mumps-shell-ssh ()
  "Run mumps-shell via a ssh (plink)."
  (interactive)
  (if (not (comint-check-proc (concat "*" mumps-shell-process-name "*")))
      (mumps-shell-win-start-ssh-process)
    ;(pop-to-buffer (process-buffer (get-process mumps-shell-process-name)))
    (mumps-shell-display-shell t))
  )
  

;;currently this one is not used
(defun mumps-shell-win-watch-for-unix-prompt-to-send-M (string)
  "Watch for unix prompt to send the mumps-shell-cmd command. This is used in the list `comint-output-filter-functions'."
  (if (string-match mumps-shell-win-unix-prompt-regexp string)
      ;(send-invisible nil)
      (send-invisible mumps-shell-cmd)
    ))

(defun mumps-shell-win-start-ssh-process (&optional host)
  "Spawn a new M session in enviornment env from NT-emacs via ssh."
  (interactive)
  (setq mumps-shell-working-dir "") ;reset
  (setq mumps-shell-working-env (upcase mumps-shell-cmd))
  (setq mumps-shell-process-name (concat "mumps-shell" "-" mumps-shell-working-env))  ;*CW 3/10/11
  ;*CW+5 9/02 not working with the way I start mumps-shell yet
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-win-watch-for-unix-prompt-to-send-M)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-user-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-passwd-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-directory-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-error-prompt)
  (let* ((defhost mumps-shell-server)
	 (comint-delimiter-argument-list '(?\  ?\t))
	 (process)
	 ;(name (concat "telnet-" (comint-arguments host 0 nil) ))
	 )
    (if current-prefix-arg
	(setq host (read-string (concat "host: [" defhost "] "))))
    (if (null host) (setq host defhost))
    (if (string= host "") (setq host defhost))
    (cond ((string-equal system-type "windows-nt")
	   (setq telnet-new-line "\r")  ;*CW "\n" causing trouble at cache Username/Password prompt found out by looking at Reflection script
           (setq comint-process-echoes t)
           ))
    (pop-to-buffer (make-comint mumps-shell-process-name "plink" nil "ljefferi@epic-trn101"))
    (setq process (get-buffer-process (current-buffer)))
    ;(set-process-filter process 'telnet-initial-filter) ;*CW 9/02 change to use a specific one
    (set-process-filter process 'mumps-shell-win-ssh-initial-filter)
    (accept-process-output process)
    ;(telnet-mode)
    ;(setq comint-input-sender 'telnet-simple-send)
    ;(setq telnet-count telnet-initial-count)
;       (set-process-sentinel proc 'mumps-shell-sentinel)
    ;here we start the m process?
    ;(telnet-simple-send process mumps-shell-cmd)
    (setq mode-line-format mumps-shell-mode-line-format)
    (setq default-directory (mumps-shell-get-client-file-directory))
    (use-local-map mumps-shell-map)
    (run-hooks 'mumps-shell-mode-hook)
;       (while (zerop (buffer-size))
; 	(sleep-for 1)
; 	)
;       )
    )
  )


(defun mumps-shell-win-start-process (&optional host)
  "Spawn a new M session in enviornment env from NT-emacs."
  (interactive)
  (setq mumps-shell-working-dir "") ;reset
  (setq mumps-shell-working-env (upcase mumps-shell-cmd))
  (setq mumps-shell-process-name (concat "mumps-shell" "-" mumps-shell-working-env))  ;*CW 3/10/11
  ;*CW+5 9/02 not working with the way I start mumps-shell yet
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-win-watch-for-unix-prompt-to-send-M)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-user-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-passwd-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-directory-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-error-prompt)
  (let* ((defhost mumps-shell-server)
	 (comint-delimiter-argument-list '(?\  ?\t))
	 (process)
	 ;(name (concat "telnet-" (comint-arguments host 0 nil) ))
	 )
    (if current-prefix-arg
	(setq host (read-string (concat "host: [" defhost "] "))))
    (if (null host) (setq host defhost))
    (if (string= host "") (setq host defhost))
    (cond ((string-equal system-type "windows-nt")
	   (setq telnet-new-line "\n")))
    (pop-to-buffer (make-comint mumps-shell-process-name telnet-program nil host))
    (setq process (get-buffer-process (current-buffer)))
    ;(set-process-filter process 'telnet-initial-filter) ;*CW 9/02 change to use a specific one
    (set-process-filter process 'mumps-shell-win-initial-filter)
    (accept-process-output process)
    ;(telnet-mode)
    (setq comint-input-sender 'telnet-simple-send)
    (setq telnet-count telnet-initial-count)
;       (set-process-sentinel proc 'mumps-shell-sentinel)
    ;here we start the m process?
    ;(telnet-simple-send process mumps-shell-cmd)
    (setq mode-line-format mumps-shell-mode-line-format)
    (setq default-directory (mumps-shell-get-client-file-directory))
    (use-local-map mumps-shell-map)
    (run-hooks 'mumps-shell-mode-hook)
;       (while (zerop (buffer-size))
; 	(sleep-for 1)
; 	)
;       )
    )
  )

;;*CW 9/2002 change the override telnet-initial-filter to 
;; mumps-shell-win-initial-filter and modify to include cache login
;;
;;*CW 10/27/2000-override telnet-initial-filter defined in telnet.el
;; to enable filter the login prompt, assuming the prompt is "login"
;; and enable M prompts
;;;;
(defun mumps-shell-win-initial-filter (proc string)
  ;For reading up to and including password; also will get machine type.
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((case-fold-search t) (cntA 0) (cntB 0)  (cntC 0))
      (cond ((string-match "No such host" string)
	     (kill-buffer (process-buffer proc))
	     (error "No such host"))
            ;;*CW 9/02 the original one in telnet.el did not check "login", it uses 
            ;;         comint-password-prompt-regexp in comint.el which makes any string
            ;;         "login" will be matched for login prompt, epic-devel have message
            ;;         of last login info and therefore cause the second Login: prompt
            ;;         in emacs telnet process
            ;;*CW 3/03 change the login matching regexp from "^login:" to "[^ ]login:"
            ;;         the previous setting is not working well to catch it the first
            ;;         time in the login prompt (if you press return then it will 
            ;;         catch the second login prompt.  The changed regext work better
            ;;         it catches the frist one and ignore the "Last login:".            
	    ((string-match "[^ ]login:" string)  
	     (telnet-filter proc string)
             (if (and mumps-shell-user-auto (stringp mumps-shell-user) (not (= cntA 1)))
                 (progn
                   (send-string proc (concat mumps-shell-user telnet-new-line))
                   (setq cntA 1))
               (send-string proc (concat (comint-read-noecho "Login: " t)
                                         telnet-new-line))
               (setq cntA 1))
             
	     )
	    ((string-match "passw" string)
	     (telnet-filter proc string)
	     (setq telnet-count 0)
	     (send-string proc (concat (comint-read-noecho "Password: " t)
				       telnet-new-line))
	     (clear-this-command-keys))
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-win-watch-for-unix-prompt-to-send-M)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-user-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-passwd-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-directory-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-error-prompt)
	    ((string-match mumps-shell-win-unix-prompt-regexp string)  
	     (telnet-filter proc string)
             (send-string proc (concat mumps-shell-cmd telnet-new-line))
             )
            ;<<<< *CW+6 5/15/2006 new additional cache prompts
            ((string-match mumps-shell-username-prompt-regexp string)
                 (telnet-filter proc string)
                 (send-string proc (concat mumps-shell-username telnet-new-line)))
            ((string-match mumps-shell-username-password-prompt-regexp string)
                 (telnet-filter proc string)
                 (send-string proc (concat mumps-shell-username-password telnet-new-line)))
            ;;<<<< *CW 5/15/2006
	    ((string-match mumps-shell-user-prompt-regexp string)  
	     (telnet-filter proc string)
             (if (and mumps-shell-default-userID-auto (stringp mumps-shell-default-userID) (not (= cntB 1)))
                 (progn
                   (send-string proc (concat mumps-shell-default-userID telnet-new-line))
                   (setq cntB 1))
               (send-string proc 
                          (concat (mumps-shell-read-minibuffer "User ID: " mumps-shell-default-userID)
                                  telnet-new-line))
               (setq cntB 1))
             )
	    ((string-match mumps-shell-password-prompt-regexp string)
	     (telnet-filter proc string)
	     (setq telnet-count 0)
	     (send-string proc (concat (mumps-shell-read-minibuffer "User Code: " "" t)
				       telnet-new-line))
	     (clear-this-command-keys))
	    ((string-match mumps-shell-directory-prompt-regexp string)
	     (telnet-filter proc string)
             (message "") ;*CW 6/6/03 some how need this to make the auto default work?
             ;(message "%S" mumps-shell-directory-prompt-regexp)
             ;(sleep-for 3)
             (if (and mumps-shell-default-dir-auto (stringp mumps-shell-default-dir) (not (= cntC 1)))
                 (progn
                   (send-string proc (concat mumps-shell-default-dir telnet-new-line))
                   (setq mumps-shell-working-dir mumps-shell-default-dir)
                   (setq cntC 1))
               (setq mumps-shell-working-dir (mumps-shell-read-minibuffer "Directory: " mumps-shell-default-dir))
               (send-string proc (concat mumps-shell-working-dir telnet-new-line))
               (setq cntC 1))
	     )
	    (t (telnet-check-software-type-initialize string)
	       (telnet-filter proc string)
	       (cond ((> telnet-count telnet-maximum-count)
		      (set-process-filter proc 'telnet-filter))
		     (t (setq telnet-count (1+ telnet-count)))))))))

;;*CW 12/2011 use SSH
(defun mumps-shell-win-ssh-initial-filter (proc string)
  ;For reading up to and including password; also will get machine type.
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((case-fold-search t) (cntA 0) (cntB 0)  (cntC 0))
      (cond ((string-match "No such host" string)
	     (kill-buffer (process-buffer proc))
	     (error "No such host"))
            ;;*CW 9/02 the original one in telnet.el did not check "login", it uses 
            ;;         comint-password-prompt-regexp in comint.el which makes any string
            ;;         "login" will be matched for login prompt, epic-devel have message
            ;;         of last login info and therefore cause the second Login: prompt
            ;;         in emacs telnet process
            ;;*CW 3/03 change the login matching regexp from "^login:" to "[^ ]login:"
            ;;         the previous setting is not working well to catch it the first
            ;;         time in the login prompt (if you press return then it will 
            ;;         catch the second login prompt.  The changed regext work better
            ;;         it catches the frist one and ignore the "Last login:".            
	    ((string-match "[^ ]login as:" string)  ;Password for /plink:cwwu@epic-devel:
	     (telnet-filter proc string)
             (if (and mumps-shell-user-auto (stringp mumps-shell-user) (not (= cntA 1)))
                 (progn
                   (send-string proc (concat mumps-shell-user telnet-new-line))
                   (setq cntA 1))
               (send-string proc (concat (comint-read-noecho "Login: " t)
                                         telnet-new-line))
               (setq cntA 1))
             
	     )
	    ((string-match "fnd's passw" string)
	     (telnet-filter proc string)
	     (setq telnet-count 0)
	     (send-string proc (concat (comint-read-noecho "Password: " t)
				       telnet-new-line))
	     (clear-this-command-keys))
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-win-watch-for-unix-prompt-to-send-M)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-user-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-passwd-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-directory-prompt)
;;   (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-error-prompt)
	    ((string-match mumps-shell-win-unix-prompt-regexp string)  
	     (telnet-filter proc string)
             (send-string proc (concat mumps-shell-cmd telnet-new-line))
             )
            ;<<<< *CW+6 5/15/2006 new additional cache prompts: only FND env needs this?!
            ((string-match mumps-shell-username-prompt-regexp string)
             (telnet-filter proc string)
             (sleep-for 4)
             (send-string proc (concat mumps-shell-username telnet-new-line))
             )
            ((string-match mumps-shell-username-password-prompt-regexp string)
             (telnet-filter proc string)
             (send-string proc (concat mumps-shell-username-password telnet-new-line)))
            ;;<<<< *CW 5/15/2006
	    ((string-match mumps-shell-user-prompt-regexp string)  ;Enter User ID
	     (telnet-filter proc string)
             (if (and mumps-shell-default-userID-auto (stringp mumps-shell-default-userID) (not (= cntB 1)))
                 (progn
                   (send-string proc (concat mumps-shell-default-userID telnet-new-line))
                   (setq cntB 1))
               (send-string proc 
                          (concat (mumps-shell-read-minibuffer "User ID: " mumps-shell-default-userID)
                                  telnet-new-line))
               (setq cntB 1))
             )
	    ((string-match mumps-shell-password-prompt-regexp string)
	     (telnet-filter proc string)
	     (setq telnet-count 0)
	     (send-string proc (concat (mumps-shell-read-minibuffer "User Code: " "" t)
	         		       telnet-new-line))
	     (clear-this-command-keys))
	    ((string-match mumps-shell-directory-prompt-regexp string)
	     (telnet-filter proc string)
             (message "") ;*CW 6/6/03 some how need this to make the auto default work?
             ;(message "%S" mumps-shell-directory-prompt-regexp)
             ;(sleep-for 3)
             (if (and mumps-shell-default-dir-auto (stringp mumps-shell-default-dir) (not (= cntC 1)))
                 (progn
                   (send-string proc (concat mumps-shell-default-dir telnet-new-line))
                   (setq mumps-shell-working-dir mumps-shell-default-dir)
                   (setq cntC 1))
               (setq mumps-shell-working-dir (mumps-shell-read-minibuffer "Directory: " mumps-shell-default-dir))
               (send-string proc (concat mumps-shell-working-dir telnet-new-line))
               (setq cntC 1))
	     )
	    (t (telnet-check-software-type-initialize string)
	       (telnet-filter proc string)
	       (cond ((> telnet-count telnet-maximum-count)
		      (set-process-filter proc 'telnet-filter))
		     (t (setq telnet-count (1+ telnet-count)))))))))


(provide 'mumps-shell-win)

;;
;;EOF mumps-shell-win.el
