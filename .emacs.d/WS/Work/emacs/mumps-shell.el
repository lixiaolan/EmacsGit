;;File: mumps-shell.el
;;
;;Created: Fri Feb 26 22:16:21 1999 Cheng-Wei Wu -cwwu-
;;Time-stamp: <2012-09-26 09:54:03 cwwu>
;;Purpose: MUMPS shell interface
;;        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; $RCSfile: mumps-shell.el,v $ $Revision: 1.6 $
;;
;; $Log: mumps-shell.el,v $
;; Revision 1.6  2012/08/15 20:23:56  cwwu
;; update
;;
;; Revision 1.5  2011/03/10 15:10:29  cwwu
;; before trying to make mumps-shell have locale variable keep track of the env
;; so I can have multiple mumps-shell to different server/env.
;;
;; Revision 1.4  2011/03/07 22:02:19  cwwu
;; adding 2 functions to set shell env: fnd and cde
;;
;; Revision 1.3  2008/06/04 04:52:41  cwwu
;; A version support FND dev env.  Still based on emacs 21.3.
;;
;; Revision 1.2  2005/11/07 22:07:04  cwwu
;; Before I changed the default to FND dev env.
;;
;; Revision 1.1  2002/12/11 18:24:50  cwwu
;; Initial revision
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Note: to connect to a M session on a remote machine, such as from
;;       emacs on a windows machine to a unix server running M, see
;;       mumps-shell-win.el as well.
;;
;; Setup:
;;
;; 1. Use following code in your emacs set-up file (such as ~/.emacs):
;;
;;    ;;mumps-shell mode setup - this is only if you running emacs 
;;        directly from a server host the Caché server. 
;;    (autoload 'mumps-shell "mumps-shell" "Start a mumps shell process" t)
;;
;;    ;;To load mumps-shell-win automatically
;;    (autoload 'mumps-shell-ssh "mumps-shell-win" "\
;;    Start a mumps shell ssh process from a MS window system."
;;      t)
;;
;; 2. Customize variable mumps-shell-default-userID, this is the UID that 
;;    default in as the M login user ID prompt.  If you don't change this
;;    you will still be able to enter your own ID interactively during
;;    logon.  The default is my UID `592'.
;;
;; 3. Customize variable mumps-shell-cmd if necessary, this is the unix
;;    shell to start a cache session without go through environment
;;    selection menu.  The default is my script `fnd'.
;;
;; 4. Customize variable mumps-shell-file-local-directory if necessary.  This is
;;    the directory path on the M server that M routine will be saved into
;;    then accessed by emacs with mumps-mode.  The default is "~/work/dev/".
;;    If the cache is running on a remoter server (from the client that is
;;    running emacs), you will need to set mumps-shell-file-server-directory
;;    and mumps-shell-file-network-directory.  See the documentation on these
;;    variables and mumps-shell-win.el for more detail.
;;
;; 5. Customize variable mumps-shell-default-dir if necessary.  This is
;;    the default logical directory for the M logon process.  The default
;;    is "DEV".
;;
;; 6. When used with mumps-shell-win (the cache server is on a remote unix), the default
;;    login process will automatically default in the user/cache user ID/
;;    cache logical directory).  This is controled by 
;;    `mumps-shell-user-auto', `mumps-shell-default-userID-auto',
;;    and `mumps-shell-default-dir-auto'.  If you don't want this default feature, customize
;;    the corresponding variable to nil.  Currently, this automatic default feature does
;;    not work with mumps-shell alone (cache server is on the local server).
;;
;; To start a mumps-shell: `M-x mumps-shell-ssh' or `M-x mumps-shell'
;;
;; Some useful commands in  mumps-shell mode/buffer, see mumps-shell-map
;; for more detail:
;;
;;       "\C-cf"    'mumps-shell-checkin-routine-with-version-lock
;;       "\C-c\C-f" 'mumps-shell-find-routine
;;       "\C-c\C-r" 'mumps-shell-find-routine-read-only
;;       "\C-c\C-v" 'mumps-shell-find-file
;;       "\C-cl"    'mumps-shell-routine-lock
;;       "\C-cr"    'mumps-shell-routine-unlock
;;       "\C-cc"    'mumps-shell-checksum
;;
;;       "\C-ck"     'mumps-shell-show-lock-count
;;       "\C-cm"     'mumps-shell-lookitt-macro
;;
;;       "\C-ce"    'mumps-shell-change-env
;;       "\C-cd"    'mumps-shell-change-dir
;;       "\C-c\C-d" 'mumps-shell-change-dev
;;       "\C-c\C-e" 'mumps-shell-change-envdir

(require 'comint)

;some customizable variables for mumps-shell

(defcustom mumps-shell-cmd "cde"
  "*The shell command to start a cache session: [\"fnd\"|\"cde\"|\"ide\"]"
  :group 'mumps-shell)

(defcustom mumps-shell-server "epic-fnd"
  "*The unix server name that runs cache if running on a remote machine. 
See mumps-shell-win.el for more detail.
For example: \"epic-devel\", \"epic-fnd\""
  :group 'mump-shell)

(defcustom mumps-shell-file-network-directory nil
  "*The network working path if running cache on a remote machine.
The path specified here should end with `/' if not nil. 
This is used with ange-ftp so it can access the file remotely from emacs
with mumps-shell.  It is also used for the mumps-shell to substract the path 
when used reomtely, so it can pass the correct full name to cache utility
for saving/loading between cache and the plain file. 

With mumps-shell-win (running cache remotely) and ange-ftp, for example:
`mumps-shell-file-server-directory'  = `/net_home/'.
`mumps-shell-file-local-directory'   = `cwwu/work/dev/',
`mumps-shell-file-network-directory' = `/cwwu@epic-devel:/net_home/',
If accessing the saved plain file through ange-ftp, it becomes reading a file 
`/cwwu@epic-devel:/net_home/cwwu/work/dev/routine.mumps'.  But when mumps-shell 
loads the file back to cache environment, it can't use this name, becuse cache 
have to use a file name with its real path on the server that is running cache.
That is `/net_home/cwwu/work/dev/routine.mumps'.

If a NFS file sytem is available to the local machine for accessing files
on the server that is running cache, for example, on a window system, samba 
can be used to make a unix server files accessible from a window system.  Then 
we don't need to use ange-ftp to read/write the saved routine file but can
read/write directly through the NFS path.  For example:
The routine file saved on cache server:
`/net_home/cwwu/work/dev/routine.mumps' it is accessbile throuh a NFS from
window as `//epic-nfs/cwwu/work/dev/routine.mumps'.  So we set:
  `mumps-shell-file-server-directory'  = `/net_home/'
  `mumps-shell-file-local-directory'   = `cwwu/work/dev/'
  `mumps-shell-file-network-directory' = `//epic-nfs/'

See `mumps-shell-file-server-directory' and mumps-shell-win.el for more detail."
  :type 'directory
  :group 'mumps-shell)

(defcustom mumps-shell-file-server-directory nil
  "*This is only needed to be defined if cache is not running on the local
machine and a NFS file path is available to the local machine for accessing
files on the server that is running cache.  On a window system, for example, 
this can be achieved by the samba server.  When this NFS is avaiable for 
both read/write, although a cache session still have to be established 
through mumps-shell/mumps-shell-win through a telnet session, however, we 
don't need to use ange-ftp to read/write the file, but instead, emacs can
read/write through samba (like a local file).

See `mumps-shell-file-network-directory' and mumps-shell-win.el for more detail."
  :type 'directory
  :group 'mumps-shell)

(defcustom mumps-shell-file-local-directory "~/work/dev/"
  "*The (local) working directory path on the cache server that cache 
routines are saved into. This should be an absolute path if 
`mumps-shell-file-network-directory' and `mumps-shell-file-server-directory'
are used (not nil).  Use relative file name if 
`mumps-shell-file-network-directory' is used."
  :type 'directory
  :group 'mumps-shell)

(defcustom mumps-shell-default-userID-auto nil
  "*When set to t it will default in the cache user ID set from 
`mumps-shell-default-userID' during the cache session login process.
Currently, this only work with mumps-shell-win."
  :type 'boolean
  :group 'mumps-shell)

(defcustom mumps-shell-default-userID "20643"
  "*The default cache user ID"
  :type 'number
  :group 'mumps-shell)

(defcustom mumps-shell-default-dir-auto nil
  "*When set to t it will default in the login logical directory set from
 `mumps-shell-default-dir' during the cache session login process.
Currently, this only work with mumps-shell-win."
  :type 'boolean
  :group 'mumps-shell)

(defcustom mumps-shell-default-dir "EMP"
  "*The default logon cache logical directory for the cache logon directory
prompt. DEV for FND, EMP for CDE. This often changes."
  :type 'string
  :group 'mumps-shell)

;;;

(defvar mumps-shell-mode-hook nil
  "*List of functions to call when entering mumps-shell mode.")

;keymap for mumps-shell mode
(defvar mumps-shell-map nil
  "Keymap for mumps-shell major mode.")

(cond ((not mumps-shell-map)
       (let ((map (make-sparse-keymap "Mumps-Shell"))
             (map2 (make-sparse-keymap)))
	 ;(setq mumps-shell-map (make-sparse-keymap))
	 (setq map2 (nconc (make-sparse-keymap) comint-mode-map))
         (define-key map2 "\C-cz" 'mumps-shell-OS-cmd)
	 (define-key map2 "\C-c\C-e" 'mumps-shell-change-envdir)
	 (define-key map2 "\C-c\C-d" 'mumps-shell-change-dev)
	 (define-key map2 "\C-cd" 'mumps-shell-change-dir)
	 (define-key map2 "\C-ce" 'mumps-shell-change-env)
         (define-key map2 "\C-cm" 'mumps-shell-lookitt-macro)
         (define-key map2 "\C-ck" 'mumps-shell-show-lock-count)
	 (define-key map2 "\C-cc" 'mumps-shell-checksum)
	 (define-key map2 "\C-cr" 'mumps-shell-routine-unlock)
	 (define-key map2 "\C-cl" 'mumps-shell-routine-lock)
         (define-key map2 "\C-c\C-v" 'mumps-shell-find-file)
	 (define-key map2 "\C-c\C-r" 'mumps-shell-find-routine-read-only)
	 (define-key map2 "\C-c\C-f" 'mumps-shell-find-routine)
	 (define-key map2 "\C-cf" 'mumps-shell-checkin-routine-with-version-lock)
	 ;
	 ;the menu bar keymap
	 (define-key map2 [menu-bar] (make-sparse-keymap))
	 (define-key map2 [menu-bar mumps-shell]
	   (cons "Mumps-Shell" map))
         (define-key map [zOScmd]
           '("OS command" . mumps-shell-OS-cmd))
         (define-key map [separator] '("--"))
	 (define-key map [change-envdir] 
	   '("Change env then dir" . mumps-shell-change-envdir))
	 (define-key map [change-dev]
	   '("Change dev dir (PDAR/SDE/IDE/FND)" . mumps-shell-change-dev))
	 (define-key map [change-dir]
	   '("Change dir" . mumps-shell-change-dir))
	 (define-key map [change-env]
	   '("Change env" . mumps-shell-change-env))
         (define-key map [lookitt-macro]
           '("Lookitt macro" . mumps-shell-lookitt-macro))
         (define-key map [show-lock-count] '("Show lock count" . mumps-shell-show-lock-count))
         (define-key map [separator] '("--"))
	 (define-key map [routine-checksum]
	   '("Routine checksum" . mumps-shell-checksum))
	 (define-key map [routine-release-lock]
	   '("Release lock" . mumps-shell-routine-unlock))
	 (define-key map [routine-lock]
	   '("Lock routine" . mumps-shell-routine-lock))
         (define-key map [find-remote-file]
           '("Find remote file" . mumps-shell-find-file))
	 (define-key map [find-read-only]
	   '("Find read-only" . mumps-shell-find-routine-read-only))
	 (define-key map [find-routine]
	   '("Find routine" . mumps-shell-find-routine))
	 (define-key map [checkin-routine-version-lock]
	   '("Checkin routine with version lock" . mumps-shell-checkin-routine-with-version-lock))
         (setq mumps-shell-map map2)
	 )))

;some variables for mumps-shell

;*CW++ 3/10/2011
;; (make-local-variable mumps-shell-server)
;; (make-local-variable mumps-shell-cmd)
;; (make-local-variable mumps-shell-working-env)
;; (make-local-variable mumps-shell-working-dir)
;;(make-local-variable mumps-shell-process-name)
;
(defvar mumps-shell-working-env nil)
(defvar mumps-shell-working-dir nil)
(defvar mumps-shell-process-name nil)


;;The core cache utilities to work with mumps-shell
(defvar mumps-shell-routine-save-tag "emRSave^%zcwemac")
(defvar mumps-shell-routine-load-tag "emRLoadn^%zcwemac")
(defvar mumps-shell-change-env-tag "chgenv^%zcweuti")
(defvar mumps-shell-change-dir-tag "chgdir^%zcweuti")
(defvar mumps-shell-change-envdir-tag "chgedir^%zcweuti")
(defvar mumps-shell-lock-rou-tag "getlock^%zcwemac")
(defvar mumps-shell-release-lock-rou-tag "rellock^%zcwemac")
(defvar mumps-shell-routine-checksum-tag "$$ChkObj^%ZaHSUM2")
(defvar mumps-shell-OS-cmd-tag "zOScmd^%Zdfnlie")
(defvar mumps-shell-lookitt-macro-tag "doMac^%ZeWMCR2")

(defvar mumps-shell-mode-line-format
  '("-"
    mode-line-mule-info
    mode-line-modified
    mode-line-frame-identification
    mode-line-buffer-identification " "
    "<" mumps-shell-working-env ":" mumps-shell-working-dir ">  "
    global-mode-string
    "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--"
    (which-func-mode
     ("" which-func-format "--"))
    (line-number-mode "L%l--")
    (column-number-mode "C%c--")
    (-3 . "%p") "-%-")
  "Mode line format for mumps-shell mode buffer.")

(defvar mumps-shell-username-prompt-regexp
  "^Username: "
  "*Regexp matching prompts for username when starting cache before user ID prompt.")

(defvar mumps-shell-username
  "dev"
  "*Username for the username prompt before user ID prompt.")

(defvar mumps-shell-username-password-prompt-regexp
  "^Password: "
  "*Regexp matching prompts for username password when starting cache before user ID prompt.")

(defvar mumps-shell-username-password
  "dev"
  "*Password for the username prompt before user ID prompt.")

(defvar mumps-shell-user-prompt-regexp
  "^Enter User ID: "
  "*Regexp matching prompts for user id in the inferior process.")

(defvar mumps-shell-password-prompt-regexp
  "^Enter User Code: "
  "*Regexp matching prompts for user code in the inferior process.")

(defvar mumps-shell-directory-prompt-regexp
  ;"^Which \\w* Directory\\? .*//"
  "^Which .* Directory\\? .*//$" 
  ;"^Which \\(\\w*\\|\\w* \\w*\\) Directory\\? .*//"
  ;"^.*Directory\\? .*//"
  "*Regexp matching prompts for directory in the inferior process.")

(defvar mumps-shell-error-prompt-regexp
  "^Error:.*$"
  "*Regexp matching prompts for errors in the inferior process.")

;functions for mumps-shell

(defun mumps-shell-get-client-file-directory ()
;(defun mumps-shell-file-path ()
  "Return the prefix of the file path that can be used by mumps-shell
to access the plain file (routine) saved by mumps-shell."
  (interactive)
  ;(concat mumps-shell-file-network-directory mumps-shell-file-local-directory))
  (expand-file-name mumps-shell-file-local-directory mumps-shell-file-network-directory))

(defun mumps-shell-get-server-file-directory ()
  "Return the full path name used on the server running cache."
  (interactive)
  ;(concat mumps-shell-file-server-directory mumps-shell-file-local-directory))
  ;(expand-file-name mumps-shell-file-local-directory mumps-shell-file-server-directory))
  ;CW-1 expand-file-name will always affected by its current default-directory value?!
  (concat mumps-shell-file-server-directory mumps-shell-file-local-directory))

(defun mumps-shell-get-server-file-fullname (fullFileName)
;(defun mumps-shell-get-local-path (fullFilePath)
  "Return the full file name used on the server running cache.
`fullFileName' is the full file name used on the client side."
  (interactive "FFile: ")
  ;(concat mumps-shell-file-server-directory
  ;  (substring fullFilePath (length mumps-shell-file-network-directory))
  ;  )
  ;(expand-file-name (file-relative-name fullFileName (mumps-shell-get-client-file-directory))
  ;                  (mumps-shell-get-server-file-directory))
  ;CW-1 expand-file-name will always affected by its current default-directory value?!
  (concat (mumps-shell-get-server-file-directory)
          (file-relative-name fullFileName (mumps-shell-get-client-file-directory)))
  )

(defun mumps-shell-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil)
         ;(tex-delete-last-temp-files)
	 )
	((memq (process-status proc) '(signal exit))
         ;(tex-delete-last-temp-files)
	 )))


(defun mumps-shell-start-process ()
  "Spawn a new M session in enviornment env."
  (interactive)
  (setq mumps-shell-working-env (upcase mumps-shell-cmd))
  (setq mumps-shell-process-name (concat "mumps-shell" "-" "test")) ;*CW 3/10/11 ;well, server is not enough yet TODO
  (setq comint-output-filter-functions (list 'mumps-shell-watch-for-user-prompt))
  (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-passwd-prompt)
  (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-directory-prompt)
  (add-hook 'comint-output-filter-functions 'mumps-shell-watch-for-error-prompt)
  (save-excursion
    (set-buffer
     (make-comint
      mumps-shell-process-name
      mumps-shell-cmd
      nil))
    (let ((proc (get-process mumps-shell-process-name)))
      (set-process-sentinel proc 'mumps-shell-sentinel)
      ;(process-kill-without-query proc)
      (setq comint-prompt-regexp "^.*>$")
      (setq default-directory (mumps-shell-get-client-file-directory))
      ;(mumps-shell-define-common-keys mumps-shell-map)
      (setq mode-line-format mumps-shell-mode-line-format)
      (use-local-map mumps-shell-map)
      (run-hooks 'mumps-shell-mode-hook)
      (while (zerop (buffer-size))
	(sleep-for 1)))))

;; temporary function to switch setting for different env
;;;###autoload
(defun mumps-shell-set-fnd ()
  (interactive)
  (setq mumps-shell-cmd "fnd")
  (setq mumps-shell-server "epic-fnd")
  (setq mumps-shell-default-dir "DEV")
)

;;;###autoload
(defun mumps-shell-set-cde ()
  (interactive)
  (setq mumps-shell-cmd "cde")
  (setq mumps-shell-server "epic-cde")
  (setq mumps-shell-default-dir "EMP")
)


;(defun mumps-shell-start-process (&optional env)
;  "Spawn a new M session in enviornment env."
;  (interactive)
;  (if (null env) (setq env "sh"))
;  (let* ((mumps-shell-prog env)
;	 (process-name "mumps-shell")
;	 (buffer-name "mumps-shell")
;	 ;; Without the following binding, ange-ftp-start-process
;	 ;; recurses on file-accessible-directory-p, since it needs to
;	 ;; restart its process in order to determine anything about
;	 ;; default-directory.
;	 ;(file-name-handler-alist)
;	 ;(default-directory
;	 ;  (if (file-accessible-directory-p default-directory)
;	 ;      default-directory
;	 ;    exec-directory))
;	 proc)
;    ;; It would be nice to make process-connection-type nil,
;    ;; but that doesn't work: ftp never responds.
;    ;; Can anyone find a fix for that?
;    (let ((process-connection-type t)
;	  (process-environment process-environment)
;	  (buffer (get-buffer-create buffer-name)))
;      (save-excursion
;	(set-buffer buffer)
;	(internal-mumps-shell-mode)
;	)
;      ;; This tells GNU ftp not to output any fancy escape sequences.
;      ;(setenv "TERM" "dumb")
;      (setq proc (apply 'start-process process-name buffer-name (list mumps-shell-prog))))
;    (save-excursion
;      (set-buffer (process-buffer proc))
;      (goto-char (point-max))
;      (set-marker (process-mark proc) (point))
;      )
;    (process-kill-without-query proc)
;    (set-process-sentinel proc (function mumps-shell-sentinel))
;    ;(accept-process-output proc)	;wait for ftp startup message
;    proc))


;(put 'internal-mumps-shell-mode 'mode-class 'special)
;(defun internal-mumps-shell-mode ()
;  "Major mode for interacting with the M session."
;  (interactive)
;  (comint-mode)
;  (setq major-mode 'internal-mumps-shell-mode)
;  (setq mode-name "Internal Mumps-shell")
;  (let ((proc (get-buffer-process (current-buffer))))
;    (make-local-variable 'mumps-shell-process-string)
;    (setq mumps-shell-process-string "")
;    (make-local-variable 'mumps-shell-process-busy)
;    (make-local-variable 'mumps-shell-process-result)
;    (make-local-variable 'mumps-shell-process-result-line)
;    (make-local-variable 'mumps-shell-process-msg)
;    (setq mumps-shell-process-result-line "")
;    (setq comint-prompt-regexp "^bash\\$ ")
;    (make-local-variable 'comint-password-prompt-regexp)
;    ;; This is a regexp that can't match anything.
;    (setq comint-password-prompt-regexp "^a\\'z")
;    (make-local-variable 'paragraph-start)
;    (setq paragraph-start comint-prompt-regexp)))

;;;###autoload
(defun mumps-shell ()
  "Run an inferior mumps-shell, with I/O through buffer *mumps-shell/ENV*.
If buffer exists but process is not running, make new one.
If buffer exists and process running, just switch to buffer `*mumps-shell/ENV*'."
  (interactive)
  (if (not (comint-check-proc (concat "*" mumps-shell-process-name "*")))
      (mumps-shell-start-process)
    ;(pop-to-buffer (process-buffer (get-process mumps-shell-process-name)))
    (mumps-shell-display-shell))
  )

(defun mumps-shell-send-command (command &optional file)
  "Send COMMAND to Mumps shell process, substituting optional FILE for *.
If COMMAND has no *, FILE will be appended, preceded by a blank, to COMMAND.
If FILE is nil, no substitution will be made in COMMAND.  COMMAND can be any
expression that evaluates to a command string."
  ;(save-excursion
    (let* ((cmd (eval command))
	   ;(proc (or (get-process mumps-shell-process-name) (error "No M subprocess")))
	   (proc (mumps-shell-check-process))
	   (buf (process-buffer proc))
           (star (string-match "\\*" cmd))
	   (string
	    (concat
	     (if file
		 (if star (concat (substring cmd 0 star)
				  file (substring cmd (1+ star)))
		   (concat cmd " " file))
	       cmd)
	     )))
      ;; Switch to buffer before checking for subproc output in it.
      (set-buffer buf)
      ;; If text is unchanged since previous tex-send-command,
      ;; we haven't got any output.  So wait for output now.
      ;(if (= (buffer-modified-tick buf) tex-send-command-modified-tick)
	;  (accept-process-output proc))
      (goto-char (point-max))
      (move-marker comint-last-input-start (point))
      (insert string)
      (comint-send-input)
      (set-marker (process-mark proc) (point))
      ;(setq tex-send-command-modified-tick (buffer-modified-tick buf))
      (mumps-shell-display-shell)
      )
    ;)
  )

(defun mumps-shell-display-shell (&optional select)
  "Make the Mumps shell buffer visible in a window.
Interactively with a prefix arg, of if select is t, the mumps shell buffer
will be selected as well."
  (interactive "P")
  (let (process)
    (setq process (mumps-shell-check-process))
    (if process
	(if select
	    (progn (switch-to-buffer-other-window (process-buffer process))
		   (mumps-shell-recenter-output-buffer nil))
	  (progn (display-buffer (process-buffer process))
		 (mumps-shell-recenter-output-buffer nil)))
      (error "No mumps-shell process found"))))

(defun mumps-shell-recenter-output-buffer (linenum)
  "Redisplay buffer of M process output so that most recent output can be seen.
The last line of the buffer is displayed on
line linenum of the window, or centered if linenum is nil."
  (interactive "P")
  (let* ((proc (or (get-process mumps-shell-process-name) (error "No M process")))
	 (m-shell (process-buffer proc))
	 (window))
    (if (null m-shell)
	(error "No M output buffer")
      (setq window (display-buffer m-shell))
      (save-selected-window
	(select-window window)
	(bury-buffer m-shell)
	(goto-char (point-max))
	(recenter (if linenum
		      (prefix-numeric-value linenum)
		    (/ (window-height) 2)))))))



(defun mumps-shell-kill-m-process ()
  "Kill the M process."
  ;TODO: maybe just need to kill the m-process buffer?
  (interactive)
  (message "TODO: mumps-shell-kill-m-process"))

(defun mumps-shell-kill-m-process-job ()
  "Kill the current job in M process using ^C."
  ;TODO
  (interactive)
  (message "TODO: mumps-shell-kill-m-process-job"))


(defun add-quote (str)
  "Wrapp double quotes on str."
  (let ((dquote "\"")
	(qstr))
    (if (numberp str) (setq str (number-to-string str)))
    (setq qstr (concat dquote str dquote))))

(defun return-itself (str)
  "A dummy function to return the argument itself"
  str)

(defun mumps-shell-form-args (argslist)
  "Form a string of arguments from a list, each element in the list
will be quoted and separeated with \",\""
  (let (rst)
    (setq rst (mapconcat 'add-quote argslist ","))))

(defun mumps-shell-form-string-args (strlist)
  "Form a string arguments from a list of string"
  (let (rst)
    (setq rst (mapconcat 'return-itself strlist " "))))
   
(defun mumps-shell-form-do-tag (fun argstring)
  "Form a string for execute M routine tag ""d tag^routine(args)""."
  (let ((cmd))
    (setq cmd (concat "d " fun "(" argstring ")"))))

(defun mumps-shell-form-write-tag (fun argstring)
  "Form a string for execute a M function tag ""w !,$$tag^routine(args)""."
  (let ((cmd))
    (setq cmd (concat "w !," fun "(" argstring ")"))))

(defun mumps-shell-save-routine (routine &optional ext file path tail slock canCreate)
  "Save a M routine into a plain file. Args: routine ext file path tail slock"
  ;(interactive)
  ;(if (null ext) (setq ext mumps-shell-working-env))
  (let (argslist string)
    (setq argslist (list routine ext file path tail slock canCreate))
    (setq string (mumps-shell-form-do-tag mumps-shell-routine-save-tag (mumps-shell-form-args argslist)))
    (mumps-shell-send-command string)))

;(defun mumps-shell-load-routine (file &optional ext path tail)
(defun mumps-shell-load-routine (file &optional slock)
  "Load a M routine plain file into M session. Args: file ext path tail"
  ;(interactive)
  ;(if (null ext) (setq ext mumps-shell-working-env))
  (let (arglist string)
    (setq arglist (list file slock))
    (setq string (mumps-shell-form-do-tag mumps-shell-routine-load-tag (mumps-shell-form-args arglist)))
    (mumps-shell-send-command string)))

(defun mumps-shell-checkin-routine-with-version-lock (routine &optional ext)
  "Save a M routine into a plain file with a M session lock and visit the 
file.  Then using version control and check it out (ci on server). 
With a prefix argument, it will prompt for an extension name. 
The routine is saved in `path/routine.mumps.ext'.  The default ext will be 
the working logical directory in M, value of mumps-shell-working-dir, 
e.g., `PDAR'."
  (interactive "sCheckout routine (with lock): \nP")
  (let ((defext (downcase mumps-shell-working-dir))
        filename oscmd cioptions
        )
    (if (null defext) (setq defext "tmp"))
    (if current-prefix-arg
	(setq ext (read-string (concat "Extension: [" defext "] "))))
    (if (null ext) (setq ext defext))
    (if (string= ext "") (setq ext defext))
    (mumps-shell-save-routine
       routine ext nil (mumps-shell-get-server-file-directory) nil "1" "2")
    (sleep-for 2) ;wait for previous job done, should find better way to do this.
    (setq filename (concat (mumps-shell-get-server-file-directory) routine ".mumps." ext))
    (setq cioptions (concat "-l -t-DLG" ext " -nDLG" ext))
    (setq oscmd (mumps-shell-form-string-args (list "ci" cioptions filename)))
    (mumps-shell-OS-cmd oscmd)
    (find-file (concat (mumps-shell-get-client-file-directory) routine ".mumps." ext))
    (message "Routine %s checkouted with lock" filename)
    ))

(defun mumps-shell-find-routine (routine &optional ext)
  "Save a M routine into a plain file with a M session lock and visit the 
file.  With a prefix argument, it will prompt for an extension name. 
The routine is saved in `path/routine.mumps.ext'.  The default ext will be 
the working logical directory in M, value of mumps-shell-working-dir, 
e.g., `PDAR'."
  (interactive "sRoutine: \nP")
  (let ((defext (downcase mumps-shell-working-dir)))
    (if (null defext) (setq defext "tmp"))
    (if current-prefix-arg
	(setq ext (read-string (concat "Extension: [" defext "] "))))
    (if (null ext) (setq ext defext))
    (if (string= ext "") (setq ext defext))
    (mumps-shell-save-routine
       routine ext nil (mumps-shell-get-server-file-directory) nil "1" "2")
    (sleep-for 2) ;wait for previous job done, should find better way to do this.
    (find-file (concat (mumps-shell-get-client-file-directory) routine ".mumps." ext))))

(defun mumps-shell-find-routine-read-only (routine &optional ext)
  "Save a M routine into a plain file without a M session lock and visit the 
file read only.  With a prefix argument, it will prompt for an extension name. 
The routine is saved in `path/routine.mumps.ext'.  The default ext will be 
the working logical directory in M, value of mumps-shell-working-dir, 
e.g., `PDAR'."
  (interactive "sRoutine: \nP")
  (let ((defext (downcase mumps-shell-working-dir)))
    (if (null defext) (setq defext "tmp"))
    (if current-prefix-arg
	(setq ext (read-string (concat "Extension: [" defext "] "))))
    (if (null ext) (setq ext defext))
    (if (string= ext "") (setq ext defext))
    (mumps-shell-save-routine
       routine ext nil (mumps-shell-get-server-file-directory) nil nil)
    (sleep-for 2) ;wait for previous job done, should find better way to do this.
    (find-file-read-only (concat (mumps-shell-get-client-file-directory) routine ".mumps." ext))))

;*CW 12/20/2002 if chaning the default-directory by `cd' works, I shall
;not need this function anymore? Because I can use find-file directly and
;even better it is a real find-file with file matching!?
(defun mumps-shell-find-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME from the remote server running mumps.
Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  Wildcard expansion
can be suppressed by setting `fild-file-widcards'.  See `find-file' for more detail."
  (interactive "sRemote File: \np")
    (find-file (concat (mumps-shell-get-client-file-directory) filename)))

;;To-do: prompt for save file first if there is unsaved change for that file?
(defun mumps-shell-load-file (fileName &optional rellock)
  "Load a M plain file back to M environment. If rellock is 1 or there is a
prefix argument, the M session lock will be released (decrement by 1)."
  (interactive "sFile: \nP")
  (if current-prefix-arg
      (setq rellock "1") (setq rellock "0"))
  (mumps-shell-load-routine (mumps-shell-get-server-file-fullname fileName) rellock)
  (message "Routine save back to M env %s directory %s (rellock=%s)" mumps-shell-working-env mumps-shell-working-dir rellock))


(defun mumps-shell-routine-lock (routine)
  "Try to obtain a M session lock the routine."
  (interactive "sRoutine: ")
  (let (arglist string)
    (setq arglist (list routine))
    (setq string (mumps-shell-form-do-tag mumps-shell-lock-rou-tag (mumps-shell-form-args arglist)))
    (mumps-shell-send-command string)
    (message "Try to get lock for routine %s in M env %s directory %s." routine  mumps-shell-working-env mumps-shell-working-dir)))

(defun mumps-shell-routine-unlock (routine)
  "Try to release lock the routine."
  (interactive "sRoutine: ")
  (let (arglist string)
    (setq arglist (list routine))
    (setq string (mumps-shell-form-do-tag mumps-shell-release-lock-rou-tag (mumps-shell-form-args arglist)))
    (mumps-shell-send-command string)
    (message "Try to release for routine %s in M env %s directory %s." routine mumps-shell-working-env mumps-shell-working-dir)))

(defun mumps-shell-change-env (env)
  "Change the mumps-shell working environment."
  (interactive "sEnvironment: ")
  (let (arglist string)
    (if (not (null env))
	(progn
	  (setq env (upcase env))
	  (setq arglist (list env))
	  (setq string (mumps-shell-form-do-tag mumps-shell-change-env-tag (mumps-shell-form-args arglist)))
	  (mumps-shell-send-command string)
	  (setq mumps-shell-working-env env)
	  (setq mumps-shell-working-dir (concat "MGR_" env)))))
  )

(defun mumps-shell-change-dir (dir)
  "Change the mumps-shell working directory."
  (interactive "sDirectory: ")
  (let (arglist string)
    (if (not (null dir))
	(progn
	  (setq dir (upcase dir))
	  (setq arglist (list dir))
	  (setq string (mumps-shell-form-do-tag mumps-shell-change-dir-tag (mumps-shell-form-args arglist)))
	  (mumps-shell-send-command string)
	  (setq mumps-shell-working-dir dir))))
  )

(defun mumps-shell-change-envdir (env dir)
  "Change the mumps-shell working environment and directory."
  (interactive "sEnvironment: \nsDirectory: ")
  (let (arglist string)
    (if (not (and (null env) (null dir)))
	(progn
	  (setq env (upcase env))
	  (setq dir (upcase dir))
	  (setq arglist (list env dir))
	  (setq string (mumps-shell-form-do-tag mumps-shell-change-envdir-tag (mumps-shell-form-args arglist)))
	  (mumps-shell-send-command string)
	  (setq mumps-shell-working-env env)
	  (setq mumps-shell-working-dir dir))))
  )

(defun mumps-shell-change-dev (dev)
  "Change the mumps-shell working development directory (PDAR,IDE,SDE)."
  (interactive "sDevelopment directory[PDAR/IDE/SDE]: ")
  (setq dev (upcase dev))
  (cond ((string-equal dev "PDAR") (mumps-shell-change-envdir "RES" "PDAR"))
	((string-equal dev "IDE") (mumps-shell-change-envdir "IDE" "IDE"))
	((string-equal dev "SDE") (mumps-shell-change-envdir "SDE" "SDE"))
	(t nil))
)

(defun mumps-shell-repaint-minibuffer ()
  "Clear any existing minibuffer message; let the minibuffer contents show."
  (message nil))

(defun mumps-shell-read-minibuffer (prompt &optional default hide)
  "Read from minibuffer with a prompt. If nothing is entered (just return) use default value. If hide is t, echoing `.' for each character typed.
End with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.
Copied and modified from ange-ftp."
  (let ((pass nil)
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t))
    (while (progn (if hide
		      (message "%s%s" prompt (make-string (length pass) ?.))
		    (if default
			(message "%s[%s] %s" prompt default (if (null pass) "" pass))
		      (message "%s%s" prompt (if (null pass) "" pass))))
		  (setq c (read-char))
		  (and (/= c ?\r) (/= c ?\n) (/= c ?\e)))
      (if (= c ?\C-u)
	  (setq pass "")
	(if (and (/= c ?\b) (/= c ?\177))
	    (setq pass (concat pass (char-to-string c)))
	  (if (> (length pass) 0)
	      (setq pass (substring pass 0 -1))))))
    (message "")
    (mumps-shell-repaint-minibuffer)
    (or pass default "")))


(defun mumps-shell-watch-for-user-prompt (string)
  "Prompt in the minibuffer for User ID. This is used in the list `comint-output-filter-functions'."
  (if (string-match mumps-shell-user-prompt-regexp string)
      ;(send-invisible nil)
      (send-invisible (mumps-shell-read-minibuffer "User ID: "))
    ))

(defun mumps-shell-watch-for-passwd-prompt (string)
  "Prompt in the minibuffer for User ID. This is used in the list `comint-output-filter-functions'."
  (if (string-match mumps-shell-password-prompt-regexp string)
      ;(send-invisible nil)
      (send-invisible (mumps-shell-read-minibuffer "User Code: " nil t))
    ))

(defun mumps-shell-watch-for-directory-prompt (string)
  "Prompt in the minibuffer for directory. This is used in the list `comint-output-filter-functions'."
  (if (string-match mumps-shell-directory-prompt-regexp string)
      (progn
	;(send-invisible nil)
	(setq mumps-shell-working-dir (mumps-shell-read-minibuffer "Directory: " mumps-shell-default-dir nil))
	(send-invisible mumps-shell-working-dir)))
  )

(defun mumps-shell-watch-for-error-prompt (string)
  "Display an error message in the minibuffer while error detected. This is used in the list `comint-output-filter-functions'."
  (if (string-match mumps-shell-error-prompt-regexp string)
      ;(send-invisible nil)
      (message "Error (%s): %s" mumps-shell-process-name string)
    ))

(defun mumps-shell-check-process ()
  "Check if a mumps-shell exists and running. The process is the returned value."
  (interactive)
  (let (process err)
    (if mumps-shell-process-name
	(setq process (get-process mumps-shell-process-name))
      (error "Variable mumps-shell-process-name is not defined"))
    (if (null process)
	(error "Mumps-shell process not found"))
    process))
  
(defun mumps-shell-checksum (object)
  "Display checksum for the current mumps code in the current env in mumps-shell."
  (interactive "sObject: ")
  (let (arglist string nodetail)
    (if current-prefix-arg
	(setq nodetail nil)
      (setq nodetail "1"))
    (setq arglist (list object nodetail))
    (setq string (mumps-shell-form-write-tag mumps-shell-routine-checksum-tag (mumps-shell-form-args arglist)))
    (mumps-shell-send-command string)))


(defun mumps-shell-show-lock-count (&optional node)
  "Display the all lock table entries containing node.
You need chengwei's [592] lookitt macro `lts' to use this feature in mumps-shell."
  (interactive "sLock entry node: ")
  (if (not (stringp node))
      (message "The lock entry node must be a string")
    (progn
      (mumps-shell-lookitt-macro (concat "lts " node)))))

(defun mumps-shell-lookitt-macro (&optional macrocmdstr)
  "Run a lookitt macro command."
  (interactive "sLookitt macro string: ")
  (let (string)
    (if (not (stringp macrocmdstr))
        (message "The argument must be a string")
      (progn
        (setq string (mumps-shell-form-do-tag
                      mumps-shell-lookitt-macro-tag
                      (mumps-shell-form-args (list macrocmdstr))))
        (mumps-shell-send-command string)))))
              

(defun mumps-shell-OS-cmd (&optional cmd)
  "Run an OS command.  For example, a unix shell command"
  (interactive "sOS command: ")
  (let (string)
    ;(if (null cmd)
    ;    (setq cmd (read-string (concat "OS cmd: "))))
    (if (not (stringp cmd))
        (message "The argument must be a string")
      (progn
	(setq string (mumps-shell-form-do-tag 
		      mumps-shell-OS-cmd-tag 
		      (mumps-shell-form-args (list cmd))))
	(mumps-shell-send-command string)))))

	  
;; These two are provided for mumps-mode
(defun mumps-shell-lock-routine (filename)
  "Try to lock the routine of the filename.  
The filename is the full file name on the local client that is 
running emacs/mumps-shell."
  (let (routine)
    (setq routine (mumps-shell-get-server-file-fullname filename))
    (mumps-shell-routine-lock routine)))

(defun mumps-shell-release-lock-routine (filename)
  "Try to release lock the routine of the filename.  
The filename name is the full file name on the local client that is
running emacs/mumps-shell."
  (let (routine)
    (setq routine (mumps-shell-get-server-file-fullname filename))
    (mumps-shell-routine-unlock routine)))


(provide 'mumps-shell)

;;
;;EOF mumps-shell.el
