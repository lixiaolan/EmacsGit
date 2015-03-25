;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; ;; 
;; ;; LorikeeM MUMPS Developer Tools 
;; ;; 
;; ;; Provides syntax highlighting and global dump for GNU Emacs 
;; ;; 
;; ;; Note that I have concentrated mostly on GT.M syntax here. the 
;; ;; list of keywords and functions comes from Edition 6.1 of  
;; ;; Jon Diamond's "Standard M Pocket Guide." I have not left out 
;; ;; any InterSystems Cache-specific syntax, but have not tested 
;; ;; its use on any Cache-specific code. 
;; ;; 
;; ;; Written by John Willis 
;; ;; john@coherent-logic.com 
;; ;; 
;; ;; Copyright (C) 2010 Coherent Logic Development LLC 
;; ;; 
;; ;; This file is part of LorikeeM.  
;; ;; 
;; ;; LorikeeM is free software: you can redistribute it and/or modify 
;; ;; it under the terms of the GNU Affero General Public License as published by 
;; ;; the Free Software Foundation, either version 3 of the License, or 
;; ;; (at your option) any later version. 
;; ;; 
;; ;; LorikeeM is distributed in the hope that it will be useful, 
;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;; ;; GNU General Public License for more details. 
;; ;; 
;; ;; You should have received a copy of the GNU Affero General Public License 
;; ;; along with LorikeeM.  If not, see <http://www.gnu.org/licenses/>. 
;; ;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 
;; (provide 'mumps-mode)  
 
;; ;; hooks for run before mode run 
;; (defvar mumps-mode-hook nil) 
 
 
;; ;; keywords for syntax highlighting 
;; (defvar mumps-keywords-ucase-abbrev 
;;   '("B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "TC" "TRE" "TRO" "TS" "U" "V" "W" "X" "ZA" "ZB" "ZD" "ZH" "ZHO" "ZI" "ZK" "ZL" "ZN" "ZP" "ZQ" "ZR" "ZS" "ZSY" "ZTC" "ZT" "ZTS" "ZU" "ZW") 
;;   "MUMPS uppercase abbreviated keywords") 
 
;; (defvar mumps-keywords-lcase-abbrev 
;;   '("b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "tc" "tre" "tro" "ts" "u" "v" "w" "x" "za" "zb" "zd" "zh" "zho" "zi" "zk" "zl" "zn" "zp" "zq" "zr" "zs" "zsy" "ztc" "zt" "zts" "zu" "zw") 
;;   "MUMPS lowercase abbreviated keywords") 
 
;; (defvar mumps-keywords-ucase-full 
;;   '("BREAK" "CLOSE" "CONTINUE" "DO" "ELSE" "ELSEIF" "FOR" "GOTO" "HALT" "HANG" "IF" "JOB" "KILL" "LOCK" "MERGE" "NEW" "OPEN" "PRINT" "QUIT" "READ" "SET" "TCOMMIT" "TRESTART" "TROLLBACK" "TSTART" "USE" "VIEW" "WHILE" "WRITE" "XECUTE" "ZALLOCATE" "ZBREAK" "ZDEALLOCATE" "ZHANG" "ZHOROLOG" "ZINSERT" "ZKILL" "ZLOAD" "ZNSPACE" "ZPRINT" "ZQUIT" "ZREMOVE" "ZSAVE" "ZSYNC" "ZSYSTEM" "ZTCOMMIT" "ZTRAP" "ZTSTART" "ZUSE" "ZWITHDRAW" "ZWRITE" "ZZDUMP") 
;;   "MUMPS uppercase full-length keywords") 
 
;; (defvar mumps-keywords-lcase-full 
;;   '("break" "close" "continue" "do" "else" "elseif" "for" "goto" "halt" "hang" "if" "job" "kill" "lock" "merge" "new" "open" "print" "quit" "read" "set" "tcommit" "trestart" "trollback" "tstart" "use" "view" "while" "write" "xecute" "zallocate" "zbreak" "zdeallocate" "zhang" "zhorolog" "zinsert" "zkill" "zload" "znspace" "zprint" "zquit" "zremove" "zsave" "zsync" "zsystem" "ztcommit" "ztrap" "ztstart" "zuse" "zwithdraw" "zwrite" "zzdump") 
;;   "MUMPS lowercase full-length keywords") 
 
;; (defvar mumps-functions-ucase-abbrev 
;;   '("$A" "$C" "$D" "$E" "$F" "$FN" "$G" "$IN" "$I" "$J" "$L" "$LI" "$LB" "$LD" "$LF" "$LFS" "$LG" "$LL" "$LS" "$LTS" "$NA" "$N" "$NUM" "$O" "$P" "$Q" "$QL" "$QS" "$R" "$S" "$ST" "$T" "$TR" "$V" "$ZBA" "$ZBC" "$ZBF" "$ZBG" "$ZBL" "$ZBN" "$ZB" "$ZBSE" "$ZBST" "$ZBX" "$ZCVT" "$ZC" "$ZD" "$ZDH" "$ZDT" "$ZDTH" "$ZDEV" "$ZI" "$ZO" "$ZP" "$ZSE" "$ZSO" "$ZT" "$ZTH" "$ZTL" "$ZU") 
;;   "MUMPS uppercase abbreviated functions") 
 
;; (defvar mumps-functions-lcase-abbrev 
;;   '("$a" "$c" "$d" "$e" "$f" "$fn" "$g" "$in" "$i" "$j" "$l" "$li" "$lb" "$ld" "$lf" "$lfs" "$lg" "$ll" "$ls" "$lts" "$na" "$n" "$num" "$o" "$p" "$q" "$ql" "$qs" "$r" "$s" "$st" "$t" "$tr" "$v" "$zba" "$zbc" "$zbf" "$zbg" "$zbl" "$zbn" "$zb" "$zbse" "$zbst" "$zbx" "$zcvt" "$zc" "$zd" "$zdh" "$zdt" "$zdth" "$zdev" "$zi" "$zo" "$zp" "$zse" "$zso" "$zt" "$zth" "$ztl" "$zu") 
;;   "MUMPS lowercase abbreviated functions") 
 
;; (defvar mumps-functions-ucase-full 
;;   '("$ASCII" "$BIT" "$BITCOUNT" "$BITFIND" "$BITLOGIC" "$CASE" "$CHAR" "$DATA" "$EXTRACT" "$FACTOR" "$FIND" "$FNUMBER" "$GET" "$INCREMENT" "$INUMBER" "$ISOBJECT" "$ISVALIDNUM" "$JUSTIFY" "$LENGTH" "$LIST" "$LISTBUILD" "$LB" "$LISTDATA" "$LISTFIND" "$LISTFROMSTRING" "$LISTGET" "$LISTLENGTH" "$LISTNEXT" "$LISTSAME" "$LISTTOSTRING" "$NAME" "$NEXT" "$NORMALIZE" "$NUMBER" "$ORDER" "$PIECE" "$QLENGTH" "$QSUBSCRIPT" "$QUERY" "$RANDOM" "$REVERSE" "$SELECT" "$SORTBEGIN" "$SORTEND" "$STACK" "$SYSTEM" "$TEXT" "$TRANSLATE" "$VIEW" "$ZABS" "$ZARCCOS" "$ZARCSIN" "$ZARCTAN" "$ZBAND" "$ZBCOUNT" "$ZBFIND" "$ZBGET" "$ZBIT" "$ZZBITAND" "$ZBITCOUNT" "$ZBITFIND" "$ZBITGET" "$ZBITLEN" "$ZBITNOT" "$ZBITOR" "$ZBITSET" "$ZBITSTR" "$ZBITXOR" "$ZBLEN" "$ZBNOT" "$ZBOOLEAN" "$ZBOR" "$ZBSET" "$ZBSTR" "$ZBXOR" "$ZCONVERT" "$ZCVT" "$ZCOS" "$ZCOT" "$ZCRC" "$ZCSC" "$ZCYC" "$ZDATE" "$ZDATEH" "$ZDATETIME" "$ZDATETIMEH" "$ZDEVICE" "$ZEXP" "$ZF" "$ZHEX" "$ZINCREMENT" "$ZINFO" "$ZLN" "$ZLOG" "$ZMESSAGE" "$ZNAME" "$ZNEXT" "$ZOBJCLASSMETHOD" "$ZOBJPROPERTY" "$ZORDER" "$ZPARSE" "$ZPOWER" "$ZPREVIOUS" "$ZSEARCH" "$ZSEC" "$ZSEEK" "$ZSIN" "$ZSOCKET" "$ZSORT" "$ZSQR" "$ZSTRIP" "$ZTAN" "$ZTEXP" "$ZTIME" "$ZTIMEH" "$ZTLOG" "$ZTRNLMN" "$ZUCI") 
;;   "MUMPS uppercase full-length functions") 
 
;; (defvar mumps-functions-lcase-full 
;;     '("$ascii" "$bit" "$bitcount" "$bitfind" "$bitlogic" "$case" "$char" "$data" "$extract" "$factor" "$find" "$fnumber" "$get" "$increment" "$inumber" "$isobject" "$isvalidnum" "$justify" "$length" "$list" "$listbuild" "$lb" "$listdata" "$listfind" "$listfromstring" "$listget" "$listlength" "$listnext" "$listsame" "$listtostring" "$name" "$next" "$normalize" "$number" "$order" "$piece" "$qlength" "$qsubscript" "$query" "$random" "$reverse" "$select" "$sortbegin" "$sortend" "$stack" "$system" "$text" "$translate" "$view" "$zabs" "$zarccos" "$zarcsin" "$zarctan" "$zband" "$zbcount" "$zbfind" "$zbget" "$zbit" "$zzbitand" "$zbitcount" "$zbitfind" "$zbitget" "$zbitlen" "$zbitnot" "$zbitor" "$zbitset" "$zbitstr" "$zbitxor" "$zblen" "$zbnot" "$zboolean" "$zbor" "$zbset" "$zbstr" "$zbxor" "$zconvert" "$zcvt" "$zcos" "$zcot" "$zcrc" "$zcsc" "$zcyc" "$zdate" "$zdateh" "$zdatetime" "$zdatetimeh" "$zdevice" "$zexp" "$zf" "$zhex" "$zincrement" "$zinfo" "$zln" "$zlog" "$zmessage" "$zname" "$znext" "$zobjclassmethod" "$zobjproperty" "$zorder" "$zparse" "$zpower" "$zprevious" "$zsearch" "$zsec" "$zseek" "$zsin" "$zsocket" "$zsort" "$zsqr" "$zstrip" "$ztan" "$ztexp" "$ztime" "$ztimeh" "$ztlog" "$ztrnlmn" "$zuci") 
;;     "MUMPS lowercase full-length functions") 
 
;; ;; define keywords for completion 
;; (defvar mumps-keywords 
;;   '("BREAK" "CLOSE" "CONTINUE" "DO" "ELSE" "ELSEIF" "FOR" "GOTO" "HALT" "HANG" "IF" "JOB" "KILL" "LOCK" "MERGE" "NEW" "OPEN" "PRINT" "QUIT" "READ" "SET" "TCOMMIT" "TRESTART" "TROLLBACK" "TSTART" "USE" "VIEW" "WHILE" "WRITE" "XECUTE" "ZALLOCATE" "ZBREAK" "ZDEALLOCATE" "ZHANG" "ZHOROLOG" "ZINSERT" "ZKILL" "ZLOAD" "ZNSPACE" "ZPRINT" "ZQUIT" "ZREMOVE" "ZSAVE" "ZSYNC" "ZSYSTEM" "ZTCOMMIT" "ZTRAP" "ZTSTART" "ZUSE" "ZWITHDRAW" "ZWRITE" "ZZDUMP" "break" "close" "continue" "do" "else" "elseif" "for" "goto" "halt" "hang" "if" "job" "kill" "lock" "merge" "new" "open" "print" "quit" "read" "set" "tcommit" "trestart" "trollback" "tstart" "use" "view" "while" "write" "xecute" "zallocate" "zbreak" "zdeallocate" "zhang" "zhorolog" "zinsert" "zkill" "zload" "znspace" "zprint" "zquit" "zremove" "zsave" "zsync" "zsystem" "ztcommit" "ztrap" "ztstart" "zuse" "zwithdraw" "zwrite" "zzdump" "$ASCII" "$BIT" "$BITCOUNT" "$BITFIND" "$BITLOGIC" "$CASE" "$CHAR" "$DATA" "$EXTRACT" "$FACTOR" "$FIND" "$FNUMBER" "$GET" "$INCREMENT" "$INUMBER" "$ISOBJECT" "$ISVALIDNUM" "$JUSTIFY" "$LENGTH" "$LIST" "$LISTBUILD" "$LB" "$LISTDATA" "$LISTFIND" "$LISTFROMSTRING" "$LISTGET" "$LISTLENGTH" "$LISTNEXT" "$LISTSAME" "$LISTTOSTRING" "$NAME" "$NEXT" "$NORMALIZE" "$NUMBER" "$ORDER" "$PIECE" "$QLENGTH" "$QSUBSCRIPT" "$QUERY" "$RANDOM" "$REVERSE" "$SELECT" "$SORTBEGIN" "$SORTEND" "$STACK" "$SYSTEM" "$TEXT" "$TRANSLATE" "$VIEW" "$ZABS" "$ZARCCOS" "$ZARCSIN" "$ZARCTAN" "$ZBAND" "$ZBCOUNT" "$ZBFIND" "$ZBGET" "$ZBIT" "$ZZBITAND" "$ZBITCOUNT" "$ZBITFIND" "$ZBITGET" "$ZBITLEN" "$ZBITNOT" "$ZBITOR" "$ZBITSET" "$ZBITSTR" "$ZBITXOR" "$ZBLEN" "$ZBNOT" "$ZBOOLEAN" "$ZBOR" "$ZBSET" "$ZBSTR" "$ZBXOR" "$ZCONVERT" "$ZCVT" "$ZCOS" "$ZCOT" "$ZCRC" "$ZCSC" "$ZCYC" "$ZDATE" "$ZDATEH" "$ZDATETIME" "$ZDATETIMEH" "$ZDEVICE" "$ZEXP" "$ZF" "$ZHEX" "$ZINCREMENT" "$ZINFO" "$ZLN" "$ZLOG" "$ZMESSAGE" "$ZNAME" "$ZNEXT" "$ZOBJCLASSMETHOD" "$ZOBJPROPERTY" "$ZORDER" "$ZPARSE" "$ZPOWER" "$ZPREVIOUS" "$ZSEARCH" "$ZSEC" "$ZSEEK" "$ZSIN" "$ZSOCKET" "$ZSORT" "$ZSQR" "$ZSTRIP" "$ZTAN" "$ZTEXP" "$ZTIME" "$ZTIMEH" "$ZTLOG" "$ZTRNLMN" "$ZUCI" "$ascii" "$bit" "$bitcount" "$bitfind" "$bitlogic" "$case" "$char" "$data" "$extract" "$factor" "$find" "$fnumber" "$get" "$increment" "$inumber" "$isobject" "$isvalidnum" "$justify" "$length" "$list" "$listbuild" "$lb" "$listdata" "$listfind" "$listfromstring" "$listget" "$listlength" "$listnext" "$listsame" "$listtostring" "$name" "$next" "$normalize" "$number" "$order" "$piece" "$qlength" "$qsubscript" "$query" "$random" "$reverse" "$select" "$sortbegin" "$sortend" "$stack" "$system" "$text" "$translate" "$view" "$zabs" "$zarccos" "$zarcsin" "$zarctan" "$zband" "$zbcount" "$zbfind" "$zbget" "$zbit" "$zzbitand" "$zbitcount" "$zbitfind" "$zbitget" "$zbitlen" "$zbitnot" "$zbitor" "$zbitset" "$zbitstr" "$zbitxor" "$zblen" "$zbnot" "$zboolean" "$zbor" "$zbset" "$zbstr" "$zbxor" "$zconvert" "$zcvt" "$zcos" "$zcot" "$zcrc" "$zcsc" "$zcyc" "$zdate" "$zdateh" "$zdatetime" "$zdatetimeh" "$zdevice" "$zexp" "$zf" "$zhex" "$zincrement" "$zinfo" "$zln" "$zlog" "$zmessage" "$zname" "$znext" "$zobjclassmethod" "$zobjproperty" "$zorder" "$zparse" "$zpower" "$zprevious" "$zsearch" "$zsec" "$zseek" "$zsin" "$zsocket" "$zsort" "$zsqr" "$zstrip" "$ztan" "$ztexp" "$ztime" "$ztimeh" "$ztlog" "$ztrnlmn" "$zuci") 
;;   "MUMPS keywords for completion") 
 
;; ;; build the regexps from the lists 
;; (defvar mkuf (regexp-opt mumps-keywords-ucase-full 'words)) 
;; (defvar mklf (regexp-opt mumps-keywords-lcase-full 'words)) 
;; (defvar mkua (regexp-opt mumps-keywords-ucase-abbrev 'words)) 
;; (defvar mkla (regexp-opt mumps-keywords-lcase-abbrev 'words)) 
;; (defvar mfuf (regexp-opt mumps-functions-ucase-full 'words)) 
;; (defvar mflf (regexp-opt mumps-functions-lcase-full 'words)) 
;; (defvar mfua (regexp-opt mumps-functions-ucase-abbrev 'words)) 
;; (defvar mfla (regexp-opt mumps-functions-lcase-abbrev 'words)) 
 
 
 
;; ;; clear un-needed memory resources 
;; (setq mumps-keywords-ucase-abbrev nil) 
;; (setq mumps-keywords-ucase-full nil) 
;; (setq mumps-keywords-lcase-abbrev nil) 
;; (setq mumps-keywords-lcase-full nil) 
;; (setq mumps-functions-lcase-full nil) 
;; (setq mumps-functions-ucase-full nil) 
;; (setq mumps-functions-ucase-abbrev nil) 
;; (setq mumps-functions-lcase-abbrev nil) 
 
 
;; ;; create the thingy that we'll feed to font-lock-defaults 
;; (setq mumps-font-lock-keywords  
;;       `(       
;;         (,mkua . font-lock-keyword-face) 
;;         (,mkuf . font-lock-keyword-face) 
;;         (,mkla . font-lock-keyword-face) 
;;         (,mklf . font-lock-keyword-face) 
;;         (,mfuf . font-lock-function-name-face) 
;;         (,mfua . font-lock-function-name-face) 
;;         (,mfla . font-lock-function-name-face) 
;;         (,mflf . font-lock-function-name-face) 
;;         (,";.*$" . font-lock-comment-face) 
;; )) 
 
;; ;; 
;; ;; get some specifics about the GT.M/FWSLC infrastructure configuration here 
;; ;; 
;; (defvar lkm-gtm-mupip (getenv "mupip")) 
;; (defvar lkm-gtm-repl-port (getenv "REPL_PORT")) 
;; (defvar lkm-gtm-log (getenv "gtm_log")) 
;; (defvar lkm-gtm-replication (getenv "GTM_REPLICATION")) 
;; (defvar lkm-gtm-replication-dat (getenv "REPL_DAT")) 
;; (defvar lkm-gtm-replication-host-ssh-host (getenv "REPL_HOST_SSH_HOST")) 
;; (defvar lkm-gtm-version (getenv "GTMVER")) 
;; (defvar lkm-gtm-lke (getenv "lke")) 
;; (defvar lkm-gtm-global-directory (getenv "gtmgbldir")) 
;; (defvar lkm-gtm-backup-host (getenv "BUP")) 
;; (defvar lkm-gtm-replication-journal (getenv "REPL_JNL")) 
;; (defvar lkm-gtm-replication-side (getenv "REPL_SIDE")) 
;; (defvar lkm-gtm-routines (getenv "gtmroutines")) 
;; (defvar lkm-gtm-replication-buffer-size (getenv "REPL_BUFSIZE")) 
;; (defvar lkm-gtm-database-instance-path (getenv "DBINST")) 
;; (defvar lkm-gtm-dse (getenv "dse")) 
;; (defvar lkm-gtm-replication-configuration (getenv "REPL_CONF")) 
;; (defvar lkm-gtm-gde (getenv "gde")) 
;; (defvar lkm-gtm-replication-host-ssh-port (getenv "REPL_HOST_SSH_PORT")) 
;; (defvar lkm-gtm-replication-host (getenv "REPL_HOST")) 
 
;; (defvar lkm-fwslc (file-exists-p "~/bin/set_env")) 
 
;; (defun lkm-about () 
;;   "About LorikeeM" 
;;   (interactive) 
   
;;   (setq lkm-about-buf "*About LorikeeM MUMPS Developer Tools*") 
;;   (get-buffer-create lkm-about-buf) 
;;   (with-current-buffer (get-buffer-create lkm-about-buf) 
;;     (goto-char (point-max)) 
;;     (insert "LorikeeM MUMPS Developer Tools\n\n") 
;;     (insert "GT.M Version                     :  " lkm-gtm-version "\n") 
;;     (insert "GT.M Database Instance Path      :  " lkm-gtm-database-instance-path "\n") 
;;     (insert "GT.M Global Directory Path       :  " lkm-gtm-global-directory "\n") 
;;     (insert "GT.M Routine Search Path         :  " lkm-gtm-routines "\n")     
;;     (insert "GT.M Replication Status          :  " lkm-gtm-replication "," (downcase lkm-gtm-replication-side) "\n")) 
;;   (switch-to-buffer-other-window lkm-about-buf)) 
 
;; (defun lkm-gtm-compile () 
;;   "Compiles the current buffer" 
;;   (interactive) 
 
;;   (setq file-name (buffer-file-name)) 
;;   (setq cmd-str (concat "mumps " file-name)) 
;;   (compile cmd-str nil)) 
 
;; (defun lkm-global-at-point () 
;;  "Look up the GT.M global at the word under the current point." 
 
;;   (interactive) 
;;   (let (mm-current-word) 
;;     (setq current-char (char-to-string (char-after))) 
;;     (if (string= current-char "^") (forward-char 1)) 
;;     (setq mm-current-word (thing-at-point 'word)) 
;;     (save-excursion 
;;       (setq prev-char (char-to-string (char-before))) 
;;       (if (not (string= prev-char "^")) (backward-word 1)) 
;;       (setq prev-char (char-to-string (char-before))) 
;;       (if (string= prev-char "^")  
;;         (lkm-gtm-global-lookup mm-current-word)  
;;         (message "%s does not appear to be a MUMPS global." mm-current-word)) 
;;       ) 
     
;;   ) 
;; ) 
 
;; (defun lkm-gtm-global-lookup (global) 
;;   "Look up a global in GT.M" 
;;   (interactive "sWhat global would you like to look up? ") 
   
;;   (get-buffer-create "*MUMPS Global Examiner*") 
;;   (with-current-buffer (get-buffer-create "*MUMPS Global Examiner*") 
;;     (goto-char (point-max)) 
;;     (insert "MUMPS Global Dump of " global "\n\n")) 
;;   (call-process "mumps" nil "*MUMPS Global Examiner*" nil "-r" "KBAWDUMP" global) 
;;   (display-buffer (get-buffer "*MUMPS Global Examiner*") t) 
;;   (with-current-buffer (get-buffer-create "*MUMPS Global Examiner*") 
;;     (goto-char (point-max)))) 
 
;; (defun lkm-jump-to-routine-def () 
;;   "Jump to the definition of the routine under the cursor." 
;;   (interactive) 
;;   (setq lkm-current-symb (thing-at-point 'symbol)) 
;;   (find-tag-other-window lkm-current-symb)) 
 
;; (defun lkm-complete-symbol () 
;;   "Perform keyword completion on word before cursor." 
;;   (interactive) 
;;   (let ((posEnd (point)) 
;;         (meat (thing-at-point 'symbol)) 
;;         maxMatchResult) 
 
;;     ;; when nil, set it to empty string, so user can see all lang's keywords. 
;;     ;; if not done, try-completion on nil result lisp error. 
;;     (when (not meat) (setq meat "")) 
;;     (setq maxMatchResult (try-completion meat mumps-keywords)) 
 
;;     (cond ((eq maxMatchResult t)) 
;;           ((null maxMatchResult) 
;;            (message "Can't find completion for “%s”" meat) 
;;            (ding)) 
;;           ((not (string= meat maxMatchResult)) 
;;            (delete-region (- posEnd (length meat)) posEnd) 
;;            (insert maxMatchResult)) 
;;           (t (message "Making completion list...") 
;;              (with-output-to-temp-buffer "*Completions*" 
;;                (display-completion-list  
;;                 (all-completions meat mumps-keywords) 
;;                 meat)) 
;;              (message "Making completion list...%s" "done"))))) 
 
 
 
;; (define-derived-mode mumps-mode fundamental-mode 
;;   "mumps mode" 
;;   "Major mode for MUMPS" 
 
;;   (setq lkm-version "0.99") 
;;   (if lkm-fwslc 
;;     (setq lkm-fw-msg ", with FWSLC tools") 
;;     (setq lkm-fw-msg ", without FWSLC tools")) 
;;   (message "LorikeeM MUMPS Developer Tools %s (GT.M %s%s)" lkm-version lkm-gtm-version lkm-fw-msg) 
 
;;   ;; 
;;   ;; set up syntax table entries 
;;   ;; 
   
;;   ;; syntax table entries for symbol constituents 
;;   ;;  we're adding ^ and % to accommodate MUMPS routine calls 
;;   (modify-syntax-entry ?^ "_") 
;;   (modify-syntax-entry ?% "_") 
;;   (modify-syntax-entry ?$ "-") 
 
;;   ;; 
;;   ;; modify the tags table list to point to our directory 
;;   ;; 
;;   (setq tags-file-name "~/.cld-elisp/TAGS") 
 
;;   ;; 
;;   ;; set up keyboard mappings 
;;   ;; 
;;   (global-set-key (kbd "<f6>") 'lkm-global-at-point) 
;;   (global-set-key (kbd "<f7>") 'lkm-gtm-global-lookup) 
;;   (global-set-key (kbd "<f8>") 'lkm-jump-to-routine-def) 
;;   (global-set-key (kbd "<f5>") 'lkm-complete-symbol) 
;;   (global-set-key (kbd "<f9>") 'lkm-gtm-compile) 
 
;;   ;; 
;;   ;; set up the MUMPS menu to be loaded after the Tools menu 
;;   ;; 
;;   (define-key-after 
;;     global-map 
;;     [menu-bar mumps-menu] 
;;     (cons "LorikeeM" (make-sparse-keymap "mumps")) 
;;     'tools ) 
;;   (define-key 
;;     global-map 
;;     [menu-bar mumps-menu gli] 
;;     '("Examine Global" . lkm-gtm-global-lookup)) 
;;   (define-key 
;;     global-map 
;;     [menu-bar mumps-menu gls] 
;;     '("Examine Global at Cursor" . lkm-global-at-point)) 
;;   (define-key 
;;     global-map 
;;     [menu-bar mumps-menu jmp] 
;;     '("Jump to Routine Definition" . lkm-jump-to-routine-def)) 
;;   (define-key 
;;     global-map 
;;     [menu-bar mumps-menu cmp] 
;;     '("Complete Keyword at Cursor" . lkm-complete-symbol)) 
;;   (define-key 
;;     global-map 
;;     [menu-bar mumps-menu abt] 
;;     '("About LorikeeM" . lkm-about)) 
;;   (define-key 
;;     global-map 
;;     [menu-bar mumps-menu com] 
;;     '("Compile Current Buffer" . lkm-gtm-compile)) 
 
;;   (setq font-lock-defaults '((mumps-font-lock-keywords))) 
 
;;   (setq mkua nil) 
;;   (setq mkuf nil) 
;;   (setq mkla nil) 
;;   (setq mklf nil) 
;;   (setq mfua nil) 
;;   (setq mfuf nil) 
;;   (setq mfla nil) 
;;   (setq mflf nil) 
;;   (setq mo nil) 
 
;;   (setq mode-name "LorikeeM") 
;;   (run-hooks 'mumps-mode-hook) 
;; )







;; #####################################################################################







;File: mumps-mode.el
;;
;;Created: Mon Mar 23 19:29:04 1998 Cheng-Wei Wu -cwwu-
;;Time-stamp: <2012-09-20 12:49:13 cwwu>
;;Purpose: Mumps/M/Cache mode
;;Reference: http://en.wikipedia.org/wiki/MUMPS_syntax
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; $RCSfile: mumps-mode.el,v $ $Revision: 1.9 $
;;
;; $Log: mumps-mode.el,v $
;; Revision 1.9  2012/09/19 19:39:07  cwwu
;; Before enhance font-lock-keywords list
;;
;; Revision 1.8  2012/09/14 20:29:43  cwwu
;; Before split out basic and enhancement parts
;;
;; Revision 1.7  2012/08/16 18:18:06  cwwu
;; make some separation with checksum, mumps-shell, and tint-instances. Still
;; not perfect but minimum.
;;
;; Revision 1.6  2012/03/01 15:51:58  cwwu
;; set tab-width to 1 as local value
;;
;; Revision 1.5  2012/01/04 22:12:18  cwwu
;; handle buffer without file name for mumps-mode-parse-file-name
;;
;; Revision 1.4  2011/11/01 21:22:25  cwwu
;; *** empty log message ***
;;
;; Revision 1.3  2011/03/10 15:01:34  cwwu
;; before try to make mumps-mode and mumps-shell "env" aware so I can use
;; multiple mumps-shell.
;;
;; Revision 1.2  2008/06/04 05:34:08  cwwu
;; A version with some improvements. Based on emacs 21.3.
;;
;; Revision 1.1  2002/12/11 18:26:33  cwwu
;; 
;; Revision 1.0  1998/04/15 14:44:50  cwwu
;;
;; TO-DOs:
;; + Some spaces are significant in some situation; they are not merely whitespace
;; + handle font-lock for comma case. Like d tagA,tagB
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup:
;;
;; Use following code in your emacs set-up file (such as ~/.emacs):
;;
;; All files with *.mumps[.*], or *.ROU[.*] use mumps mode
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.\\(mumps\\|mump\\|ROU\\)\\(\\.\\sw*\\)?$" . mumps-mode)
;; 	 ) auto-mode-alist))
;;
;; Also since we only support windows-1252 code base system now, so it is better to ensure 
;;   the routines are using this encoding. It seems that cannot be done inside mumps-mode.el
;; (add-to-list 'file-coding-system-alist '("\\.\\(mumps\\|mump\\|ROU\\)\\(\\.\\sw*\\)?\\'" . windows-1252-unix) )
;; 
;; To load mumps-mode automatically
;; (autoload 'mumps-mode "mumps-mode")
;;
;; To turn on font-lock mode.  CW 12/02 no need to do this anymore
;; (if window-system
;;     (add-hook 'mumps-mode-hook 'turn-on-font-lock))
;;
;; Some useful commands in mumps-mode buffer, see mumps-shell-map
;; for more detail:
;;
;;   "\C-cf"    'mumps-checkin-routine-with-version-lock
;;   "\C-c\C-f" 'mumps-find-routine
;;   "\C-c\C-r" 'mumps-find-routine-read-only
;;   "\C-cm"    'mumps-shell-lookitt-macro
;;   "\C-c\C-l" 'mumps-display-mumps-shell
;;   "\C-c\C-m" '(lambda () (interactive) (mumps-display-mumps-shell t))
;;
;;   "\C-c\C-s" 'mumps-save-buffer
;;   "\C-cl"    'mumps-lock-routine
;;   "\C-cr"    'mumps-release-lock-routine
;;   "\C-ck"    'mumps-show-routine-lock-count
;;   "\C-cc"    'mumps-checksum
;;   "\C-c\C-c" 'checksum-region-noeol-show
;;   "\C-c\C-w" 'mumps-save-log
;;   "\C-ci"    'mumps-check-in-buffer
;;   "\C-co"    'mumps-check-out-buffer
;;   "\C-c~"     'mumps-diff-with-previous-version
;;
;;   "\C-ct"    'mumps-find-tag
;;   "\C-c\C-t" 'mumps-find-tag-line
;;   "\C-c="    'mumps-count-tag-lines
;;   "\C-c-"    'mumps-move-line
;;   "\C-c\C-n" 'mumps-next-tag
;;   "\C-c\C-p" '(lambda () (interactive) (mumps-next-tag t))
;;
;;
;;

;(require 'tint-instances nil t)  ;optional ;TODO-replace with hilight-regexp
(require 'font-lock)

;some customizable variables for mumps-mode

;*CW+3 6/17/08 adding support to automatically use windows-1252 for *.mumps*, *.ROU* (as unix) and ARD-*.txt (as dos) 
; may need make the regular expression more precise, is "\\'" means end of string like "$"?
; Now moved to .emacs-init.el: it seems too late to do this when first time mumps-mode is loading for opening a mumps file.  Put it here it won'r work for the first opened file.
;(add-to-list 'file-coding-system-alist '("\\.\\(mumps\\|ROU\\)\\(\\.\\sw*\\)?\\'" . windows-1252-unix) )
;(add-to-list 'file-coding-system-alist '("ARD-.*\\.txt\\'" . windows-1252-dos) )


;*CW 3/10/2011
(defvar mumps-mode-env nil   
  "the environment this mode is associated with.")

;keymap for mumps-shell mode
(defvar mumps-mode-map nil
  "Keymap for mumps major mode.")

(defvar mumps-mode-use-mumps-shell nil
  "t if it uses mumps-shell")

(defvar mumps-mode-use-checksum
  "t if it has checksum.el")

(defvar mumps-mode-line-format nil
  "Mumps mode line format")

;buffer local variabls
(defvar mumps-mode-file-name nil
  "File name in mumps mode")

(defvar mumps-mode-DLG nil
  "DLG number in the file name, like 'ROU.mumps.12345'")

(defvar mumps-mode-rev nil
  "Revision number when in revision control")

(make-variable-buffer-local 'mumps-mode-file-name)
(make-variable-buffer-local 'mumps-mode-DLG)
(make-variable-buffer-local 'mumps-mode-rev)

(setq mumps-mode-use-mumps-shell
      (require 'mumps-shell nil t))

(setq mumps-mode-use-checksum
      (require 'checksum nil t))

(if mumps-mode-map
    nil
  (let ((map (make-sparse-keymap "Mumps-Mode"))
        (map2 (make-sparse-keymap)))
    ;*CW+2 future change?
    ;(setq mumps-mode-map
	;  (nconc (make-sparse-keymap) mumps-shell-map))
    ;(setq mumps-mode-map (make-keymap))
    (if mumps-mode-use-mumps-shell
        (progn
          (define-key map2 "\C-cf" 'mumps-checkin-routine-with-version-lock)
          (define-key map2 "\C-c\C-f" 'mumps-find-routine)
          (define-key map2 "\C-c\C-r" 'mumps-find-routine-read-only)
          (define-key map2 "\C-cm" 'mumps-shell-lookitt-macro)
          (define-key map2 "\C-c\C-l" 'mumps-display-mumps-shell)
          (define-key map2 "\C-c\C-m"
            '(lambda () (interactive) (mumps-display-mumps-shell t)))
          ;
          (define-key map2 "\C-c\C-s" 'mumps-save-buffer)
          (define-key map2 "\C-cl" 'mumps-lock-routine)
          (define-key map2 "\C-cr" 'mumps-release-lock-routine)
          (define-key map2 "\C-ck" 'mumps-show-routine-lock-count)
          ))
    (if mumps-mode-use-checksum
        (progn
          (define-key map2 "\C-cc" 'mumps-checksum)
          (define-key map2 "\C-c\C-c" 'checksum-region-noeol-show)
          ))
    (if mumps-mode-use-mumps-shell
        (progn
          (define-key map2 "\C-c\C-w" 'mumps-save-log)
          (define-key map2 "\C-ci" 'mumps-check-in-buffer)
          (define-key map2 "\C-co" 'mumps-check-out-buffer)
          (define-key map2 "\C-c~" 'mumps-mode-diff-with-previous-version)
          ))
    ;
    (define-key map2 "\C-ct" 'mumps-find-tag)
    (define-key map2 "\C-c\C-t" 'mumps-find-tag-line)
    (define-key map2 "\C-c=" 'mumps-count-tag-lines)
    (define-key map2 "\C-c-" 'mumps-move-line)
    (define-key map2 "\C-c\C-n" 'mumps-next-tag)
    (define-key map2 "\C-c\C-p" '(lambda () (interactive) (mumps-next-tag t)))
    ;
    ;the menu bar keymap
    (define-key map2 [menu-bar] (make-sparse-keymap))
    (define-key map2 [menu-bar mumps-mode]
      (cons "Mumps-Mode" map))
;    (define-key map [prev-tag]
;      '("Prev tag" . '(lambda () (interactive) (mumps-next-tag t)))
    (define-key map [next-tag] '("Next tag" . mumps-next-tag))
    (define-key map [move-line] '("Move line forward " . mumps-move-line))
    (define-key map [count-tag-lines] '("Count tag line" . mumps-count-tag-lines))
    (define-key map [find-tag-line] '("Find tag line". mumps-find-tag-line))
    (define-key map [find-tag] '("Find tag" . mumps-find-tag))
;    (define-key map [switch-to-mumps-shell] 
;      '("Switch to cache in other window" . lambda () (interactive) (mumps-display-mumps-shell t))))
    (if mumps-mode-use-mumps-shell
        (progn
          (define-key map [separator-2] '("--"))
          (define-key map [diff-prev] '("Diff with version" . mumps-mode-diff-with-previous-version))
          (define-key map [checkout] '("Check-out" . mumps-check-out-buffer))
          (define-key map [checkin] '("Check-in" . mumps-check-in-buffer))
          (define-key map [save-routines-in-DLG]
            '("Save routines in DLG" . mumps-save-log))
          ))
    (if mumps-mode-use-checksum
        (progn

          (define-key map [region-checksum]
            '("Region checksum" . checksum-region-noeol-show))
          (define-key map [checksum] '("Checksum" . mumps-checksum))
          ))
    (if mumps-mode-use-mumps-shell
        (progn
          (define-key map [routine-show-lock-count] '("Show lock count" . mumps-show-routine-lock-count))
          (define-key map [routine-release-lock]
            '("Release lock" . mumps-release-lock-routine))
          (define-key map [routine-lock] '("Lock routine" . mumps-lock-routine))
          (define-key map [save-buffer] '("Save to Cache" . mumps-save-buffer))
          (define-key map [separator-1] '("--"))
          ))

    (if mumps-mode-use-mumps-shell
        (progn
          (define-key map [display-mumps-shell]
            '("Display cache in other window" . mumps-display-mumps-shell))
          (define-key map [lookitt-macro]
            '("Lookitt macro" . mumps-shell-lookitt-macro))
          (define-key map [find-read-only] 
            '("Find read-only" . mumps-find-routine-read-only))
          (define-key map [find-routine] 
            '("Find routine". mumps-find-routine))
          (define-key map [checkin-routine-version-lock]
            '("Checkin routine with version lock" . mumps-checkin-routine-with-version-lock))
          ))
    (if mumps-mode-use-checksum
        (put 'region-checksum 'menu-enable 'mark-active)) ;modify some property of region-checksum
    (setq mumps-mode-map map2)
    )
  )

;(defvar mumps-mode-hook nil  ;*CW+6 6/08 use defcustom
;  "*List of functions to call when entering Mumps mode.")
(defcustom mumps-mode-hook nil
  "*List of hooks run when entering Mumps mode."
  :type 'hook
  :options '(turn-on-flyspell turn-on-font-lock)
  :group 'data)

(add-hook 'mumps-mode-hook 'turn-on-flyspell)

;variables for mumps-mode

(defvar mumps-mode-syntax-table nil
  "Sntax table used in mumps-mode buffers.")

(if mumps-mode-syntax-table ;when done replace nil with mumps-mode-syntax-table
    ()
  (setq mumps-mode-syntax-table (make-syntax-table))
  ;; add comment syntax
  (modify-syntax-entry ?\; "<    " mumps-mode-syntax-table)  ;comment start
  (modify-syntax-entry ?\n ">    " mumps-mode-syntax-table)  ;comment end
  (modify-syntax-entry ?\^m ">    " mumps-mode-syntax-table) ;comment end
  ;; map some M operators as symbol
  (modify-syntax-entry ?\\ "_"   mumps-mode-syntax-table)    ;like +,-,*
  (modify-syntax-entry ?$  "_"   mumps-mode-syntax-table)    ;function/intrinsic function
  (modify-syntax-entry ?!  "_"   mumps-mode-syntax-table)    ;logical or
  (modify-syntax-entry ?#  "_"   mumps-mode-syntax-table)    ;modulo
  (modify-syntax-entry ?'  "_"   mumps-mode-syntax-table)    ;logical not
  (modify-syntax-entry ??  "_"   mumps-mode-syntax-table)    ;patten matching
  (modify-syntax-entry ?@  "_"   mumps-mode-syntax-table)    ;indirection
  (modify-syntax-entry ?[  "_"   mumps-mode-syntax-table)    ;contains
  (modify-syntax-entry ?]  "_"   mumps-mode-syntax-table)    ;follows (]) and sort after(]])
  )
  

(defvar mumps-mode-abbrev-table nil "")

(if (< emacs-major-version 22)  ;before version 22 vc-mode is in minor-mode-alist, but in default mode-line-format after 22
    (setq mumps-mode-line-format ; for emacs version before 22
      '("-"
        mode-line-mule-info
        mode-line-modified
        mode-line-frame-identification
        mode-line-buffer-identification "   "
        "<" mumps-shell-working-env ":" mumps-shell-working-dir ">  "
        global-mode-string
        "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--"
        (which-func-mode
         ("" which-func-format "--"))
        (line-number-mode "L%l--")
        (column-number-mode "C%c--")
        (-3 . "%p") "-%-"))
  (setq mumps-mode-line-format  ;for emacs version after 22
    '("%e" ;error message about memory full
      #("-" 0 1
        (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
      mode-line-mule-info
      mode-line-modified
      mode-line-frame-identification
      mode-line-buffer-identification 
      #("   " 0 3
        (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
      "<" mumps-shell-working-env ":" mumps-shell-working-dir ">"
      #("  " 0 2
        (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
      (vc-mode vc-mode)
      "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]"
      #("--" 0 2
        (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
      (which-func-mode
       ("" which-func-format 
        #("--" 0 2 
          (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))))
      (line-number-mode "L%l")
      #("-" 0 1
        (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
      (column-number-mode "C%c")
      #("--" 0 2
        (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
      (-3 . "%p")
      #("--" 0 2
        (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
      (global-mode-string
       (#("--" 0 2
          (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
        global-mode-string))
      #("-%-" 0 3
        (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
      ))
  )


(defun mumps-mode-local-variables (lisp-syntax)
  (cond (lisp-syntax
	 (set-syntax-table mumps-mode-syntax-table)))
  ;(setq local-abbrev-table mumps-mode-abbrev-table)
  (make-local-variable 'mumps-mode-env)   ;*CW 3/10/11
  ;*CW+7 defined in newcomment.el
  (set (make-local-variable 'comment-start) ";")    ; ";" indicating start a new comment
  (set (make-local-variable 'comment-end) "")       ; empty string indicating comments are terminated by end-of-line
  (set (make-local-variable 'comment-column) 1)     ; column to indent right-margin comments to
  (set (make-local-variable 'comment-start-skip) ";+[ \t]*")    ; regexp to match the start of a comment plus everyting up to its body
  (setq indent-tabs-mode nil) ;makes TAB insert spaces
  (setq tab-width 1) ;make TAB shows as one space - we don't want to have TAB in the routine anyway - indent-tabs-mode nil above
  "Make and define some buffer local-variables for mumps-mode"
  )

;I don't know how to use facemenu yet.
;(if (fboundp 'facemenu-unlisted-faces)
;    (add-to-list 'facemenu-unlisted-faces 'mumps-space-face)

;setup mumps-space-face
(if (fboundp 'defface) ;defface is a macro in emacs 20.2
    (defface mumps-space-face
      '((((class color)) (:background  "hotpink"))
	(t (:reverse-video t)))
      "Face to use for highlighting leading spaces in Font-Lock mode."
      :group 'faces
      :group 'mumpsmode)
  ;; for emacs 19.34.1, use following (from emacs 19 make-mode.el)
  ;; make-file-define-space-face
  (if (fboundp 'make-face)
      (progn
	(make-face 'mumps-space-face)
	(or (not (or (eq window-system 'x) (eq window-system 'win32)))
	    (face-differs-from-default-p 'mumps-space-face)
	    (let* ((params (frame-parameters))
		   (light-bg (cdr (assq 'background-mode params)))
		   (bg-color (cond ((eq (cdr (assq 'display-type params)) 'mono)
				    (if light-bg "black" "white"))
				   ((eq (cdr (assq 'display-type params)) 'grayscale)
				    (if light-bg "black" "white"))
				   (light-bg	; Light color background.
				    "hotpink")
				   (t		; Dark color background.
				    "hotpink"))))
	      (set-face-background 'mumps-space-face bg-color))))))

;setup mumps-DLG-face
(if (fboundp 'defface) ;defface is a macro in emacs 20.2
    (defface mumps-DLG-face
      '((((class color)) (:background  "Firebrick" :foreground "Yellow"))
	(t (:reverse-video t)))
      "Face to use for highlighting leading spaces in Font-Lock mode."
      :group 'faces
      :group 'mumpsmode)
  ;; for emacs 19.34.1, use following (from emacs 19 make-mode.el)
  ;; make-file-define-space-face
  (if (fboundp 'make-face)
      (progn
	(make-face 'mumps-DLG-face)
	(or (not (or (eq window-system 'x) (eq window-system 'win32)))
	    (face-differs-from-default-p 'mumps-DLG-face)
	    (let* ((params (frame-parameters))
		   (light-bg (cdr (assq 'background-mode params)))
		   (bg-color (cond ((eq (cdr (assq 'display-type params)) 'mono)
				    (if light-bg "black" "white"))
				   ((eq (cdr (assq 'display-type params)) 'grayscale)
				    (if light-bg "black" "white"))
				   (light-bg	; Light color background.
				    "Firebrick")
				   (t		; Dark color background.
				    "Firebrick"))))
	      (set-face-background 'mumps-DLG-face bg-color))))))



(defvar mumps-space-face 'mumps-space-face
  "Face to use for highlighting leading spaces in Font-Lock mode.")

(defvar mumps-DLG-face 'mumps-DLG-face
  "Face to make DLG number")

;followed is referenced from pascal.el
(defconst mumps-font-lock-keywords
  (purecopy
   (list
    ;; preprocessor tags and comments
    '("[[:space:]]\\(;;#[[:alnum:]]+#\\)" 1 font-lock-preprocessor-face t)
    ;; 
    ;; #STR# comments
    ;'("[[:space:]];[[:space:]]*\\(#\\(STR\\|CMT\\)#\\)" 1 font-lock-type-face t)
    '("[[:space:]];[[:space:]]*\\(#\\(STR\\|CMT\\)#[[:alnum:]]+#\\)" 1 font-lock-type-face t)   ;002, 002L2
    ;'("[[:space:]];[[:space:]]*\\(#STR#[[:alnum:]]+#\\)" 1 font-lock-type-face t) 
    ;'("[[:space:]];[[:space:]]*\\(#CMT#[[:digit:]]+#\\)" 1 font-lock-type-face t)
    '("[[:space:]];[[:space:]]*\\(#\\(STR\\|CMT\\)#[[:alnum:]]+#\\)\\(.*\\)" 3 font-lock-doc-face t)
    ;; 
    ;; tags/labels
    '("^%?[[:digit:]]+" . font-lock-type-face) ; tag that start with digit may not contain letters
    '("^%?[a-z][[:alnum:]]*" . font-lock-type-face)
    ;; 
    ;; function calls, match $$^ROU by not forcing a tag name
    '("\\$\\$%?[[:digit:]]+" . font-lock-type-face) ; tag that start with digit may not contain letters
    '("\\$\\$%?[a-z][[:alnum:]]*" . font-lock-type-face)
    '("\\<do?\\(:.+?\\)? \\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\(.*\\)?" 2 font-lock-type-face)
    ;'("\\<do?\\(:.+?\\)? \\(\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\(.*\\)?,\\)\\{1\\}\\(\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\(.*\\)?\\)" 7 font-lock-type-face)  ;TODO - how to handle comma case?
    ;'("do?\\(:.+?\\)? \\(\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\((.*)\\)?,\\)*\\(\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\((.*)\\)?\\)\\(,\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\((.*)\\)?\\)*" 7 font-lock-warning-face)

    '("\\<g\\(oto\\)?\\(:.+?\\)? \\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?" 3 font-lock-type-face)
    ;; 
    ;; built-in functions
    '("\\$[A-Za-z]+" . font-lock-builtin-face)
    ;; 
    ;; variables & arguments
    '("[[:space:]]n\\(ew\\)? \\(.+\\)" 2 font-lock-variable-name-face)
    '("^%?[[:digit:]]+(\\(.+\\)[ \n]" 1 font-lock-variable-name-face)
    '("^%?[a-z][[:alnum:]]*(\\(.+\\))[ \n]" 1 font-lock-variable-name-face)
    ;; 
    ;; routines or globals
    '("\\^%?[[:alnum:]]+" . font-lock-constant-face)
    ;; 
    ;; highlight keywords
    (cons (concat "\\<\\(c\\(lose\\)?\\|g\\(oto\\)?\\|h\\(ang\\)?\\|j\\(ob\\)?\\|m\\(erge\\)?\\|n\\(ew\\)?\\|o\\(pen\\)?\\|r\\(ead\\)?\\|s\\(et\\)?\\|u\\(se\\)?\\|v\\(iew\\)?\\|w\\(rite\\)?\\|x\\(ecute\\)?\\)\\>[ :]")
	  'font-lock-keyword-face) ; type 1 keywords
    (cons (concat "\\<\\(b\\(reak\\)?\\|h\\(alt\\)?\\|tc\\(ommit\\)?\\|tre\\(start\\)?\\|tro\\(llback\\)?\\|ts\\(tart\\)?\\)\\>[:\n ]")
	  'font-lock-keyword-face) ; type 2 keywords
    (cons (concat "\\<\\(d\\(o\\)?\\|k\\(ill\\)?\\|l\\(ock\\)?\\|q\\(uit\\)?\\)\\>[:\n ]")
	  'font-lock-keyword-face) ; type 3 keywords
    (cons (concat "\\<\\(f\\(or\\)?\\|i\\(f\\)?\\)\\>[ ]")
	  'font-lock-keyword-face) ; type 4 keywords-need more work
    (cons (concat "\\<\\(e\\(lse\\)?\\)\\>  ")
	  'font-lock-keyword-face) ; type 5 keywords
    '("\t+" . mumps-space-face) ;highlight TABs since MUMPS does not like TABS
    ))

  "Additional expressions to highlight in Mumps mode.")

;(put 'mumps-mode 'font-lock-defaults '(mumps-font-lock-keywords nil t)) ;cwwu 10/8/03: looks like redundant to add/create a font-lock-defaults property to mumps-mode

(defvar mumps-font-lock-keywords-local nil
  "The real expressions to highlight in Mumps mode.")

;(defvar mumps-time-string-format "%b %d, %Y %l:%M %p"  ; refert to documentation of format-time-string
(defvar mumps-time-string-format "%Y-%m-%d %H:%M:%S"
  "Time string format for mumps-mode.")

;functions for mumps-mode

(defun mumps-locate-timestamp (&optional noerr)
  "Locate the timestamp position in mumps-code, and move the cursor there.
Return a pair of points to the beginning and end of timestamp in the buffer."
  (interactive "P")
  (let* ((min-point (point-min)) endline spoint epoint)
    (goto-char min-point)
    (forward-line 4)
    (end-of-line)
    (setq endline (point))
    (goto-char min-point)
    (setq spoint (re-search-forward "^\\sw+\\((.*)\\)? *;[^;\n]*;[^;\n]*\\(;\\).*$" endline noerr 1))
    (setq spoint (match-beginning 2))
    (if (not (null spoint)) 
	(progn
	  (setq spoint (+ spoint 1))
	  (end-of-line)
	  (setq endline (point))
	  (goto-char spoint)
	  (setq epoint (re-search-forward ";" endline 0 1)) ;if failed just move the the limit search
	  (if epoint (setq epoint (- epoint 1))
	    (setq epoint (point)))
	  (list spoint epoint)))
    )
  )

(defun mumps-insert-timestamp ()
  "Insert a timestamp in mumps-mode, using format in mumps-time-string-format."
  (interactive "*") ;abort if buffer is readonly
  (insert (format-time-string mumps-time-string-format (current-time)))
  )

(defun mumps-update-timestamp ()
  "Find timestamp in mumps code and replace it with the current time."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(let* ((pairs (mumps-locate-timestamp t))
	       start end)
	  (if pairs
	      (progn
		(setq start (car pairs))
		(setq end (car (cdr pairs)))
		(goto-char start)
		(delete-region start end)
		;(insert " ") ;*CW 8/27/07 don't add space
		(mumps-insert-timestamp))
	    (message "Timestamp not updated.")
	    (ding nil)
	    (sleep-for 1)
	    ;(run-at-time 1 nil 'message "Timestamp not updated.")
	    ))
	)
      )
    )
  nil)

(defun mumps-find-tag (tag)
  "Find the tag in the Mumps code."
  (interactive "sTags: ")
  (let ((match (concat "^" tag))
	;(spoint (push-mark (point) t)))
	(spoint (point)))
    (goto-char (point-min))
    (if (not (search-forward-regexp match nil t))
	(progn (message "Tag \"%s\" not found." tag)
	       (goto-char spoint))))
  )

(defun mumps-find-tag-line (tag ln)
  "Move the cursor to the lines of tag in the Mumps code."
  (interactive "sTags: \nNnumber of lines: ")
  (mumps-find-tag tag)
  (if ln (mumps-move-line ln)))

(defun mumps-mode ()
  "Major mode for editing Mumps/M/Cache code.
While saving the file, it will update the time stamp and
remove all tailing spaces at the end of each line.
\\{mumps-mode-map}
Entry to this mode calls the value of `mumps-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mumps-mode-map)
  (setq major-mode 'mumps-mode)
  (setq mode-name "Mumps")
  ;; Font lock support
  ;(make-local-variable 'font-lock-defaults)
  ;(setq font-lock-defaults '(mumps-font-lock-keywords nil t))
  (mumps-mode-local-variables t)
  ;; for parsing file name, is there a difference than using make-local-variable?
  ;(make-variable-buffer-local 'mumps-mode-file-name)
  ;(make-variable-buffer-local 'mumps-mode-DLG)
  ;(make-variable-buffer-local 'mumps-mode-rev)
  (mumps-mode-parse-file-name)
  ;; Font lock support and use mumps-mode-DLG to highlight DLG number
  ;(if (require 'tint-instances nil t)
      (if (not (null mumps-mode-DLG)) ;highlight existing DLG number
          (if (stringp mumps-mode-DLG)
              (if (not (string= "" mumps-mode-DLG))
                  ;(tint_all_instances_of_string mumps-mode-DLG))
                  (highlight-phrase mumps-mode-DLG 'hi-yellow))
            ))
  ;  )
  ;this is not really working yet, because DLG is bared inside comments, how to override that just for the DLG number?
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'mumps-font-lock-keywords-local)
  (if (stringp mumps-mode-DLG)
      (if (string= "" mumps-mode-DLG)
          (setq mumps-font-lock-keywords-local mumps-font-lock-keywords) ;if an empty string
        (setq mumps-font-lock-keywords-local 
              (append
               mumps-font-lock-keywords
               (list
                ;(cons (concat "\\<" mumps-mode-DLG "\\>") 'mumps-DLG-face)
                ;(cons (concat "\\b" mumps-mode-DLG "\\b") 'mumps-DLG-face)
                (list (concat "\\b\\(" mumps-mode-DLG "\\)\\b") 1 'mumps-DLG-face))
               ))
        )
    ;if mumps-mode-DLG is not a string
    (setq mumps-font-lock-keywords-local mumps-font-lock-keywords)
    )
  (setq font-lock-defaults '(mumps-font-lock-keywords-local nil t))
                                        ;  
  (add-hook 'local-write-file-hooks 'mumps-update-timestamp)
  (add-hook 'local-write-file-hooks 'mumps-remove-trailing-space t) ;should be the last one performed
  (setq mode-line-format mumps-mode-line-format)
  (run-hooks 'mumps-mode-hook)
  )

(if mumps-mode-use-checksum
    (defun mumps-checksum ()
      "Display checksum for the current mumps code in the current buffer. Actually 
what it does is calculate checksum starting from line 2. There are two 
checksum numbers reported. The one in the parenthesis is the checksum 
excluding end-of-line's (the one used by our release team ^%ZaHSUM2."
      (interactive)
      (save-excursion
        (save-restriction
          (save-match-data
            (let (start lastline
                        (sum1 0.0)
                        (sum2 0.0))
              (mumps-locate-timestamp)
              (forward-line 1)
              (setq start (checksum-line-number (point)))
              (setq lastline (checksum-last-line-number))
              (setq sum1 (checksum-lines start lastline sum1))
              (setq sum2 (checksum-lines start lastline sum2 t))
                                        ;(message "Mumps checksum for %s = %10.0f (neol %10.0f)" buffer-file-truename sum1 sum2)))))
              (message "Mumps checksum for %s = %10.0f (neol %10.0f)" (buffer-name) sum1 sum2)))))
      nil)
  )

(if mumps-mode-use-mumps-shell
        (progn
          (defun mumps-display-mumps-shell (&optional select)
            "Make the Mumps shell buffer visible in a window from a mumps mode buffer.
If the optional argument select is non-nil, it will select the buffer."
            (interactive "P")
            (mumps-shell-display-shell select))

          (defun mumps-save-log (lognum)
            "Save the current file to file with the lognum without changing the visit 
buffer as `FILE.mumps.12345'. Use write-file to do similar and also switch 
the buffer to that new file."
            (interactive "Nlog numer: ")
            (let ((name (buffer-name)))
              (setq name (concat name "." lognum))
              (write-region (point-min) (point-max) name)))
          ))


(defun mumps-move-line (linenum)
  "Move the cursor forward/backward linenum lines."
  (interactive "Nnumber of lines: ")
  (let* ((tmp (forward-line linenum)))
    (if (not (= tmp 0)) (message "Out of rang (%d)" tmp))))

(defun mumps-next-tag (&optional reverse)
  "Find the next tag. If reverse is non-nil, find the previous tag."
  (interactive "P")
  (let ((match "^\\S-")
	;(spoint (push-mark (point) t)))
	(spoint (point))
	tpoint
	ok)
    (if reverse
	(setq ok (search-backward-regexp match nil t))
      (end-of-line)
      (setq ok (search-forward-regexp match nil t)))
    (if (not ok)
	(progn (message "No tag found")
	       (goto-char spoint))
      (beginning-of-line)
      (setq tpoint (point))
      tpoint)))
  
(defun mumps-count-tag-lines ()
  "Return the line number of the current position to the previous tag."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	(let (curpos tagpos lines epos taglabel)
	  (beginning-of-line)
	  (setq curpos (point))
	  (setq tagpos (mumps-next-tag t))
	  (if tagpos
	      (progn
		(setq epos (search-forward-regexp " " nil t))
		(setq taglabel (buffer-substring tagpos epos))
		(setq lines (count-lines tagpos curpos))
		(message "%d lines to previous tag %s" lines taglabel))
	    (message "No tag found")))))))

(defun mumps-find-trailing-space (&optional cp)
  "Find a line with a trailing spaces and move to the first space of that 
   trailing spaces. If cp is not nil, it will start search from point cp"
  (interactive)
  (let (pt)
    (if (numberp cp) (goto-char cp))
    (setq pt (re-search-forward "^.*\\s +$" (point-max) t))
    (if pt
	(progn
	  (setq pt (search-backward-regexp "\\S " (point-min) t))
	  (forward-char 1)))
    pt))

(defun mumps-remove-trailing-space (&optional cp)
  "Remove all trailing spaces for each line of text. "
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
        (if (numberp cp) (goto-char cp) (goto-char (point-min)))
	(while (mumps-find-trailing-space)
	  (delete-horizontal-space))
	)))
  nil)


(defun mumps-locate-routine-header (&optional noerr)
  "Locate the routine header"
  (interactive "P")
  (let* ((spoint (point-min)) endline epoint)
    (goto-char spoint)
    (end-of-line)
    (setq endline (point))
    (goto-char spoint)
    (setq epoint (re-search-forward "^Epic Systems Corporation\\^*" endline noerr))
    (if epoint
	(progn
	  (end-of-line 4)
	  (setq endline (point))
	  (beginning-of-line)
	  (setq epoint (re-search-forward "^\\^\\sw+$" endline noerr))
	))
  ))

(defun mumps-locate-routine-tail (&optional noerr)
  "Locate the routine header"
  (interactive "P")
  (let* ((epoint (point-max)) endline spoint)
    (goto-char epoint)
    (forward-line -1)
    (setq spoint (point))
    (setq epoint (re-search-forward "^\\*\\*\\*\\*\\*\\*\\*$" endline noerr))
    ))

(if mumps-mode-use-mumps-shell
    (progn
      (defun mumps-checkin-routine-with-version-lock (routine &optional ext)
        (interactive "sRoutine: \nP")
        (mumps-shell-checkin-routine-with-version-lock routine ext))

      (defun mumps-find-routine (routine &optional ext)
        (interactive "sRoutine: \nP")
        (save-excursion
          (mumps-shell-find-routine routine ext)))

      (defun mumps-find-routine-read-only (routine &optional ext)
        (interactive "sRoutine: \nP")
        (save-excursion
          (mumps-shell-find-routine-read-only routine)))

      ;;To-do: prompt for save buffer first if there is unsaved chnage in the buffer
      (defun mumps-save-buffer (&optional rellock)
        "Load the current routine back to M environment.  If rellock is 1 or there is
a prefix argument, the M session lock will be released (decrement by 1).
Note, it will load the *saved* plain file back to M.  So unsaved changes in 
the buffer will not be saved into M environment."
        (interactive)
        (save-excursion
          (let (filename)
            (if current-prefix-arg
                (setq rellock "1") (setq rellock "0"))
            (setq filename (buffer-file-name))
            (mumps-shell-load-file filename rellock))))

      ;;This is working but I am not satisified with it yet.
      (defun mumps-check-in-buffer (&optional DLG)
        "RCS check in the file in the current buffer.  DLG is a DLG number used 
to use with ci option -t and -n to stamp the DLG number."
        (interactive "*")
        (save-excursion
          (let (filename cioptions oscmd)
            (if (and (null DLG) current-prefix-arg)
                (setq DLG (read-string "DLG: ")))
            (if (not (null DLG))
                (progn
                  (if (numberp DLG) (setq DLG (number-to-string DLG)))
                  (setq cioptions (concat " -t-DLG" DLG " -nDLG" DLG))))
            (setq cioptions (concat "-l" cioptions))
            (setq filename (mumps-shell-get-server-file-fullname (buffer-file-name)))
            (setq oscmd (mumps-shell-form-string-args (list "ci" cioptions filename)))
            (mumps-shell-OS-cmd oscmd)
            (if (null DLG)
                (message "File %s checked in" filename)
              (message "File %s checked in for %s" filename DLG)))))
  
      (defun mumps-check-out-buffer ()
        "RCS check in the file in the current buffer"
        (interactive)
        (save-excursion
          (let (filename oscmd)
            (setq filename (mumps-shell-get-server-file-fullname (buffer-file-name)))
            (setq oscmd (mumps-shell-form-string-args (list "co" filename)))
            (mumps-shell-OS-cmd oscmd)
            (message "File %s checked out" filename))))


      (defun mumps-lock-routine ()
        (interactive)
        (save-excursion
          (let (filename)
            (setq filename (buffer-file-name))
            (mumps-shell-lock-routine filename))))

      (defun mumps-release-lock-routine ()
        (interactive)
        (save-excursion
          (let (filename)
            (setq filename (buffer-file-name))
            (mumps-shell-release-lock-routine filename)
            )))

      (defun mumps-show-routine-lock-count ()
        "Display the current lock count for the routine in the current buffer.
This is using lookit macro and displayed the lock table in the mumps-shell buffer"
        (interactive)
        (let (filename node)
          (setq filename (file-relative-name (buffer-file-name) (mumps-shell-get-client-file-directory)))
          (setq node (substring filename 0 (string-match "\\." filename)))
                                        ;not finished yet.  Need to pice out and get just the routine name
                                        ;(message "%S" node)
          (mumps-shell-show-lock-count node)
          ))
      ))


(defun mumps-narrow ()
  "Narrow the buffer to show only the real routine body.  For file saved by
^%ZeRSAVE that will include some extra header and trailer."
  (interactive)
  (let (spoint epoint)
    (setq spoint (mumps-locate-routine-header t))
    (if spoint
	(progn
	  (forward-line 1)
	  (setq spoint (point))
	  (setq epoint (mumps-locate-routine-tail t))
	  (if epoint
	      (progn
		(beginning-of-line)
		(setq epoint (point))
		(narrow-to-region spoint epoint)))))))
		
(defun mumps-widen ()
  "Widen the buffer to show the whole file.  For file saved by ^%ZeRSAVE 
that will include some extra header and trailer."
  (interactive)
  (widen))

(defun mumps-mode-get-previous-version (&optional REV)
  "Visit the original version that before modification if available.  
This is using vc to get the last version"
  (interactive)
  (if (null REV)
      (setq REV (read-string "REV: [1.1]")))
  (if (not (stringp REV))
      (message "REV must be a string")
    (progn
      (if (string= REV "")
          (setq REV "1.1"))
      (vc-revision-other-window REV) ;always assume 1.1 is the original version
      )
    )
  )

(defun mumps-mode-diff-with-previous-version (&optional REV)
  "Do a diff with a previous version in source conrol if available."
  (interactive)
  (let (buf1 buf2)
    (if (null REV)
        (setq REV (read-string "REV: [1.1]")))
    (if (not (stringp REV))
        (message "REV must be a string")
      (progn
        (if (string= REV "")
            (setq REV "1.1"))
        (vc-revision-other-window REV)
        (setq buf1 (concat mumps-mode-file-name ".mumps." mumps-mode-DLG))
        (setq buf2 (concat buf1 "." mumps-mode-rev))
        (ediff-buffers buf1 buf2)
        )
      )
    )
  )

(defun mumps-mode-parse-file-name ()
  "Parse the full file name into three pieces: `mumps-mode-file-name', 
   `mumps-mode-DLG', and `mumps-mode-rev'.  For example, `KNSUBAT1.mumps.12345.~1.1~',
    mumps-mode-file-name : KNSUBAT1
    mumps-mode-DLG:        12345
    mumps-mode-rev:        1.1    <-- when in source control"
  (interactive)
  (let (str p1 p2 p3 p4 s1 s2 s3 s4 e1 e2 e3 e4)
    (setq str buffer-file-name)
    ;(if (not (null (string-match "\\(\\w+\\)\.mumps\.\\(\\w+\\)\\($\\|.\\.*\\)\\(.*\\)" str)))
    ;(if (not (null (string-match "^.*/\\(\\w+\\)\.mumps\.\\(\\w+\\)\\($\\|.\\.*\\)\\(.*\\)" str))) ;*CW+1 1/04/12 handle buffer without file name
    (if (not (null str))
        (if (not (null (string-match "^.*/\\(\\w+\\)\.mumps?\.\\(\\w+\\)\\($\\|.\\.*\\)\\(.*\\)" str)))
            (progn
              (setq s1 (match-beginning 1))
              (setq s2 (match-beginning 2))
              (setq s3 (match-beginning 3))
              (setq s4 (match-beginning 4))

              (setq e1 (match-end 1))
              (setq e2 (match-end 2))
              (setq e3 (match-end 3))
              (setq e4 (match-end 4))

              (if (not (null s1)) (setq p1 (substring str s1 e1)))
              (if (not (null s2)) (setq p2 (substring str s2 e2)))
              (if (not (null s3)) (setq p3 (substring str s3 e3)))
              (if (not (null s4)) (setq p4 (substring str s4 e4)))
                                        ;piece 3 now is just to match the additional `.' if in source control: `ABC.mumps.1234.~1.1~'
                                        ;(message "P1=%s, P2=%s, P3=%s, P4=%s, P5=%s" p1 p2 p3 p4 p5)
              (setq mumps-mode-file-name p1)
              (setq mumps-mode-DLG p2)
              (setq mumps-mode-rev p4)
              )))))

(provide 'mumps-mode)

;;
;;EOF mumps-mode.el





