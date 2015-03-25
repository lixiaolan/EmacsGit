;;File: checksum.el
;;
;;Created: Mon Apr 13 20:03:07 1998 Cheng-Wei Wu -cwwu-
;;Time-stamp: <2010-11-11 13:01:02 cwwu>
;;Purpose: Checksum utility
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; $Log: checksum.el,v $
;; Revision 1.4  2010/11/11 19:02:10  cwwu
;; Handles UTF-8 encoded with multicharacters.
;;
;; Revision 1.3  2008/03/31 13:50:15  cwwu
;; Modified checksum-char-function.  Hope this solves file encoding
;; problem and files with characters like Ã€.
;;
;; Revision 1.2  2005/11/15 14:33:09  cwwu
;; Before modify checksum-string-function.
;; Use string-to-char will make the result not independent of the buffer coding system.  Try to use the result as reported in "File code" in what-cursor-position for non-ASCII multibyte character.
;;
;; Revision 1.1  2002/12/11 18:27:01  cwwu
;; Initial revision
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun checksum-buffer (&optional sum noeol)
  "Return a checksum for the buffer."
  (setq sum (checksum-lines (checksum-line-number (point-min)) (checksum-line-number (point-max)) sum noeol))
  sum)

(defun checksum-region (&optional sum noeol)
  "Return a checksum for the region between the points defined by region.  Currently, the region has to be defined starting from a beginning of a line, and cannot end in a middle of a line."
  (let* ((start (region-beginning))
         (end (region-end))
         (startLn (checksum-line-number start))
         (endLn (1- (checksum-line-number end)))) ;subtract 1: since when region-end is at eol of the last line in the region, line counts will return one extra count (so as if region-end is at the bol of the next line after the region-end.  See count-lines documentation.
    (setq sum (checksum-lines startLn endLn sum noeol))
    sum))

(defun checksum-lines (sline eline &optional sum noeol)
  "Return a checksum for string between line sline and eline.  
Notice, should use line number in widen mode."
  ;(interactive)
  (let (lcount (cnt 0))
    (setq lcount (1+ (- eline sline)))
    (goto-line sline)
    (while (and (> lcount 0) (= cnt 0))  ;so cnt =0 means forward-line not failed
      (setq sum (checksum-line sum noeol))
      (setq cnt (forward-line 1))  ;note forward 1 line on eob will not fail!
      (if (end-of-bufferp) (setq lcount 0)) ;takes care of last line have no eol char
      (setq lcount (1- lcount)))
    sum))
  
(defun checksum-line (&optional sum noeol)
  "Return a checksum for the string of the current line.
End-of-line character is included, if there is one.
End-of-line character is excluded, if noeol is t."
  ;(interactive)
  (let (string)
    (setq string (line-string nil noeol))
    (setq sum (checksum-string string sum)))
  sum)
    
	  
(defun checksum-string (string &optional sum)
  "Return a checksum for the input string."
  (let ((strsize (length string))
	(ix 0))
    (while (< ix strsize)
      (setq sum (checksum-char-function (aref string ix) sum))
      ;must use decimal for higher precision
      ;(princ (format "Iteration %d %10.0f\n" ix sum))
      (setq ix (1+ ix))))
  ;(princ (format "Checksum %10.0f" sum))
  (if (null sum) (setq sum 0))  ;so empty string will count as 0
  sum
  )

;;this one seems not useful
(defun checksum-string-function (string &optional sum)
  "Checksum function for the first char of a string."
  (if sum nil (setq sum 0.0))
  (setq sum (checksum-char-function (string-to-char string) sum))
  )

(defun checksum-char-function (char &optional sum)
  "Checksum function. Char is a single char, not string."
  ;use decimal for higher precision
  ;*CW+2 3/2008 The original code
  ;(if (null sum) (setq sum 0.0)) ;may need work
  ;(setq sum (mod (+ (* sum 64.0) char) 1000000007.0))
  ;
  ;*CW+9 3/2008 New code.  This solves the problem like character À
  ; À: char as 2240 but we want to use file code 192 for checksum calculation
  ;    this makes char (2240) becomes 192 (vchar)
  ;(let ((coding buffer-file-coding-system) encoded) ;; ref simple.el: what-cursor-position
  ;(setq encoded (and (>= char 128) (encode-coding-char char coding)))
  ;(encoded-string-description (encode-coding-char aachar buffer-file-coding-system) buffer-file-coding-system) ;this returns "C0" for "À") ;ref mule-diag.el:describe-char-after
  ;; ref mule-cmds.el:encode-coding-char
  ;*CW++ 11/11/2010 handles if we have UTF-8 encoding in routine - but will also rely on the file is opened with correct encoding
  (let ( 
        (vchar (encode-coding-char char buffer-file-coding-system))  ;*CW 11/11/2010 'utf-8-unix
        cnt ix
        )
    (if (null sum) (setq sum 5.))
    (if (stringp vchar)
        (progn
          (setq cnt (length vchar))
          (setq ix 0)
          (while (< ix cnt)
            (setq sum (mod (+ (* sum 64.0) (aref vchar ix)) 1000000007.0))
            (setq ix (1+ ix))
            ))
      )
    sum
    )
  )


;;functions for helping the checksum.el development
(defun line-string (&optional line-num noeol)
  "Return a string at line number line-num, current line if line-num is null.
The `end-of-line' character is included, if it has one.
Notice, should use line number in widen mode."
  ;(interactive)
  (let (start end)
    (if line-num (goto-line line-num))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq end (point))
    (if noeol nil
      (if (eobp)
	  nil
	(setq end (1+ end))))
    (buffer-substring start end)))

; (defun checksum-last-line ()
;   "Return the line numbe for the last line that is not nil in the buffer (respect narrow mode).
; Noteice, the line number returned is the line number respects narrow mode."
;   (interactive)
;   (save-excursion
;     (save-restriction
;       (save-match-data
; 	(let ((lastline 0)
; 	      (maxline 99999999)
; 	      (left 0))
; 	  (goto-char (point-min))
; 	  (while (and (not (eobp)) (< lastline maxline))
; 	    (end-of-line)
; 	    (setq lastline (1+ lastline))
; 	    (setq left (forward-line 1)))
; 	  (if (= lastline maxline)
; 	      (progn
; 		(message "Warning: buffer is too big, last line number may be incorrect.")
; 		(sleep-for 2)))
; 	  ;(message "Last line = %d (%d)" lastline maxline)
; 	  lastline)))))

(defun checksum-line-number (&optional pt)
  "Return the line number for point pt.  Notice, the line number returned is the line number
respects widen mode."
  ;(interactive)
  (save-excursion
    (let ((linenum 0))
      (if (null pt) (setq pt (point)))
      (beginning-of-line)
      (setq linenum (+ 1 (count-lines 1 pt)))
      ;(message "Line number = %d" linenum)
      linenum)))
  
(defun checksum-last-line-number ()
  "Return the line numbe for the last line in the buffer (respect narrow mode).
Notice, the line number returned is the line number respects widen mode."
  ;(interactive)
  (checksum-line-number (point-max)))

(defun checksum-buffer-noeol-show (&optional sum)
  "Return and print a checksum (noeol) for the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	;(widen) ;should use lines number in the current scope (considering narrow)
	(setq sum (checksum-buffer sum t))
	(message "Checksum for buffer (noeol) = %10.0f" sum)
	sum))))


(defun checksum-buffer-show (&optional sum)
  "Return and print a checksum for the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	;(widen) ;should use lines number in the current scope (considering narrow)
	(setq sum (checksum-buffer sum))
	(message "Checksum for buffer = %10.0f" sum)
	sum))))

(defun checksum-region-noeol-show (&optional sum)
  "Return and print a checksum (noeol) for the region selected (whole lines in the region). Currently, the region has to be defined starting from a beginning of a line, and cannot end in a middle of a line. See checksum-region."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	;(widen) ;should use lines number in the current scope (considering narrow)
	(setq sum (checksum-region sum t))
	(message "Checksum for the selected region (noeol) = %10.0f" sum)
  sum
  ))))

(defun checksum-region-show (&optional sum)
  "Return and print a checksum for the region selected (whole lines in the region). Currently, the region has to be defined starting from a beginning of a line, and cannot end in a middle of a line. See checksum-region."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	;(widen) ;should use lines number in the current scope (considering narrow)
	(setq sum (checksum-region sum))
	(message "Checksum for the selected region = %10.0f" sum)
  sum
  ))))

(defun checksum-lines-noeol-show (sline eline &optional sum)
  "Return and print a checksum (noeol) for string between line sline and eline."
  (interactive "nBegin line: \nnEnd line: ")
  (save-excursion
    (save-restriction
      (save-match-data
	;(widen) ;should use lines number in the current scope (considering narrow)
	(setq sum (checksum-lines sline eline sum t))
	(message "Checksum for lines (noeol) %d-%d=%10.0f" sline eline sum)
	sum))))

(defun checksum-lines-show (sline eline &optional sum)
  "Return and print a checksum for string between line sline and eline."
  (interactive "nBegin line: \nnEnd line: ")
  (save-excursion
    (save-restriction
      (save-match-data
	;(widen) ;should use lines number in the current scope (consider narrow)
	(setq sum (checksum-lines sline eline sum))
	(message "Checksum for lines %d-%d=%10.0f" sline eline sum)
	sum))))

(defun checksum-line-noeol-show (&optional sum)
  "Return and print a checksum for the string of the current line.
End-of-line character is excluded, if there is one."
  (interactive)
  (save-excursion
    (save-match-data
      (setq sum (checksum-line sum t))
      (message "Checksum (noeol) of current line=%10.0f" sum)
      sum)))

(defun checksum-line-show (&optional sum)
  "Return and print a checksum for the string of the current line.
End-of-line character is included, if there is one."
  (interactive)
  (save-excursion
    (save-match-data
      (setq sum (checksum-line sum))
      (message "Checksum of current line=%10.0f" sum)
      sum)))

(defun mshow-ch (&optional pos)
  (interactive)
  (let ((ppos pos) ch txt)
    (if (not ppos) (setq ppos (point)))
    (setq ch (char-after ppos))
    (setq txt (char-to-string ch))
    (message "Ch %d(%d)=%s" ch ppos txt)))

(defun end-of-linep ()
  (interactive)
  (let (yes)
    (setq yes (eolp))
    (message "End-of-linep: %s" yes)))

(defun end-of-bufferp ()
  (interactive)
  (let (yes)
    (setq yes (eobp))
    (message "End-of-bufferp: %s" yes)
    yes))

(provide 'checksum)

;;
;;EOF checksum.el
