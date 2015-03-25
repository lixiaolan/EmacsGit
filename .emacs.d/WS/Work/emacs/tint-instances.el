;;; tint-instances.el -- Tint all instances of the current word.
;;; Copyright 1997 by Bruno Daniel and Uwe Schabe

;; Author:    Bruno Daniel  <daniel@fzi.de, http://www.uni-karlsruhe.de/~ub68/>
;;            Uwe Schabe    <uwe@cad-cam-concept.de>
;; Created:   July 1997
;; Version:   1.0 
;; Keywords:  tint-instances, tinting, highlighting   
;; Availability: newsgroup "gnu.emacs.sources" and archives thereof

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;; *CW customized - including copy facemenu-get-face from older versioin of facemenu.el

;; This emacs script takes the word where point is currently on and tints
;; all instances of this word in the current buffer.
;; If the region is active, it is taken as the current word to be tinted.
;;
;;
;; Quickstart Installation:
;; ========================
;;
;; To get this to work, make emacs execute the line
;;
;; (require 'tint-instances)			;; load the package
;;
;; I would also recommend executing the following commands
;; so as to extend the bindings in your global keymap:
;;
;; (define-key global-map "\M-c" 'tint_all_instances_of_current_word)
;; (define-key global-map "\M-v" 'untint_all_instances_of_current_word)
;;
;; If there are problems with the colors used, change the entries in the
;; color_array to your favourite colors (you can list all available colors
;; of your installation by the command "M-x list-colors-display")
;;
;;; Code:

;*CW++ 6/08 copied from older version of facemenu.el (1996) - this function no longer exists in the newer version (2008)
(defun facemenu-get-face (symbol)
  "Make sure FACE exists.
If not, create it and add it to the appropriate menu.  Return the SYMBOL.

If a window system is in use, and this function creates a face named
`fg:color', then it sets the foreground to that color.  Likewise, `bg:color'
means to set the background.  In either case, if the color is undefined,
no color is set and a warning is issued."
  (let ((name (symbol-name symbol))
	foreground)
    (cond ((facep symbol))
	  ((and (display-color-p)
		(or (setq foreground (string-match "^fg:" name))
		    (string-match "^bg:" name)))
	   (let ((face (make-face symbol))
		 (color (substring name 3)))
	     (if (x-color-defined-p color)
		 (if foreground
		     (set-face-foreground face color)
		   (set-face-background face color))
	       (message "Color \"%s\" undefined" color))))
	  (t (make-face symbol))))
  symbol)

(defun current_word ()
  "Return the word where point is currently on.
Dependent on the major mode punctuations are considered as part of the word:
emacs-lisp-mode:  '_' '-'
other modes:      '_'

If the region is active, it's taken instead." 

  (interactive)
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (save-excursion 
      (forward-word 1)
      ;; In `emacs-lisp-mode' the chartacters '_' and '-'
      ;; are considered as part of the word.
      (if (eq major-mode 'emacs-lisp-mode)
	  (progn
	    (while (progn
		     (forward-word -1)
		     (or (char-equal (preceding-char) ?_)
			 (char-equal (preceding-char) ?-))
		     ))
	    (setq last_word (point))
	    (while (progn
		     (forward-word 1)
		     (or (char-equal (following-char) ?_)
			 (char-equal (following-char) ?-))
		     ))
	    (setq next_word (point)))
	;; In all other modes (especially C modes),
	;; only the chartacter '_' is considered as part of the word.
	(while (progn
		 (forward-word -1)
		 (char-equal (preceding-char) ?_)))
	(setq last_word (point))
	(while (progn
		 (forward-word 1)
		 (char-equal (following-char) ?_)))
	(setq next_word (point))
	)
      (buffer-substring-no-properties last_word next_word))))


; Painting everything red is a bit boring. So, here is a small array of
; colors in order to distinguish different highlightings.
;(setq color_array ["Yellow"  "OrangeRed" "SkyBlue" "PaleGreen" "Plum"
;                   "Orange"  "Cyan" "Firebrick" "LightBlue"  "YellowGreen"
;                   "Pink"])

;(setq color_array ["Firebrick" ]) ;*CW 8/10/2001 just use one color

;*CW+3 choose different color, TODO: is there a way to also use different forcolor depending on each backcolor? >> There are now highlight-regexp, highlight-phrase provided in hi-lock.el.
(setq color_array ["RoyalBlue"  "OrangeRed" "goldenrod" "SeaGreen" "DarkViolet"
                   "DarkOrange"  "cyan4" "Firebrick"
                   "HotPink"])

; The index indicating the current color
(setq current_color_index 0)


(defun tint_all_instances_of_current_word ()
  "Tints all instances of the word in the current buffer point is on.
   If the region is active it's taken instead."
  (interactive)
  (let* ((word_was_marked mark-active)
	 (desired_face (facemenu-get-face
			(intern (concat "bg:" (aref color_array
						    current_color_index)))))
	 (curr_word (current_word))
	 (word_length (length curr_word)))
    (save-excursion
      (beginning-of-buffer)
      ;; Search for `curr_word', ignoring differences in punctuation.
      ;; If the region wasn't active, perform word search.
      (while (if word_was_marked (search-forward curr_word nil t)
	       (word-search-forward curr_word nil t))
	(let* ((begin_of_word (- (point) word_length))
	       (char_before_begin_of_word (char-after (1- begin_of_word))))
	  ;; But punctuation is significant!
	  ;; In `emacs-lisp-mode', if `curr_word' is preceded or followed
	  ;; by the characters '_' or '-', it is part of another word.
	  ;; Don't tint it!
	  (when (or word_was_marked
		    (not (or (char-equal (following-char) ?_)
			     (char-equal char_before_begin_of_word ?_)
			     (and (eq major-mode 'emacs-lisp-mode)
				  (or (char-equal (following-char) ?-)
				      (char-equal char_before_begin_of_word ?-)
				      )))))
			 (overlay-put (make-overlay begin_of_word (point))
				      'face desired_face))
	    ))))
  (setq current_color_index (1+ current_color_index))
  (if (>= current_color_index (length color_array))
      (setq current_color_index 0)))


(defun untint_all_instances_of_current_word ()
  "Delete all overlay-colors of all instances of the current word."
  (interactive)
  (let ((word_was_marked mark-active)
	(curr_word (current_word)))
    (save-excursion
      (beginning-of-buffer)
      (while (if word_was_marked (search-forward curr_word nil t)
	       (word-search-forward curr_word nil t))
	(mapcar 'delete-overlay (overlays-at (- (point) 1)))))))

;;*CW added
(defun tint_all_instances_of_string (string)
  "Tints all instances of the word in the current buffer.
   If the region is active it's taken instead."
  (interactive "sHighlight all instances of string: ")
  (let* ((desired_face (facemenu-get-face
			(intern (concat "bg:" (aref color_array
						    current_color_index)))))  ;*CW 7->current_color_index
	 (curr_word string)
	 (word_length (length curr_word))
         toverlay) ;*CW 5/02 use overlay with priority to see if it works with ediff
    (save-excursion
      (beginning-of-buffer)
      ;; Search for `curr_word', ignoring differences in punctuation.
      ;; If the region wasn't active, perform word search.
      (while (search-forward curr_word nil t)
	(let* ((begin_of_word (- (point) word_length)))
          (setq toverlay (make-overlay begin_of_word (point)))
          (overlay-put toverlay 'face desired_face)
          (overlay-put toverlay 'priority 110)
          ;(overlay-put (make-overlay begin_of_word (point)) 
	  ;			      'face desired_face)
          ))))
  (setq current_color_index (1+ current_color_index))
  (if (>= current_color_index (length color_array))
      (setq current_color_index 0))
  )

(defun untint_all_instances_of_string (string)
  "Delete all overlay-colors of all instances of the string."
  (interactive "sClear all overlay-colors of all instances of string: ")
  (save-excursion
    (beginning-of-buffer)
      (while (search-forward string nil t)
	(mapcar 'delete-overlay (overlays-at (- (point) 1))))))

(define-key global-map "\M-s" 'tint_all_instances_of_string)
(define-key global-map "\M-S" 'untint_all_instances_of_string)


(provide 'tint-instances)
;; tint-instances.el ends here


