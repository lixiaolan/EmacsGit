(defun ljj-open-selected-in-buffer () 
  "This takes selected text and opens it in a new buffer at point."
  (interactive)
  (setq ljj-child-buffer "ljj-child-buffer")		; Default scratch buffer
  (setq ljj-text (buffer-substring (mark) (point)))
  (get-buffer-create ljj-child-buffer)
  (setq ljj-parrent-buffer (current-buffer))
  (set-buffer ljj-child-buffer)
  (insert ljj-text)
  (javascript-mode)
  (uncomment-region (point-min) (point-max))
  (xml-mode)
  (local-set-key "\C-c'" 'ljj-replace-selected-with-buffer)
  (make-local-variable 'ljj-parrent-buffer)
  (switch-to-buffer ljj-child-buffer)
  t)

(defun ljj-replace-selected-with-buffer ()
  "This takes all the text from a buffer and replaces selected
  region with that text"
  (interactive)
  (setq ljj-child-buffer "ljj-child-buffer")	       	; Default scratch buffer
  (set-buffer ljj-child-buffer)
  (javascript-mode)
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


/// <summary>
///   This adds two number
/// </summary>
/// <param type="integer" name="a">
///   this is the first number!
/// </param>
/// <param>
///   This is the second number!
/// </param>


















