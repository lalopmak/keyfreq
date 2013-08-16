;; keyfreq-custom
;; Customizations to keyfreq.el

;; Copyright 2013 - lalop

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;;;;;;
;;General variables
;;;;;;;

(defvar keyfreq-custom-C-replacement nil
  "How to display C- in strings.
nil means the default setting of the function (usually \"C-\" or \"\")")

(defvar keyfreq-custom-M-replacement nil
  "How to display M- in strings.
nil means the default setting of the function (usually \"C-\" or \"\")")

(defvar keyfreq-custom-show-spaces t
  "Whether or not to show spaces in the display.

Useful to set to nil when making heat maps.")

(defvar keyfreq-custom-require-keybindings t
  "If true, only include commands that have keybindings.")

;;;;;;;
;;Variables related to keyfreq-custom-show-func
;;;;;;;

(defvar keyfreq-custom-use-show-func t
  "Whether or not we should use a custom keyfreq show func")

(defvar keyfreq-custom-show-func-max-command-length 11
  "The max command length to display in our keyfreq-custom-show-func")

;;;;;;;
;;Variables related to keyfreq-custom-list 
;;;;;;;
(defvar keyfreq-custom-filter-list t
  "Whether or not to filter keyfreq-list by keyfreq-custom-accept-command")

(defvar keyfreq-custom-use-keyfreq-custom-list t

  "Whether or not we should use a custom keyfreq list")

(defvar keyfreq-custom-list-max-command-length 11
  "The max command length to allow in our keyfreq-custom-list")

(defvar keyfreq-custom-list-include-inserts t
  "Whether or not our custom keyfreq list should include self-insert-commands")

(defvar keyfreq-custom-list-include-backspace t
  "Whether or not our custom keyfreq list should include delete-backward-char")

(defvar keyfreq-custom-list-include-representations nil
  "Whether or not our custom keyfreq list should include items of the form <backspace>, DEL")

(defvar keyfreq-custom-dontcheck-command
  '(undefined
    scroll-bar-toolkit-scroll
    mouse-set-point
    evil-mouse-drag-region
    mwheel-scroll
    mouse-save-then-kill
    mouse-set-region
    mouse-drag-mode-line
    )
"Commands for keyfreq-custom-list not to include.")

;;;;;;;
;;The actual functions
;;;;;;;

(defun keyfreq-custom-convert-modifiers-in-string (s)
  "Replaces C- and M- in string s with \"\""
  (replace-regexp-in-string "C-" 
                            (or keyfreq-custom-C-replacement "C-") 
                            (replace-regexp-in-string "M-"
                                                      (or keyfreq-custom-M-replacement "M-")
                                                      s)))

(defun keyfreq-custom-replace-spaces-in-string (s)
  "Replaces \" \" in string s with \"\""
  (replace-regexp-in-string " " "" s))

(defun keyfreq-custom-key-description-length-leq (max-length keybindings)
  "Returns a key description of keybindings <= max-length, or \"\" if none exists.

If max-length nil, then any length is acceptable.

If keyfreq-custom-show-modifiers is true, then C- and M- are replaced with \"\"."
  (let* ((key-descriptions (mapcar 'key-description keybindings))
         (converted-key-descriptions (mapcar 'keyfreq-custom-convert-modifiers-in-string
                                             key-descriptions))
         (unspaced-key-descriptions (mapcar (lambda (s)
                                               (if keyfreq-custom-show-spaces
                                                   s
                                                 (keyfreq-custom-replace-spaces-in-string s)))
                                             converted-key-descriptions))
         (unrepresented-key-descriptions (remove-if (lambda (s)
                                                      (and (not keyfreq-custom-list-include-representations)
                                                           (or (and (string-match "<" s)
                                                                    (string-match ">" s))
                                                               (string-match "DEL" s))))
                                                    unspaced-key-descriptions)) 
         (short-key-descriptions (remove-if-not (lambda (s)
                                                  (or (not max-length)
                                                      (<= (length s)
                                                          max-length)))
                                                unrepresented-key-descriptions)))
    (if short-key-descriptions
      (car short-key-descriptions)
      "")))

(defun keyfreq-custom-exists-key-description-length-leq (max-length keybindings)
  "Whether or not a keybinding of length <= max-length (or, if max-length nil, any length) exists in keybindings.

If keyfreq-custom-show-modifiers is nil, then C- and M- do not add anything to string's length."
  (not (equal "" (keyfreq-custom-key-description-length-leq max-length keybindings))))

(defun make-whitespace (&optional len)
  "Makes a string of all spaces \" \", length len.  

If len nil, defaults to 1."
  (make-string (or len 1) ?\s))

(defun keyfreq-custom-show-func (num percent command)
  "Custom version of show function used in keyfreq-show; feel free to overload."
    (format "%7d  %6.2f%% %s %s\n" 
            num 
            percent 
            (let* ((keybindings (where-is-internal command))
                   (padlength (max 7 keyfreq-custom-show-func-max-command-length))
                   (padder  (lambda (s) (if (<= (length s) padlength)
                                            (let* ((lengthDifference (- padlength (length s)))
                                                   (leftPadLength (/ lengthDifference 2))
                                                   (rightPadLength (- lengthDifference leftPadLength)))
                                              (concat (make-whitespace leftPadLength) 
                                                      s 
                                                      (make-whitespace rightPadLength)))
                                          (make-whitespace padlength)))))
              (cond ((equal command 'self-insert-command) (funcall padder "various"))
                    ((equal command 'undefined) (funcall padder "undefined"))
                    (keybindings (funcall padder (keyfreq-custom-key-description-length-leq keyfreq-custom-show-func-max-command-length
                                                                                            keybindings)))
                    (t (funcall padder ""))))
            ;; (mapcar 'key-description (where-is-internal command) )
            command ))

(defun not-equal (o1 o2)
  (not (equal o1 o2)))

(defun keyfreq-custom-accept-command (command)
  "Lets the command through in accordance to keyfreq-custom constants"
  (let ((keybindings (where-is-internal command)))
    (and (or (not keyfreq-custom-require-keybindings)
             keybindings)
         (or keyfreq-custom-list-include-inserts
             (not-equal command 'self-insert-command))
         (or keyfreq-custom-list-include-backspace
             (not-equal command 'delete-backward-char))
         (not (member command keyfreq-custom-dontcheck-command))
         (keyfreq-custom-exists-key-description-length-leq keyfreq-custom-list-max-command-length
                                                           keybindings)))) 



;;custom keyfreq-list function for converting keyfreq table to list
(defun keyfreq-custom-list (table &optional reverse limit)
  "Custom version of keyfreq-list; feel free to change/overload"
    (let (l (sum 0))
      (maphash
       (lambda (k v) 
         (when (keyfreq-custom-accept-command k)
           (setq l (cons (cons k v) l) sum (+ sum v))))
       table)
      (cons sum
            (cond
             ((equal reverse 'no-sort) l)
             (reverse (sort l (lambda (a b) (< (cdr a) (cdr b)))))
             (t       (sort l (lambda (a b) (> (cdr a) (cdr b)))))))))

(provide 'keyfreq-custom)
