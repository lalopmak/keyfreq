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

(defvar keyfreq-custom-show-modifiers t
  "Whether to show modifiers, e.g. C-x, M-y, or drop the C-, M-.

Useful for making heat maps, since most heat-map makers don't recognize C- or M-.")

(defvar keyfreq-custom-show-spaces t
  "Whether or not to show spaces in the display.

Useful to set to nil when making heat maps.")

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

(defvar keyfreq-custom-use-keyfreq-custom-list t
  "Whether or not we should use a custom keyfreq list")

(defvar keyfreq-custom-list-max-command-length 11
  "The max command length to allow in our keyfreq-custom-list")

(defvar keyfreq-custom-list-include-inserts nil
  "Whether or not our custom keyfreq list should include self-insert-commands")

(defvar keyfreq-custom-list-include-backspace nil
  "Whether or not our custom keyfreq list should include delete-backward-char")

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
"Commands for keyfreq-custom-list not to count")

;;;;;;;
;;The actual functions
;;;;;;;

(defun keyfreq-custom-replace-modifiers-in-string (s)
  "Replaces C- and M- in string s with \"\""
  (replace-regexp-in-string "C-" "" (replace-regexp-in-string "M-"
                                                              ""
                                                              s)))

(defun keyfreq-custom-replace-spaces-in-string (s)
  "Replaces \" \" in string s with \"\""
  (replace-regexp-in-string " " "" s))

(defun keyfreq-custom-key-description-length-leq (max-length keybindings)
  "Returns a key description of keybindings <= max-length, or \"\" if none exists.

If max-length nil, then any length is acceptable.

If keyfreq-custom-show-modifiers is true, then C- and M- are replaced with \"\"."
  (let* ((key-descriptions (mapcar 'key-description keybindings))
         (converted-key-descriptions (mapcar (lambda (s)
                                               (if keyfreq-custom-show-modifiers
                                                   s
                                                 (keyfreq-custom-replace-modifiers-in-string s)))
                                             key-descriptions))
         (unspaced-key-descriptions (mapcar (lambda (s)
                                               (if keyfreq-custom-show-spaces
                                                   s
                                                 (keyfreq-custom-replace-spaces-in-string s)))
                                             converted-key-descriptions))
         (short-key-descriptions (remove-if-not (lambda (s)
                                                  (or (not max-length)
                                                      (<= (length s)
                                                          max-length)))
                                                unspaced-key-descriptions)))
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

(defvar keyfreq-custom-show-func 
  (lambda (num percent command)
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
            command )))



;;custom keyfreq-list function for converting keyfreq table to list
(defvar keyfreq-custom-list 
  (lambda (table &optional reverse limit)
    (let (l (sum 0))
      (maphash
       (lambda (k v) 
         (let ((keybindings (where-is-internal k)))
           (unless (or (not keybindings)
                       (and (not keyfreq-custom-list-include-inserts)
                            (equal k 'self-insert-command))
                       (and (not keyfreq-custom-list-include-backspace)
                            (equal k 'delete-backward-char))
                       (member k keyfreq-custom-dontcheck-command)
                       (not (keyfreq-custom-exists-key-description-length-leq keyfreq-custom-list-max-command-length
                                                                              keybindings)))
             (setq l (cons (cons k v) l) sum (+ sum v)))))
       table)
      (cons sum
            (cond
             ((equal reverse 'no-sort) l)
             (reverse (sort l (lambda (a b) (< (cdr a) (cdr b)))))
             (t       (sort l (lambda (a b) (> (cdr a) (cdr b))))))))))

(provide 'keyfreq-custom)
