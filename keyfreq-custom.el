;; keyfreq-custom
;; Customizations to keyfreq.el

;; Copyright 2013

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
;;Variables related to custom-keyfreq-show-func
;;;;;;;

(defvar keyfreq-use-custom-keyfreq-show-func t
  "Whether or not we should use a custom keyfreq show func")

(defvar keyfreq-custom-show-func-max-command-length 1
  "The max command length to display in our custom-keyfreq-show-func")

(defvar keyfreq-custom-show-func-unmodify-keys t
  "Whether or not to convert modified keys, e.g. C-x, M-y, to unmodified ones, e.g. x, y.

Useful for making heat maps, since most heat-map makers don't recognize C- or M-.")

;;;;;;;
;;Variables related to custom-keyfreq-list 
;;;;;;;

(defvar keyfreq-use-custom-keyfreq-list t
  "Whether or not we should use a custom keyfreq list")

(defvar keyfreq-custom-keyfreq-list-max-command-length 1
  "The max command length to allow in our custom-keyfreq-list")

(defvar keyfreq-custom-keyfreq-list-show-inserts nil
  "Whether or not our custom keyfreq list should show self-insert-commands")

(defvar keyfreq-custom-keyfreq-list-show-backspace nil
  "Whether or not our custom keyfreq list should show delete-backward-char")

(defvar custom-dontcheck-command
  '(undefined
    scroll-bar-toolkit-scroll
    mouse-set-point
    evil-mouse-drag-region
    mwheel-scroll
    mouse-save-then-kill
    mouse-set-region
    mouse-drag-mode-line
    )
"Commands for custom-keyfreq-list not to count")

;;;;;;;
;;The actual functions
;;;;;;;

(defun keyfreq-custom-replace-modifiers-in-string (s)
  "Replaces C- and M- in string s with \"\""
  (replace-regexp-in-string "C-" "" (replace-regexp-in-string "M-"
                                                              ""
                                                              s)))

(defun keyfreq-custom-key-description-length-leq (max-length keybindings)
  "Returns a key description of keybindings <= max-length, or \"\" if none exists.

If max-length nil, then any length is acceptable.

If keyfreq-custom-show-func-unmodify-keys is true, then C- and M- are replaced with \"\"."
  (let* ((key-descriptions (mapcar 'key-description keybindings))
         (converted-key-descriptions (mapcar (lambda (s)
                                               (if keyfreq-custom-show-func-unmodify-keys
                                                   (keyfreq-custom-replace-modifiers-in-string s)
                                                 s))
                                             key-descriptions))
         (short-key-descriptions (remove-if-not (lambda (s)
                                                  (or (not max-length)
                                                      (<= (length s)
                                                          max-length)))
                                                converted-key-descriptions)))
    (if short-key-descriptions
      (car short-key-descriptions)
      "")))

(defun exists-keyfreq-custom-key-description-length-leq (max-length keybindings)
  "Whether or not a keybinding of length <= max-length (or, if max-length nil, any length) exists in keybindings.

If keyfreq-custom-show-func-unmodify-keys is true, then C- and M- do not add anything to string's length."
  (not (equal "" (keyfreq-custom-key-description-length-leq max-length keybindings))))

(defvar custom-keyfreq-show-func 
  (lambda (num percent command)
    (format "%7d  %6.2f%% %s %s\n" 
            num 
            percent 
            (let* ((keybindings (where-is-internal command))
                   (padlength (max 7 keyfreq-custom-show-func-max-command-length))
                   (padder  (lambda (s) (let ((spaces (lambda (len) (make-string len ?\s))))
                                          (if (<= (length s) padlength)
                                              (let* ((lengthDifference (- padlength (length s)))
                                                     (leftPadLength (/ lengthDifference 2))
                                                     (rightPadLength (- lengthDifference leftPadLength)))
                                                (concat (funcall spaces leftPadLength) 
                                                        s 
                                                        (funcall spaces rightPadLength)))
                                            (funcall spaces padlength))))))
              (cond ((equal command 'self-insert-command) (funcall padder "various"))
                    ((equal command 'undefined) (funcall padder "unknown"))
                    (keybindings (funcall padder (keyfreq-custom-key-description-length-leq keyfreq-custom-show-func-max-command-length
                                                                                            keybindings)))
                    (t (funcall padder ""))))
            ;; (mapcar 'key-description (where-is-internal command) )
            command )))



;;custom keyfreq-list function for converting keyfreq table to list
(defvar custom-keyfreq-list 
  (lambda (table &optional reverse limit)
    (let (l (sum 0))
      (maphash
       (lambda (k v) 
         (let ((keybindings (where-is-internal k)))
           (unless (or (not keybindings)
                       (and (not keyfreq-custom-keyfreq-list-show-inserts)
                            (equal k 'self-insert-command))
                       (and (not keyfreq-custom-keyfreq-list-show-backspace)
                            (equal k 'delete-backward-char))
                       (member k custom-dontcheck-command)
                       (not (exists-keyfreq-custom-key-description-length-leq keyfreq-custom-keyfreq-list-max-command-length
                                                                              keybindings)))
             (setq l (cons (cons k v) l) sum (+ sum v)))))
       table)
      (cons sum
            (cond
             ((equal reverse 'no-sort) l)
             (reverse (sort l (lambda (a b) (< (cdr a) (cdr b)))))
             (t       (sort l (lambda (a b) (> (cdr a) (cdr b))))))))))

(provide 'keyfreq-custom)
