;;; instrument-defun.el --- Instrument defuns to add litable behaviour. -*- lexical-binding: t -*-

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 21st March 2016
;; Package-requires: ((dash "2.10.0") (ov "1.0"))
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'ov)

(require 'edebug)

;; TODO: rename `data' to `state'
(defun litable-point (data)
  "Return point relative to defun's beginning.

DATA is the data plist."
  (- (point) (plist-get data :point)))

(defun litable-instrument-arglist (variables data)
  "Instrument arglist with VARIABLES."
  (save-excursion
    (down-list)
    `(progn
       ,@(mapcar
          (lambda (var)
            (if (memq var '(&optional &rest &key))
                (progn
                  (forward-symbol 2)
                  (forward-symbol -1))
              `(litable-variable
                ,(litable-point data)
                ,(progn
                   (forward-symbol 1)
                   (prog1 (litable-point data)
                     (forward-symbol 1)
                     (forward-symbol -1)))
                ',var
                ,var
                ',(plist-get data :name)
                'font-lock-variable-name-face)))
          variables))))

(defun litable-instrument-variable (variable data)
  "Instrument a single VARIABLE."
  (save-excursion
    `(litable-variable
      ,(litable-point data)
      ,(progn
         (forward-symbol 1)
         (litable-point data))
      ',variable
      ,variable
      ',(plist-get data :name))))

(defun litable-instrument-setq (setq-form data)
  "Instrument a SETQ-FORM."
  (save-excursion
    (down-list)
    (forward-sexp)
    `(setq
      ,@(-mapcat
         (-lambda ((var def))
           (forward-symbol 1)
           (forward-symbol -1)
           (let ((beg (litable-point data))
                 (end (progn (forward-sexp) (litable-point data))))
             ;; we need to leave the variable as first argument to
             ;; setq, so instead we wrap the next form with the code
             ;; to fontify the preceding variable.
             (list var `(litable-variable
                         ,beg ,end ',var
                         ,(litable--instrument-defun def data)
                         ',(plist-get data :name)
                         'font-lock-warning-face))))
         (-partition 2 (cdr setq-form))))))

(defun litable--instrument-let (let-form data)
  "Instrument a LET-FORM.

DATA is the instrumentation state."
  (save-excursion
    (down-list 2)
    `(,(car let-form)
      ,(-map
        (-lambda ((var def))
          (down-list)
          (forward-symbol 1)
          (forward-symbol -1)
          (prog1 (let ((beg (litable-point data))
                       (end (progn (forward-sexp) (litable-point data))))
                   ;; we need to leave the variable as first argument to let
                   ;; ("the place"), so instead we wrap the next form with
                   ;; the code to fontify the preceding variable.
                   (list var `(litable-variable
                               ,beg ,end ',var
                               ,(litable--instrument-defun def data)
                               ',(plist-get data :name)
                               'font-lock-warning-face)))
            (up-list)))
        (cadr let-form))
      ,@(progn
          (up-list)
          (mapcar (lambda (f) (litable--instrument-defun f data))
                  (cddr let-form))))))

(defun litable--instrument-defun (form data)
  "FORM."
  (cond
   ((consp form)
    (cond
     ((eq 'quote (car form))
      (forward-sexp)
      form)
     ((eq 'setq (car form))
      (prog1 (litable-instrument-setq form data)
        (forward-sexp)))
     ((memq (car form) '(let let*))
      (prog1 (litable--instrument-let form data)
        (forward-sexp)))
     ;; TODO: add replacement of `lambda' form arguments... at best
     ;; reuse this code, or extract the common body into
     ;; `litable--instrument-lambda'
     ((eq 'defun (car form))
      (down-list)
      (forward-sexp 2)
      (let ((arglist (litable-instrument-arglist (caddr form) data)))
        (forward-sexp 1)
        (-cons*
         'lambda
         (caddr form)
         ;; we need to put the form after the optional docstring
         (if (stringp (cadddr form))
             (-cons*
              (cadddr form)
              arglist
              (mapcar (lambda (f) (litable--instrument-defun f data))
                      (cddddr form)))
           (cons
            arglist
            ;; TODO: couldn't we do without the mapcar here? Add a
            ;; shortcut to do the unwraping or implement it
            ;; recursively even on the first argument... such that
            ;; ((a) (b...)) goes down to `a'. This needs to be
            ;; implemented in the `t' branch
            (mapcar (lambda (f) (litable--instrument-defun f data))
                    (cdddr form)))))))
     (t
      (down-list)
      (forward-sexp)
      (prog1 (cons
              (car form)
              (mapcar (lambda (f) (litable--instrument-defun f data)) (cdr form)))
        (up-list)))))
   ((or (stringp form)
        (vectorp form)
        (numberp form)
        (and (symbolp form)
             (or (eq form t)
                 (eq form nil))))
    (forward-sexp)
    form)
   (t
    (forward-symbol 1)
    (forward-symbol -1)
    (prog1 (litable-instrument-variable form data)
      (forward-symbol 1)))))

;; TODO: save relative offsets against defun's top position so we dont
;; have to reinstrument every time defun moves around
(defun litable-instrument-defun ()
  "Instrument defun after point."
  (let* ((def (sexp-at-point))
         ;; We thread data through the instrumentation pipeline to
         ;; carry local information about the defun, like its name
         (data (list :point (point)
                     :name (cadr def))))
    (save-excursion
      (fset (cadr def)
            (eval (litable--instrument-defun def data) lexical-binding)))
    (save-excursion
      (let ((beg (point))
            (end (progn
                   (end-of-defun)
                   (point))))
        (put (cadr def) 'litable-defun-beg beg)
        (put (cadr def) 'litable-defun-end end)
        (put (cadr def) 'litable-defun-definition
             (buffer-substring-no-properties beg end))))))

;; TODO: add "form id" to 'litable property so we can only
;; clear/update specific form's overlays
;; TODO: no overlays/replacement should happen if the litable-mode is
;; not on
(defun litable-variable (beg end var value defname &optional face)
  "Add litable overlay over a variable.

BEG, END is the overlay range, VAR is the variable symbol and
VALUE is its current value in the context where this function is
called.  DEFNAME is the name of the defun.
"
  (setq face (or face 'font-lock-type-face))
  (ov (+ (get defname 'litable-defun-beg) beg)
      (+ (get defname 'litable-defun-beg) end)
      'litable t
      'face face
      ;; TODO: add better formatter
      'display (format "%s{%S}" (symbol-name var)
                       (cond
                        ((and (consp value)
                              (eq (car value) 'lambda))
                         "#<lambda>")
                        ((and (consp value)
                              (eq (car value) 'closure))
                         "#<closure>")
                        (t value))))
  value)

(defun litable--goto-toplevel-form ()
  "Go to toplevel form around the point."
  (while (/= (car (syntax-ppss)) 0) (litable--backward-up-list)))

(defun litable--backward-up-list ()
  "Like `backward-up-list' but works inside strings too."
  (interactive)
  (if (nth 3 (syntax-ppss))
      (while (nth 3 (syntax-ppss))
        (backward-char))
    (backward-up-list)))

(defun litable2-refresh (&optional a b c)
  "A B C."
  (when a
    (ignore-errors
      (let ((form (save-excursion
                    (litable--goto-toplevel-form)
                    (sexp-at-point))))
        (ov-clear 'litable)
        (when (listp form)
          ;; re-instrument all defuns which have changed
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "^(defun " nil t)
              (skip-syntax-forward " ")
              (let ((function-name (symbol-at-point))
                    (beg (progn (beginning-of-line) (point)))
                    (end (save-excursion (end-of-defun) (point))))
                (if (equal
                     (get function-name 'litable-defun-definition)
                     (buffer-substring-no-properties beg end))
                    ;; update the beg/end
                    (progn
                      (put function-name 'litable-defun-beg beg)
                      (put function-name 'litable-defun-end end))
                  (message "Reinstrument defun %s" function-name)
                  (litable-instrument-defun)))
              (end-of-defun)))
          (unless (memq (car form) '(defun defmacro cl-defun cl-defmacro))
            (eval form)))))))

(defun litable2-init ()
  "Initialize litable in the buffer."
  (add-hook 'after-change-functions 'litable2-refresh nil t)
  ;; (add-hook 'post-command-hook 'litable-update-defs-if-moved nil t)
  )

(defun litable2-stop ()
  "Stop litable in the buffer."
  (remove-hook 'after-change-functions 'litable2-refresh t)
  ;; (remove-hook 'post-command-hook 'litable-update-defs-if-moved t)
  (ov-clear 'litable))

(defvar litable-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap eval-defun] 'litable-eval-defun)
    map)
  "litable mode map.")

(defun litable-eval-defun (edebug-it)
  "Eval defun and instrument it with litable info."
  (interactive "P")
  (if edebug-it
      (edebug-eval-defun t)
    (edebug-eval-defun nil)
    (save-excursion
      (end-of-defun)
      (beginning-of-defun)
      (litable-instrument-defun))))

;; TODO: if we turn the mode off, ideally we should un-instrument the
;; defuns back to whatever they were before
;;;###autoload
(define-minor-mode litable2-mode
  "Toggle litable2-mode"
  :lighter " litable2"
  :keymap litable-mode-map
  :group 'litable
  (if litable2-mode
      (litable2-init)
    (litable2-stop)))

(provide 'instrument-defun)
;;; instrument-defun.el ends here
