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
  "Instrument arglist with VARIABLES.

DATA is the instrumentation state."
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
  "Instrument a single VARIABLE.

DATA is the instrumentation state."
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
  "Instrument a SETQ-FORM.

DATA is the instrumentation state."
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
                         ,(litable--instrument-form def data)
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
                               ,(litable--instrument-form def data)
                               ',(plist-get data :name)
                               'font-lock-warning-face)))
            (up-list)))
        (cadr let-form))
      ,@(progn
          (up-list)
          (litable--instrument-form-body (cddr let-form) data)))))

(defun litable--instrument-form-body (form data)
  "Instrument a FORM's body.

FORM is a list of forms and we instrument each child form recursively.

DATA is the instrumentation state."
  (mapcar (lambda (f) (litable--instrument-form f data)) form))

(defun litable--instrument-function (form data)
  "Instrument a lambda FORM.

FORM can either contain a `lambda' or a `defun'.

DATA is the instrumentation state."
  (when (eq (car form) 'defun)
    (setq form (cons (car form) (cddr form))))
  (save-excursion
    (down-list)
    (forward-sexp (if (eq (car form) 'defun) 2 1))
    (let ((arglist-raw (cadr form))
          (arglist (litable-instrument-arglist (cadr form) data))
          (front-matter nil))
      (forward-sexp) ;; skip arglist
      (setq form (cddr form)) ;; drop lambda/defun and arglist
      ;; skip docstring
      (when (stringp (car form))
        (forward-sexp)
        (push (pop form) front-matter))
      ;; skip interactive spec
      (when (and (consp (car form))
                 (eq (caar form) 'interactive))
        (forward-sexp)
        (push (pop form) front-matter))
      `(lambda
         ,arglist-raw
         ,@(nreverse front-matter)
         ,arglist
         ,@(litable--instrument-form-body form data)))))

(defun litable--wrap-function-result (form data)
  "Wrap FORM with `litable-result'.

FORM is a lambda expression.

DATA is the instrumentation state."
  (let* ((form (cdr form))
         (err (make-symbol "err"))
         (p (litable-point data))
         (name (plist-get data :name))
         (args (pop form))
         (front-matter nil))
    (when (stringp (car form))
      (push (pop form) front-matter))
    (when (and (consp (car form))
               (eq (caar form) 'interactive))
      (push (pop form) front-matter))
    `(lambda
       ,args
       ,@front-matter
       (condition-case ,err
           (litable-result
            (progn
              ,@form)
            nil ,p ',name)
         (error (litable-error
                 (error-message-string ,err) nil ,p ',name)
                (signal (car ,err) (cdr ,err)))))))

(defun litable--instrument-form (form data)
  "FORM."
  (cond
   ((consp form)
    (cond
     ((eq 'quote (car form))
      (forward-sexp)
      form)
     ((eq 'condition-case (car form))
      (down-list)
      (forward-sexp 2)
      (prog1 (-cons*
              (car form)
              (cadr form)
              (litable--instrument-form-body (cddr form) data))
        (up-list)))
     ((eq 'setq (car form))
      (prog1 (litable-instrument-setq form data)
        (forward-sexp)))
     ((memq (car form) '(let let*))
      (prog1 (litable--instrument-let form data)
        (forward-sexp)))
     ((memq (car form) '(lambda defun))
      (let ((function-form (litable--instrument-function form data)))
        (forward-sexp)
        (litable--wrap-function-result function-form data)))
     (t
      (down-list)
      (forward-sexp)
      (prog1 (cons
              (car form)
              (litable--instrument-form-body (cdr form) data))
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

(defun litable-instrument-defun ()
  "Instrument defun after point."
  (let* ((def (sexp-at-point))
         ;; We thread data through the instrumentation pipeline to
         ;; carry local information about the defun, like its name
         (data (list :point (point)
                     :name (cadr def))))
    (save-excursion
      (fset (cadr def)
            (eval (litable--instrument-form def data) lexical-binding)))
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

;; TODO: `litable-result' and `litable-error' are exactly the same
;; except the face and format string => remove the duplication?
(defun litable-result (value &optional face beg defname)
  "Print VALUE in FACE as a result after the current form.

If BEG and DEFNAME are set, print the value after the function."
  (setq face (or face 'font-lock-keyword-face)
        beg (if beg
                (+ (get defname 'litable-defun-beg) beg)
              (save-excursion
                (end-of-defun)
                (backward-char)
                (point))))
  (ov beg beg
      'litable t
      'before-string (propertize
                      (format " => %S" value)
                      'face face))
  value)

(defun litable-error (error-message &optional face beg defname)
  (setq face (or face 'font-lock-warning-face)
        beg (if beg
                (+ (get defname 'litable-defun-beg) beg)
              (save-excursion
                (end-of-defun)
                (backward-char)
                (point))))
  (ov beg beg
      'litable t
      'before-string (propertize
                      (format " => %s" error-message)
                      'face face))
  error-message)

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

(defun litable-refresh (&optional a b c)
  "A B C."
  (when a
    (ignore-errors
      (let ((form (save-excursion
                    (litable--goto-toplevel-form)
                    (sexp-at-point))))
        (ov-clear 'litable)
        (when (and form (listp form))
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
            (condition-case err
                (litable-result (eval form))
              (error (litable-error (error-message-string err))))))))))

(defun litable-init ()
  "Initialize litable in the buffer."
  (add-hook 'after-change-functions 'litable-refresh nil t))

(defun litable-stop ()
  "Stop litable in the buffer."
  (remove-hook 'after-change-functions 'litable-refresh t)
  (ov-clear 'litable))

(defvar litable-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap eval-defun] 'litable-eval-defun)
    map)
  "Keymap for `litable-mode'.")

(defun litable-eval-defun (edebug-it)
  "Eval defun and instrument it with litable info.

With EDEBUG-IT use `edebug-eval-defun' instead of
`litable-eval-defun'."
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
(define-minor-mode litable-mode
  "Toggle litable-mode"
  :lighter " litable"
  :keymap litable-mode-map
  :group 'litable
  (if litable-mode
      (litable-init)
    (litable-stop)))

(provide 'instrument-defun)
;;; instrument-defun.el ends here
