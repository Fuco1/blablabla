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

(defun litable-instrument-arglist (variables)
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
                ,(point)
                ,(progn
                   (forward-symbol 1)
                   (prog1 (point)
                     (forward-symbol 1)
                     (forward-symbol -1)))
                ',var
                ,var)))
          variables))))

(defun litable-instrument-variable (variable)
  "Instrument a single VARIABLE."
  (save-excursion
    `(litable-variable
      ,(point)
      ,(progn
         (forward-symbol 1)
         (point))
      ',variable
      ,variable)))

(defun litable-instrument-setq (setq-form)
  "Instrument a SETQ-FORM."
  (save-excursion
    (down-list)
    (forward-sexp)
    `(setq
      ,@(-mapcat
         (-lambda ((var def))
           (forward-sexp)
           (list var (litable--instrument-defun def)))
         (-partition 2 (cdr setq-form))))))

(defun litable--instrument-defun (form &optional data)
  "FORM."
  (cond
   ((consp form)
    (cond
     ((eq 'quote (car form))
      (forward-sexp)
      form)
     ((eq 'setq (car form))
      (forward-sexp)
      (litable-instrument-setq form))
     ((eq 'defun (car form))
      (down-list)
      (forward-sexp 2)
      (let ((arglist (litable-instrument-arglist (caddr form))))
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
            (mapcar (lambda (f) (litable--instrument-defun f data))
                    (cdddr form)))))))
     (t
      (down-list)
      (forward-sexp)
      (prog1 (cons
              (car form)
              (mapcar (lambda (f) (litable--instrument-defun f)) (cdr form)))
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
    (prog1 (litable-instrument-variable form)
      (forward-symbol 1)))))

(defun litable-instrument-defun ()
  "Instrument defun after point."
  (let* ((def (sexp-at-point)))
    (save-excursion
      (fset (cadr def) (eval (litable--instrument-defun def) lexical-binding)))))

(defun litable-variable (beg end var value)
  "Add litable overlay over a variable.

BEG, END is the overlay range, VAR is the variable symbol and
VALUE is its current value in the context where this function is
called."
  (ov beg end
      'litable t
      'face 'font-lock-type-face
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

(provide 'instrument-defun)
;;; instrument-defun.el ends here
