;; see side-effect-free-fns in emacs-lisp/byte-opt.el for list of pure
;; functions

(defun instrument-defun-1 (form vars &optional context)
  (cond
   ((listp form)
    (let ((vars (cond
                 ((memq (car form) '(let let*))
                  (-union (litable--extract-variables (cadr form))
                          vars))
                 ((eq (car form) 'setq)
                  (-union (cadr form) vars))
                 (t vars))))
      (cond
       ((listp (car form))
        (down-list)
        (prog1 (mapcar (lambda (x) (instrument-defun-1 x vars ctx)) form)
          (up-list)))
       (t
        (down-list)
        (litable--next-sexp)
        (let ((ctx (cond
                    ((memq (car form) '(let let*))
                     'let))))
          (prog1 (cond
                  ((eq context 'let)
                   `(,(car form)
                     (litable-let-body ',(car form)
                                       ,@(mapcar (lambda (x) (instrument-defun-1 x vars ctx)) (cdr form))
                                       ,(- (save-excursion (backward-sexp 2) (point)) def-start)
                                       ',def-name)))
                  (t
                   `(,(car form)
                     ,@(if (eq ctx 'let)
                           (append
                            (mapcar (lambda (x) (instrument-defun-1 x vars ctx)) (list (cadr form)))
                            (mapcar (lambda (x) (instrument-defun-1 x vars)) (cddr form)))
                         (mapcar (lambda (x) (instrument-defun-1 x vars ctx)) (cdr form))))))
            (up-list)))))))
   (t
    (prog1 (if (memq form vars)
               `(litable-variable ',form ,form ,(- (point) def-start) ',def-name)
             form)
      (litable--next-sexp)))))

(defun instrument-defun ()
  (save-excursion
    (let* ((def (sexp-at-point))
           (def-start (point-marker))
           (def-name (cadr def))
           (instrumented
            (progn
              (down-list)
              (forward-sexp 3)
              `(lambda ,(caddr def)
                 ;; handle docstring & interactive properly!
                 (litable-args ,(- (save-excursion (backward-sexp) (point)) def-start)
                               ',def-name
                               ',(caddr def)
                               ,@(caddr def))
                 ,@(litable-modify-last
                    (mapcar (lambda (x) (instrument-defun-1 x '(x y))) (cdddr def))
                    (lambda (form)
                      `(litable-end ',def-name ,form)))))))
      (put def-name :litable-defun-start def-start)
      (fset def-name instrumented))))

(defun litable-modify-last (list fun)
  (let* ((last (-last-item list))
         (list (nbutlast list)))
    (-snoc list (funcall fun last))))

(defun litable-args (pos def-name names &rest values)
  (save-excursion
    (let ((pos (+ pos (get def-name :litable-defun-start)))
          (pairs (-zip names values)))
      (goto-char pos)
      (down-list)
      (--each pairs
        (create-subst-overlay (point)
                              (+ (point) (length (symbol-name (car it))))
                              (format "%s{%s}" (symbol-name (car it)) (cdr it)))
        (litable--next-sexp))
      (litable--print-input values
                            (progn
                              (end-of-line)
                              (point))
                            'font-lock-variable-name-face)))
  t)

(defun litable-end (def-name value)
  (save-excursion
    (goto-char (get def-name :litable-defun-start))
    (forward-sexp)
    (litable--print-result value (point) 'font-lock-constant-face))
  value)

(defun litable-update-defs (&optional a b c)
  (litable-remove-overlays)
  (when a
    (ignore-errors
      (let ((form (save-excursion
                    (litable-goto-toplevel-form)
                    (sexp-at-point))))
        (eval form)))))

(defun litable-variable (name value pos def-name)
  (let ((pos (+ pos (get def-name :litable-defun-start))))
    (create-subst-overlay pos
                          (+ pos (length (symbol-name name)))
                          (format "%s{%s}" (symbol-name name) value)))
  value)

(defun litable-let-body (name value pos def-name)
  (let ((pos (+ pos (get def-name :litable-defun-start))))
    (create-subst-overlay pos
                          (+ pos (length (symbol-name name)))
                          (format "%s{%s}" (symbol-name name) value)
                          font-lock-warning-face))
  value)

(defun create-subst-overlay (start end value &optional face)
  "Create the overlay that shows the substituted value."
  ;; TODO: make the face customizable
  (setq face (or face 'font-lock-type-face))
  (let (o (print-quoted t))
    (setq o (make-overlay start end))
    (push o litable-overlays)
    (litable--set-overlay-priority o)
    (overlay-put o 'display
                 (propertize
                  ;; TODO: extract this format into customize
                  ;; TODO: customize max-length
                  ;; for the subexpression, then
                  ;; cut off and replace with
                  ;; "bla..."
                  value
                  'face face))))
