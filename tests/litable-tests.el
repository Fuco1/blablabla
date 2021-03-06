(require 'instrument-defun)
(require 'thingatpt)

(defmacro litable-test-with-temp-buffer (initial initform &rest forms)
  "Setup a new buffer, then run FORMS.

First, INITFORM are run in the newly created buffer.

Then INITIAL is inserted (it is expected to evaluate to string).
If INITIAL contains | put point there as the initial
position (the character is then removed).  If it contains M, put
mark there (the character is then removed).

Finally, FORMS are run."
  (declare (indent 2)
           (debug (form form body)))
  `(save-window-excursion
     (let ((case-fold-search nil))
       (with-temp-buffer
         (set-input-method nil)
         ,initform
         (pop-to-buffer (current-buffer))
         (insert ,initial)
         (goto-char (point-min))
         (when (search-forward "M" nil t)
           (delete-char -1)
           (set-mark (point))
           (activate-mark))
         (goto-char (point-min))
         (when (search-forward "|" nil t)
           (delete-char -1))
         ,@forms))))


(describe "Navigation helpers"

  (describe "backward-up-list"

    (it "should jump up a sexp"
      (expect
       (litable-test-with-temp-buffer "(foo| bar)" nil
         (litable--backward-up-list)
         (point))
       :to-equal 1))

    (it "should jump up a string"
      (expect
       (litable-test-with-temp-buffer "\"foo| bar\"" nil
         (litable--backward-up-list)
         (point))
       :to-equal 1))

    ;; fixes https://github.com/Fuco1/litable/issues/17
    (it "should jump up a string even if it contains ^(defun)"
      (expect
       (litable-test-with-temp-buffer "\"foo
(defun| bar)\"" nil
         (litable--backward-up-list)
         (point))
       :to-equal 1))

    (it "should work twice in a row"
      (expect
       (litable-test-with-temp-buffer "(asd ad \"foo| bar\")" nil
         (litable--backward-up-list)
         (litable--backward-up-list)
         (point))
       :to-equal 1)))

  (describe "goto-toplevel-form"

    (it "should be able to start in a sexp"
      (expect
       (litable-test-with-temp-buffer "(asd ad (foo| bar))" nil
         (litable--goto-toplevel-form)
         (point))
       :to-equal 1))

    (it "should be able to start in a sexp inside a string"
      (expect
       (litable-test-with-temp-buffer "(asd ad \"(foo| bar)\")" nil
         (litable--goto-toplevel-form)
         (point))
       :to-equal 1))

    (it "should be able to start in a string"
      (expect
       (litable-test-with-temp-buffer "(asd ad \"foo| bar\")" nil
         (litable--goto-toplevel-form)
         (point))
       :to-equal 1))

    (it "should be able to start in a deeply nested sexp"
      (expect
       (litable-test-with-temp-buffer "(asd) (progn (asd) (asd) (asd ad (let ((foo| bar)))))" nil
         (litable--goto-toplevel-form)
         (point))
       :to-equal 7))))


(describe "Defun helpers"

  (describe "litable-point"

    (it "should return point relative to defun's beginning"
      (expect
       (litable-test-with-temp-buffer "(abcdef) (defun| foo () nil)" nil
         (litable-point (list :point 10)))
       :to-equal 6))))


(describe "Instrument variable"

  (it "should instrument rhs variable in setq"
    (expect
     (litable-test-with-temp-buffer "(defun bar () (setq x |a))" nil
       (litable-instrument-variable 'a (list :name 'bar :point 1)))
     :to-equal
      '(litable-variable 22 23 'a a 'bar)))

  (it "should instrument rhs variable in let"
    (expect
     (litable-test-with-temp-buffer "(defun bar () (let ((foo |a)) a))" nil
       (litable-instrument-variable 'a (list :name 'bar :point 1)))
     :to-equal
      '(litable-variable 25 26 'a a 'bar)))

  (it "should instrument variable when it stands alone"
    (expect
     (litable-test-with-temp-buffer "(defun bar () (let ((foo a)) |a))" nil
       (litable-instrument-variable 'a (list :name 'bar :point 1)))
     :to-equal
      '(litable-variable 29 30 'a a 'bar)))

  (it "should instrument variable when it is used inside a form"
    (expect
     (litable-test-with-temp-buffer "(defun bar () (let ((foo a)) (1+ |a)))" nil
       (litable-instrument-variable 'a (list :name 'bar :point 1)))
     :to-equal
      '(litable-variable 33 34 'a a 'bar))))

(describe "Instrument setq"

  (it "should instrument setq with single variable"
    (expect
     (litable-test-with-temp-buffer "(defun bar () |(setq x a))" nil
       (litable-instrument-setq '(setq x a) (list :name 'bar :point 1)))
     :to-equal
      '(setq x
             (litable-variable 20 21 'x
                               (litable-variable 22 23 'a a 'bar)
                               'bar 'font-lock-warning-face))))

  (it "should instrument with single variable and complex expression"
    (expect
     (litable-test-with-temp-buffer "(defun bar () |(setq x (progn (foo))))" nil
       (litable-instrument-setq '(setq x (progn (foo))) (list :name 'bar :point 1)))
     :to-equal
      '(setq x
             (litable-variable 20 21 'x
                               (progn
                                 (foo))
                               'bar 'font-lock-warning-face))))

  (it "should instrument with single variable and complex expression involving a variable"
    (expect
     (litable-test-with-temp-buffer "(defun bar () |(setq x (progn (foo a))))" nil
       (litable-instrument-setq '(setq x (progn (foo a))) (list :name 'bar :point 1)))
     :to-equal
      '(setq x
             (litable-variable 20 21 'x
                               (progn
                                 (foo
                                  (litable-variable 34 35 'a a 'bar)))
                               'bar 'font-lock-warning-face))))

  (it "should instrument lhs and rhs of setq with multiple variables"
    (expect
     (litable-test-with-temp-buffer "(defun bar () |(setq x a y b))" nil
       (litable-instrument-setq '(setq x a y b) (list :name 'bar :point 1)))
     :to-equal
      '(setq x
             (litable-variable 20 21 'x
                               (litable-variable 22 23 'a a 'bar)
                               'bar 'font-lock-warning-face)
             y
             (litable-variable 24 25 'y
                               (litable-variable 26 27 'b b 'bar)
                               'bar 'font-lock-warning-face))))

  (it "should instrument lhs and rhs of setq with multiple variables with complex expressions"
    (expect
     (litable-test-with-temp-buffer "(defun bar () |(setq x (+ (foo) (bar)) y (apply '+ list)))" nil
       (litable-instrument-setq '(setq x (+ (foo) (bar)) y (apply '+ list)) (list :name 'bar :point 1)))
     :to-equal
      '(setq x
             (litable-variable 20 21 'x
                               (+
                                (foo)
                                (bar))
                               'bar 'font-lock-warning-face)
             y
             (litable-variable 38 39 'y
                               (apply '+
                                      (litable-variable 50 54 'list list 'bar))
                               'bar 'font-lock-warning-face)))))


(describe "Instrument function"

  (it "should instrument the argument list"
    (expect
     (litable-test-with-temp-buffer "|(lambda (a b) b)" nil
       (litable--instrument-function '(lambda (a b) b) (list :name 'foo :point 1)))
     :to-equal
      '(lambda
         (a b)
         (progn
           (litable-variable 9 10 'a a 'foo 'font-lock-variable-name-face)
           (litable-variable 11 12 'b b 'foo 'font-lock-variable-name-face))
         (litable-variable 14 15 'b b 'foo))))

  (it "should handle put the instrumented arglist form after the docstring"
    (expect
     (litable-test-with-temp-buffer "|(lambda (a) \"doc\" t)" nil
       (litable--instrument-function '(lambda (a) "doc" t) (list :name 'foo :point 1)))
     :to-equal
      '(lambda (a) "doc"
         (progn
           (litable-variable 9 10 'a a 'foo 'font-lock-variable-name-face)) t)))

  (it "should handle put the instrumented arglist form after the docstring in a defun"
    (expect
     (litable-test-with-temp-buffer "|(defun foo (a) \"doc\" t)" nil
       (litable--instrument-function '(defun foo (a) "doc" t) (list :name 'foo :point 1)))
     :to-equal
      '(lambda (a) "doc"
         (progn
           (litable-variable 12 13 'a a 'foo 'font-lock-variable-name-face)) t)))

  (it "should skip the docstring before instrumenting the body"
    (expect
     (litable-test-with-temp-buffer "|(defun foo (a) \"doc\" nil a)" nil
       (litable--instrument-function '(defun foo (a) "doc" nil a) (list :name 'foo :point 1)))
     :to-equal
      '(lambda (a) "doc"
         (progn
           (litable-variable 12 13 'a a 'foo 'font-lock-variable-name-face))
         nil
         (litable-variable 25 26 'a a 'foo))))

  (it "should skip the interactive form before instrumenting the body"
    (expect
     (litable-test-with-temp-buffer "|(defun foo (a) (interactive \"P\") nil a)" nil
       (litable--instrument-function '(defun foo (a) (interactive "P") nil a)
                                     (list :name 'foo :point 1)))
     :to-equal
      '(lambda (a)
         (interactive "P")
         (progn
           (litable-variable 12 13 'a a 'foo 'font-lock-variable-name-face))
         nil
         (litable-variable 37 38 'a a 'foo))))

  (it "should skip the docstring and interactive form before instrumenting the body"
    (expect
     (litable-test-with-temp-buffer "|(defun foo (a) \"doc\" (interactive \"P\") nil a)" nil
       (litable--instrument-function '(defun foo (a) "doc" (interactive "P") nil a)
                                     (list :name 'foo :point 1)))
     :to-equal
      '(lambda (a)
         "doc"
         (interactive "P")
         (progn
           (litable-variable 12 13 'a a 'foo 'font-lock-variable-name-face))
         nil
         (litable-variable 43 44 'a a 'foo)))))


(describe "Instrument let"

  (it "should instrument single let declaration"
    (expect
     (litable-test-with-temp-buffer "|(let ((foo a)))" nil
       (litable--instrument-let
        '(let ((foo a)))
        (list :name 'bar :point 1)))
     :to-equal
      '(let
           ((foo
             (litable-variable 7 10 'foo
                               (litable-variable 11 12 'a a 'bar)
                               'bar 'font-lock-warning-face))))))

  (it "should instrument two let declarations"
    (expect
     (litable-test-with-temp-buffer "|(let ((foo a) (bar a)))" nil
       (litable--instrument-form
        '(let ((foo a) (bar a)))
        (list :name 'bar :point 1)))
     :to-equal
      '(let
           ((foo
             (litable-variable 7 10 'foo
                               (litable-variable 11 12 'a a 'bar)
                               'bar 'font-lock-warning-face))
            (bar
             (litable-variable 15 18 'bar
                               (litable-variable 19 20 'a a 'bar)
                               'bar 'font-lock-warning-face))))))

  (it "should instrument an atom in the body of the let declaration"
    (expect
     (litable-test-with-temp-buffer "|(let ((foo a)) a)" nil
       (litable--instrument-form
        '(let ((foo a)) a)
        (list :name 'bar :point 1)))
     :to-equal
      '(let
           ((foo
             (litable-variable 7 10 'foo
                               (litable-variable 11 12 'a a 'bar)
                               'bar 'font-lock-warning-face)))
         (litable-variable 15 16 'a a 'bar))))

  (it "should instrument a list in the body of the let declaration"
    (expect
     (litable-test-with-temp-buffer "|(let ((foo a)) (progn a))" nil
       (litable--instrument-form
        '(let ((foo a)) (progn a))
        (list :name 'bar :point 1)))
     :to-equal
      '(let
           ((foo
             (litable-variable 7 10 'foo
                               (litable-variable 11 12 'a a 'bar)
                               'bar 'font-lock-warning-face)))
         (progn
           (litable-variable 22 23 'a a 'bar))))))

(describe "Instrument form"

  (it "should instrument a regular form"
    (expect
     (litable-test-with-temp-buffer "|(progn x)" nil
       (litable--instrument-form
        '(progn x)
        (list :name 'bar :point 1)))
     :to-equal
      '(progn (litable-variable 7 8 'x x 'bar))))

  (it "should not instrument a macro"
    (expect
     (litable-test-with-temp-buffer "|(pop x)" nil
       (litable--instrument-form
        '(pop x)
        (list :name 'bar :point 1)))
     :to-equal
      '(pop x))))
