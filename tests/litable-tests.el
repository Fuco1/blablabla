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


;; TODO: add separate tests for let as well? Or only keep top-level
;; itegration tests
(describe "Instrument defun"

  (it "should instrument single let declaration"
    (expect
     (litable-test-with-temp-buffer "|(defun bar () (let ((foo a))))" nil
       (litable--instrument-defun
        '(defun bar () (let ((foo a))))
        (list :name 'bar :point 1)))
     :to-equal
      '(lambda nil
         (progn)
         (let
             ((foo
               (litable-variable 21 24 'foo
                                 (litable-variable 25 26 'a a 'bar)
                                 'bar 'font-lock-warning-face)))))))

  (it "should instrument two let declarations"
    (expect
     (litable-test-with-temp-buffer "|(defun bar () (let ((foo a) (bar a))))" nil
       (litable--instrument-defun
        '(defun bar () (let ((foo a) (bar a))))
        (list :name 'bar :point 1)))
     :to-equal
      '(lambda nil
         (progn)
         (let
             ((foo
               (litable-variable 21 24 'foo
                                 (litable-variable 25 26 'a a 'bar)
                                 'bar 'font-lock-warning-face))
              (bar
               (litable-variable 29 32 'bar
                                 (litable-variable 33 34 'a a 'bar)
                                 'bar 'font-lock-warning-face)))))))

  (it "should instrument an atom in the body of the let declaration"
    (expect
     (litable-test-with-temp-buffer "|(defun bar () (let ((foo a)) a))" nil
       (litable--instrument-defun
        '(defun bar () (let ((foo a)) a))
        (list :name 'bar :point 1)))
     :to-equal
      '(lambda nil
         (progn)
         (let
             ((foo
               (litable-variable 21 24 'foo
                                 (litable-variable 25 26 'a a 'bar)
                                 'bar 'font-lock-warning-face)))
           (litable-variable 29 30 'a a 'bar)))))

  (it "should instrument a list in the body of the let declaration"
    (expect
     (litable-test-with-temp-buffer "|(defun bar () (let ((foo a)) (progn a)))" nil
       (litable--instrument-defun
        '(defun bar () (let ((foo a)) (progn a)))
        (list :name 'bar :point 1)))
     :to-equal
      '(lambda nil
         (progn)
         (let
             ((foo
               (litable-variable 21 24 'foo
                                 (litable-variable 25 26 'a a 'bar)
                                 'bar 'font-lock-warning-face)))
           (progn
             (litable-variable 36 37 'a a 'bar)))))))
