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