(defun sort-lst-helper (tail r test &optional (result nil) (is-swapped nil))
  (if (or (zerop r) (endp tail) (endp (cdr tail)))
      (values (append (reverse result) tail) is-swapped)
      (let* ((a  (car tail))
             (b  (cadr tail))
             (ka (car a))
             (kb (car b)))
        (if (funcall test kb ka)
            (sort-lst-helper (cons a (cddr tail)) (1- r) test (cons b result) t)
            (sort-lst-helper (cdr tail) (1- r) test (cons a result) is-swapped)))))


 (defun sort-lst (original-list &key (key #'identity) (test #'<))
  (let ((key-value-lst
          (mapcar (lambda (x)
                    (cons (funcall key x) x))
                  original-list)))
    (labels ((my-sort (lst r)
               (if (<= r 0)
                   lst
                   (multiple-value-bind (result swapped)
                       (sort-lst-helper lst r test)
                     (if swapped
                         (my-sort result (1- r))
                         result)))))
      (mapcar #'cdr (my-sort key-value-lst (1- (length key-value-lst)))))))
      
      
(defun check-sort-lst (name input expected)
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (sort-lst input) expected)
          name))


 (defun test-sort-lst ()
  (check-sort-lst "test 1.0" '(0) '(0))
  (check-sort-lst "test 1.1" '(1) '(1))
  (check-sort-lst "test 1.2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-sort-lst "test 1.3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort-lst "test 1.4" '(3 1 4 1 5 9) '(1 1 3 4 5 9))
  (check-sort-lst "test 1.5" '(10 9 8 8 7 7 6 6 5) '(5 6 6 7 7 8 8 9 10))
  (check-sort-lst "test 1.6" '(1 0.1) '(0.1 1))
  (check-sort-lst "test 1.7" '(0 -1 -16 4) '(-16 -1 0 4)))




(defun rpropaqation-reducer (&key (comparator #'<))
           (lambda (x tail)
             (cond
               ((null tail) (list x))
               ((funcall comparator x (car tail)) (cons x tail))
               (t (cons (car tail) tail)))))
              
               
(defun check-rpropaqation (name input expected &key (comparator #'<))
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (reduce (rpropaqation-reducer :comparator comparator)
                         input
                         :from-end t
                         :initial-value nil)
                 expected)
          name))


(defun test-rpropaqation ()
  (check-rpropaqation "test 1.1" '(3 2 1 2 3) '(1 1 1 2 3))
  (check-rpropaqation "test 1.2" '(3 1 4 2) '(1 1 2 2))
  (check-rpropaqation "test 1.3" '(1 2 3) '(1 2 3))
  (check-rpropaqation "test 2.1" '(3 2 1 2 3) '(3 3 3 3 3) :comparator #'>)
  (check-rpropaqation "test 2.2" '(3 1 4 2) '(4 4 4 2) :comparator #'>)
  (check-rpropaqation "test 2.3" '(1 2 3) '(3 3 3) :comparator #'>))
