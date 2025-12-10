<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
<em> Червоноокий Дмитро КВ-21</em></p>

<p align="right"><b>Рік</b>: <em>2025</em></p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
- використати функції вищого порядку для роботи з послідовностями (де це
доречно);
- додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.

## Варіант першої частини (3)

Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

### Лістинг реалізації першої частини завдання

```lisp
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
```

### Тестові набори та утиліти першої частини

```lisp
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
```

### Тестування першої частини

```lisp
CL-USER> (test-sort-lst)
PASSED... test 1.0
PASSED... test 1.1
PASSED... test 1.2
PASSED... test 1.3
PASSED... test 1.4
PASSED... test 1.5
PASSED... test 1.6
PASSED... test 1.7
NIL
```

## Варіант другої частини (6)
Написати функцію rpropagation-reducer, яка має один ключовий параметр — функцію comparator. rpropagation-reducer має повернути функцію, яка при застосуванні в якості першого аргумента reduce робить наступне: при обході списку з кінця, якщо елемент списку-аргумента reduce не «кращий» за попередній (той, що «справа») згідно з comparator, тоді він заміняється на значення попереднього, тобто «кращого», елемента. Якщо ж він «кращий» за попередній елемент згідно comparator, тоді заміна не відбувається. Функція comparator за замовчуванням має значення #'<. Обмеження, які накладаються на використання функції-результату rpropagation-reducer при передачі у reduce, визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів функції reduce from-end та initial-value).


```lisp
CL-USER> (reduce (rpropagation-reducer)
'(3 2 1 2 3)
:from-end ...
:initial-value ...)
(1 1 1 2 3)
CL-USER> (reduce (rpropagation-reducer)
'(3 1 4 2)
:from-end ...
:initial-value ...)
(1 1 2 2)
CL-USER> (reduce (rpropagation-reducer :comparator #'>)
'(1 2 3)
:from-end ...
:initial-value ...)
(3 3 3)
```

### Лістинг реалізації другої частини завдання

```lisp
(defun rpropaqation-reducer (&key (comparator #'<))
           (lambda (x tail)
             (cond
               ((null tail) (list x))
               ((funcall comparator x (car tail)) (cons x tail))
               (t (cons (car tail) tail)))))
```

### Тестові набори та утиліти другої частини

```lisp
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
```

### Тестування другої частини

```lisp
CL-USER> (test-rpropaqation)
PASSED... test 1.1
PASSED... test 1.2
PASSED... test 1.3
PASSED... test 2.1
PASSED... test 2.2
PASSED... test 2.3
NIL
```
