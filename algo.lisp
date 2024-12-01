;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2024, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:fprog)

;; non-optimal
(defun fib (n)
  (if (< n 2)
    1
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defun fib-labels (n)
  (labels ((rec (n a b)
             (if (< n 2)
               b
               (rec (- n 1) b (+ a b)))))
    (rec n 1 1)))

(defun fib-loop (n)
  (loop :for a = 1 :then b
        :and b = 1 :then (+ a b)
        :repeat n
        :finally (return a)))

(defun fib-mem (n)
  (let ((h (make-hash-table)))
    (labels ((rec (n)
               (or (gethash n h)
                   (setf (gethash n h)
                         (if (< n 2)
                           1
                           (+ (rec (- n 1))
                              (rec (- n 2))))))))
      (rec n))))

;; non-optimal:
(defun divrem (x y)
  (let ((div (truncate x y))
        (rem (rem x y)))
    (list div rem)))

(defun divrem-values (x y)
  (let ((div (truncate x y))
        (rem (rem x y)))
    (values div rem)))

;;; vim: ft=lisp et
