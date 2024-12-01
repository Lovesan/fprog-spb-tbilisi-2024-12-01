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

(define-constant +fibs-for-64-bit+
    (coerce
     (loop :for i :from 0
           :for fib = (fib-loop i)
           :while (typep fib '(unsigned-byte 64))
           :collect fib)
     'vector)
  :test 'equalp)

(deftype fib64-n ()
  (list 'integer 0 (1- (length +fibs-for-64-bit+))))

(defun fib-const (n)
  (declare (type fib64-n n))
  (aref +fibs-for-64-bit+ n))

(defun fib-with-compiler-macro (n)
  (fib-loop n))

(define-compiler-macro fib-with-compiler-macro (n)
  (if (constantp n)
    (fib-loop (eval n))
    `(fib-loop ,n)))

(defun two-arg-mul (x y)
  (* x y))

(defun mul (&rest args)
  (loop :with acc = 1
        :for arg :in args
        :do (setf acc (two-arg-mul acc arg))
        :finally (return acc)))

(define-compiler-macro mul (&rest args)
  (let ((n-args (length args)))
    (case n-args
      (0 1)
      (1 (first args))
      (t `(two-arg-mul
           (mul ,@(subseq args 0 (floor n-args 2)))
           (mul ,@(subseq args (floor n-args 2))))))))

;;; vim: ft=lisp et
