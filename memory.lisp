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

(deftype vec3 () '(simple-array single-float (3)))

(declaim (inline vec3))
(defun vec3 (x y z)
  (make-array 3 :element-type 'single-float
                :initial-contents (list x y z)))

(defun vec3-1-1-1 ()
  (load-time-value (vec3 1.0 1.0 1.0) t))

(declaim (inline vec3-length))
(defun vec3-length (vec)
  (declare (type vec3 vec))
  (let ((x (aref vec 0))
        (y (aref vec 1))
        (z (aref vec 2)))
    (sqrt (+ (* x x)
             (* y y)
             (* z z)))))

(defun length3 (x y z)
  (declare (type single-float x y z)
           (optimize speed (safety 0)))
  (let ((vec (vec3 x y z)))
    (declare (dynamic-extent vec))
    (vec3-length vec)))

(declaim (start-block))

(defun fib-tailrec (n a b)
  (declare (type fixnum n)
           (type integer a b))
  (if (< n 2)
    b
    (fib-tailrec (- n 1) b (+ a b))))

(defun fib-block (n)
  (fib-tailrec n 1 1))

(declaim (end-block))

;;; vim: ft=lisp et
