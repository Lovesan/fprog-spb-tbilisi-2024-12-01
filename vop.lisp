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

(deftype index () '(integer 0 #.(1- array-total-size-limit)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown popcnt (fixnum) fixnum
      (movable flushable foldable always-translatable)
    :overwrite-fndb-silently t)

  (define-vop (popcnt)
    (:translate popcnt)
    (:args (x :scs (signed-reg) :target rv))
    (:arg-types fixnum)
    (:results (rv :scs (signed-reg)))
    (:result-types fixnum)
    (:policy :fast-safe)
    (:generator 1
      (inst popcnt rv x))))

(defun popcnt (x)
  (popcnt x))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-vop (memcpy)
    (:args (dest :scs (sap-reg) :target rv)
           (src :scs (sap-reg))
           (count :scs (unsigned-reg)))
    (:info backwardp)
    (:arg-types system-area-pointer
                system-area-pointer
                positive-fixnum
                (:constant *))
    (:temporary (:sc unsigned-reg :offset rdi-offset) rdi)
    (:temporary (:sc unsigned-reg :offset rsi-offset) rsi)
    (:temporary (:sc unsigned-reg :offset rcx-offset) rcx)
    (:results (rv :scs (sap-reg)))
    (:result-types system-area-pointer)
    (:policy :fast-safe)
    (:generator 1
      (move rv dest)
      (move rdi dest)
      (move rsi src)
      (move rcx count)
      (if backwardp
        (inst std)
        (inst cld))
      (inst rep)
      (inst movs :byte))))

(progn
  (declaim (inline memcpy))
  (defun memcpy (dest src count)
    (declare (type system-area-pointer dest src)
             (type index count))
    (%primitive memcpy dest src count nil))

  (declaim (inline memmove))
  (defun memmove (dest src count)
    (declare (type system-area-pointer dest src)
             (type index count))
    (if (sap<= dest src)
      (%primitive memcpy dest src count nil)
      (%primitive memcpy dest src count t)))

  (declaim (inline copy-byte-vector))
  (defun copy-byte-vector (dest src &key (start1 0)
                                         end1
                                         (start2 0)
                                         (end2))
    (declare (type index start1 start2)
             (type (or null index) end1 end2)
             (type (vector (unsigned-byte 8)) dest src))
    (let* ((end1 (or end1 (length dest)))
           (end2 (or end2 (length src)))
           (count (max 0 (min (- end1 start1)
                              (- end2 start2)))))
      (with-pinned-objects (dest src)
        (let ((pdest (sap+ (vector-sap dest) start1))
              (psrc (sap+ (vector-sap src) start2)))
          (memmove pdest psrc count)
          dest)))))

;;; vim: ft=lisp et
