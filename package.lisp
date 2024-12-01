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

(in-package #:cl-user)

(uiop:define-package #:fprog
  (:use #:cl #:global-vars)
  (:import-from #:alexandria
                #:define-constant)
  (:import-from #:sb-c
                #:defknown
                #:deftransform
                #:define-vop
                #:movable
                #:foldable
                #:flushable
                #:always-translatable
                #:location=
                #:move
                #:tn-p)
  (:import-from #:sb-assem
                #:inst
                #:gen-label
                #:emit-label)
  (:import-from #:sb-ext
                #:simd-pack
                #:simd-pack-256
                #:start-block
                #:end-block
                #:*block-compile-default*)
  (:import-from #:sb-vm
                #:n-word-bits
                #:n-word-bytes)
  (:import-from #:sb-sys
                #:sap<=
                #:sap+
                #:system-area-pointer
                #:vector-sap
                #:with-pinned-objects)
  (:export
   ))

(in-package #:fprog)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import `(sb-vm::single-reg
                      sb-vm::double-reg
                      sb-vm::descriptor-reg
                      sb-vm::any-reg
                      sb-vm::signed-reg
                      sb-vm::unsigned-reg
                      sb-vm::sap-reg
                      sb-vm::int-sse-reg
                      sb-vm::single-sse-reg
                      sb-vm::double-sse-reg
                      sb-vm::int-avx2-reg
                      sb-vm::single-avx2-reg
                      sb-vm::double-avx2-reg
                      sb-vm::rax-offset
                      sb-vm::rbx-offset
                      sb-vm::rcx-offset
                      sb-vm::rdx-offset
                      ,@(loop :for i :from 8 :to 15
                              :collect (intern (format nil "R~A-OFFSET" i) '#:sb-vm))
                      sb-vm::rip-tn
                      sb-vm::rsi-offset
                      sb-vm::rdi-offset
                      sb-vm::rbp-offset
                      sb-vm::rsp-offset
                      ,@(loop :for i :from 0 :to 15
                              :collect (intern (format nil "FLOAT~a-OFFSET" i) '#:sb-vm))
                      sb-vm::ea
                      sb-vm::index-scale
                      sb-c::%primitive
                      sb-vm::positive-fixnum
                      )))

;;; vim: ft=lisp et
