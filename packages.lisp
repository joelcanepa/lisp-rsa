(in-package :cl)

(defpackage rsm.mod
  (:use :cl)
  (:export :inverse)
  (:documentation "This package contains functions to do modular arithmetic"))

(defpackage :cl-primality
  (:use :cl)
  (:export :primep :miller-rabin :expt-mod :coprime)
  (:documentation  "This package contains functions to do the miller rabin 
  primality test and modular exponentiation"))

(defpackage :rsa
  (:use :cl :rsm.mod :cl-primality)
  (:export :generar-claves
           :print-rsa
           :p
           :q
           :n
           :phi-n
           :e
           :d
           :encrypt
           :decrypt)
  (:documentation "This package contains an implementation of asymmetric rsa encryption."))