(in-package :rsa)

;; definition of rsa parameters
(setq p nil)

(setq q nil)

(setq n nil)

(setq phi-n nil)

(setq e nil)

(setq d nil)

;; Generates a pseudo random number of n bits (between zero and 2^n bits)
(defun random-n-bits (bits)
  (random (expt 2 bits) (make-random-state t)))

;; returns (with an error probability of 2^-100) a prime number
;; by NIST FIPS 186-5 Digital Signature Standard
(defun generate-prime (bits)
  ;; k = number of miller rabin test rounds to perform
  (setq k 0)
  (cond
     ((eq bits 512)  (setq k 7))
     ((eq bits 1024) (setq k 4))
     ((eq bits 1536) (setq k 3))
     ((eq bits 2048) (setq k 2))
  )
  ;; loops until a number passes the miller rabin primality test
  (loop
     (setq number (random-n-bits bits))
  (when (eq (miller-rabin  number k) number) (return number))))

;; calculates parameter n
(defun calc-n (p q)
  (* p q))

;; calculates parameter phi-n using carmichael's function
;; assuming that p and q are prime numbers, then the function can be
;; calculated as the least common multiple between p-1 and q-1
(defun fun-carmichael (p q)
  (lcm (- p 1) (- q 1)))

;; calculates parameter e (the public key)
;; seeks a random number that satisfies:
;; 1<e<phi-n where e and phi-n are coprime numbers
(defun calc-e (phi-n)
  (loop
     (setq e (+ (random (- phi-n 1) (make-random-state t)) 1))
  (when (eq (coprime phi-n e) t) (return e))))

;; calculates parameter d (the private key)
;; finds the modular inverse 'd' of e = 1 mod phi-n
(defun calc-d (phi-n e)
  (inverse e phi-n))

;; Calculates all rsa parameters
(defun generar-claves (bits)
  (format t "Generating rsa keys...~%")
  (setq p (generate-prime bits))
  (setq q (generate-prime bits))
  (setq n (calc-n p q))
  (setq phi-n (fun-carmichael p q))
  (setq e (calc-e phi-n))
  (setq d (calc-d phi-n e))
  (format t "Key generation completed.~%"))

;; rsa encryption
(defun encrypt (m e n)
  (expt-mod m e n))

;; rsa decryption
(defun decrypt (c d n)
  (expt-mod c d n))

;; outputs all rsa parameters
(defun print-rsa ()
  (format t "p: ~A~%" p)
  (format t "q: ~A~%" q)
  (format t "n: ~A~%" n)
  (format t "phi-n: ~A~%" phi-n)
  (format t "e: ~A~%" e)
  (format t "d: ~A~%" d))
