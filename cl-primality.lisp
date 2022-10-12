(in-package :cl-primality)

;; @\section{Introduction}

;; @CL-Primality is a small library that can test whether integers are prime or
;; not, and perform common actions that require that knowledge.  As of now, the
;; implementation is based on the Miller-Rabin probabilistic primality
;; algorithm.  It is written with some speed considerations in mind.

;; @\section{Utilities}

;; The sort of number theoretical calculations involved in primality testing
;; typically need some support for modular arithmetic.  We introduce the
;; functions <<*-mod>> and <<expt-mod>>, which perform multiplication and
;; exponentiation modulo some number.

;;<<>>=
(defun *-mod (n m md)
  "Multiply N by M, modulo MD."
  (mod (* n m) md))

(defun expt-mod-ref (b e md)
  "A reference implementation, do not use except for testing purposes."
  (mod (expt b e) md))

;;<<>>=
(defun expt-mod (b e md &optional (tot 1))
  "Raise B to the power of E, modulo MD \(leave TOT as 1)."
  (declare (type integer e))
  (cond ((= e 0) tot)
        ((oddp e)
         (expt-mod (mod (* b b) md)
                   (ash e -1)
                   md
                   (mod (* tot b) md)))
        (t (expt-mod (mod (* b b) md)
                     (ash e -1)
                     md
                     tot))))

;; @According to Euler's theorem, $a^b \mod m = a^{b \mod \phi(m)} \mod m$,
;; where $phi(m)$ is Euler's totient function equal to the number of integers
;; coprime to $m$ in the range
;; $[1,m]$.\footnote{https://stackoverflow.com/questions/11448658/modular-exponentiation}
;; For a prime number, $\phi(m) = m$.  Euler's totient product formula says
;; that\footnote{https://en.wikipedia.org/wiki/Euler%27s_totient_function#Euler.27s_product_formula}:

;; \[
;; \phi(n) = n \prod_{p|n}\left(1 - \frac 1 p \right)
;; \]

;; ... where the product is over all integers that divide $n$ including $1$ and $p$. This requires 

;; (defun eulers-totient (n)
;;   (if (and nil (primep n))
;;       n
;;       ;; Fall back to a slow method of calculating
;;       (let ((tot 0))
;;         (iter (for i :below n)
;;           (unless (> (gcd i (mod n (if (> i 0) i n))) 1)
;;             (incf tot))
;;           (finally (return tot))))))

;; (defun %expt-mod (b e md)
;;   "Raise B to the power of E, modulo MD \(leave TOT as 1)."
;;   (declare (type integer e))
;;   (let ((e (mod e (eulers-totient md))))
;;     (expt-mod b e md)))

;; @\section{Primality Algorithms}

;; @\subsection{The Miller-Rabin Algorithm}

;; The Miller-Rabin algorithm is a common implementation for primality testing.
;; It performs a probabilistic check for primality that gives guarantees on the
;; maximum likelihood of a false positive (a number identified as prime when it
;; is actually composite) and never gives false negatives (a number identified
;; as composite whin it is in fact prime).  This value is set via the optional
;; parameter <chance-of-error>.  By default, <chance-of-error> is set to a very
;; small number which can slow things down if you don't need that strong of a
;; guarantee.

;;<<,2>>=

;; se modifico la funcion original para que en lugar de recibir la chance de falso positivo
;; reciba el numero de iteraciones o pasadas a realizar del algoritmo rabin-miller
(defun miller-rabin (n &optional (iterations 10))
  "Miller-Rabin probabilistic primality test:

Checks if N is prime with the chance of a false positive less than
CHANCE-OF-ERROR.  This algorithm never gives false negatives."
;; la probabilidad de tener un falso positivo es de (ceiling (log chance-of-error 1/4))
  (declare (optimize (speed 3) (debug 0)))
  (cond ((= n 1) nil)
        ((= n 2) n)
        ;; n-iter define el numero de iteraciones a realizar del algoritmo miller-rabin
        (t (let ((n-iter iterations))
             (labels
                 ((rec (n n-iter)
                    (cond ((= n-iter 0) n)
                          (t (and (miller-rabin-pass n (1+ (random (- n 1))))
                                  (rec n (- n-iter 1)))))))
               (rec n n-iter))))))

(defun miller-rabin-pass (n a)
  "Performs one 'pass' of the Miller-Rabin primality algorithm."
  (declare (optimize (speed 3) (debug 0))
           (inline miller-rabin-pass))
  (labels ((decompose-val (n s)
             (cond ((or (= n 0) (oddp n)) (values n s))
                   (t (decompose-val (/ n 2) (1+ s))))))
    (multiple-value-bind (d s) (decompose-val (- n 1) 0)
         (cond ((= 1 (expt-mod a d n)) n)
               ((do* ((a-loc (expt-mod a d n) (expt-mod a-loc 2 n))
                      (i 0 (1+ i))
                      (ret (= (- n 1) a-loc) (= (- n 1) a-loc)))
                     ((or ret (= i s)) (if (/= i s) t))) n)
               (t nil)))))

;; se agrego la funcion coprime que verifica si dos numeros son coprimos
;; perteneciente a el paquete LisPrime D. Radisavljević
(defun coprime (firstnum secondnum)
(SETQ  numcheck 0)
(if (eq (gcd firstnum secondnum) 1) (setQ numcheck 1) (setQ numcheck 0))
(if (eq numcheck 1) (return-from coprime t) (return-from coprime nil) ) )

