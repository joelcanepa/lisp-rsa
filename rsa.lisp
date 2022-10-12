(in-package :rsa)

;; definicion e inicializacion de parametros rsa
(setq p nil)

(setq q nil)

(setq n nil)

(setq phi-n nil)

(setq e nil)

(setq d nil)

;; Defino un estado inicial o semilla para la generaci�n pseudoaleatoria de numeros
(setq random-state (make-random-state t))

;; Genera un numero pseudoaleatorio de n bits (entre cero y 2^n-bits)
(defun random-n-bits (bits)
  (random (expt 2 bits) random-state))

;; devuelve un numero primo.
(defun generar-primo (bits)
  ; numero de iteraciones
  (setq k 0)
  ;; con una probabilidad de error
  ;; de 2^-100 segun NIST FIPS 186-5
  (cond
     ((eq bits 512)  (setq k 7))
     ((eq bits 1024) (setq k 4))
     ((eq bits 1536) (setq k 3))
     ((eq bits 2048) (setq k 2))
  )
  ;; se itera hasta encontrar un numero
  ;; que pase el test de M-R
  (loop
     (setq numero (random-n-bits bits))
  (when (eq (miller-rabin  numero k) numero) (return numero))))

;; calcula el valor de n
(defun calc-n (p q)
  (* p q))

;; calcula el valor de phi-n, usando la funcion de carmichael
;; asumiendo que p y q son primos, entonces la funcion se puede calcular como
;; el minimo comun multiplo entre p-1 y q-1
(defun fun-carmichael (p q)
  (lcm (- p 1) (- q 1)))

;; calcula el valor de e (clave publica)
;; se busca un numero al azar e tal que  1<e<phi-n, y e, phi-n son coprimos
(defun calc-e (phi-n)
  (loop
     (setq e (+ (random (- phi-n 1) random-state) 1))
  (when (eq (coprime phi-n e) t) (return e))))

;; calcula el valor de d (clave privada)
;; halla el inverso d de e = 1 mod phi-n
(defun calc-d (phi-n e)
  (inverse e phi-n))

;; Calcula todos los valores de rsa
(defun generar-claves (bits)
  (format t "Generando claves...~%")
  (setq p (generar-primo bits))
  (setq q (generar-primo bits))
  (setq n (calc-n p q))
  (setq phi-n (fun-carmichael p q))
  (setq e (calc-e phi-n))
  (setq d (calc-d phi-n e))
  (format t "Generacion de claves completa.~%"))

;; funcion de cifrado rsa
(defun encrypt (m e n)
  (expt-mod m e n))

;; funcion de descifrado rsa
(defun decrypt (c d n)
  (expt-mod c d n))

;; muestra por pantalla todos los valores de rsa
(defun print-rsa ()
  (format t "p: ~A~%" p)
  (format t "q: ~A~%" q)
  (format t "n: ~A~%" n)
  (format t "phi-n: ~A~%" phi-n)
  (format t "e: ~A~%" e)
  (format t "d: ~A~%" d))