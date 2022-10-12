(in-package :cl)

(defpackage rsm.mod
  (:use :cl)
  (:export :inverse)
  (:documentation "Este paquete contiene funciones para resolver aritmetica modular.
Resumen de Export:
inverse: Encuentra, si existe, el inverso de Z mod n."))

(defpackage :cl-primality
  (:use :cl)
  (:export :primep :miller-rabin :expt-mod :coprime)
  (:documentation  "Este paquete contiene funciones para evaluar si un numero es primo
o no, utilizando el test de primalidad probabilistico de Miller-Rabin. Adem�s, tiene una funcion
para resolver exponenciacion modular.
Resumen de Export:
primep: evalua mediante el test de Miller-Rabin si un numero es primo o no.
expt-mod: eleva la base b a la potencia e, modulo md utilizando la exponenciacion modular"))

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
  (:documentation "Este paquete contiene una  implementacion del algoritmo RSA.
Resumen de Export:
gennerar-claves: Genera un nuevo par de claves RSA con la cantidad de bits indicadas por parametro. Por defecto, 
si no se corrió al menos una vez, el criptosistema se encuentra inicializado en nil
print-rsa: Imprime por pantalla todos los valores del criptosistema RSA.
p:, q: Son el par de numeros primos que se generaron al azar y que fueron utilizados para generar
las claves RSA. Se deben mantener en secreto.
n: p*q. Forma parte de la clave pubica y la clave privada.
phi-n: Resultado de la funcion de carmichael.
e: Clave publica
d: Clave privada"))