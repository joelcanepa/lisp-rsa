# cl-rsa

## Description
Common Lisp implementation of the asymmetric encryption algorithm RSA.

This was my final project for the subject "Programming Paradigms IV" in University. This work along with an article ("Programación Funcional, RSA y Firmas Digitales con Lisp") were accepted and published in Argentina's National Congress of Computer Engineering / Information Systems (CoNaIISI) 2022.

## Running the executable binary

Download the latest release and run `rsa`:

<pre><code>$ ./rsa (key length in bits) </code></pre>

Example:

<pre><code>$ ./rsa 1024 </code></pre>

## Recommendation
**It is recommended you choose one of the following key lengths:**

* 512
* 1024
* 1536
* 2048

These key lengths are covered in NIST FIPS 186-5 and they have explicitly coded the necessary rounds of miller rabin test to guarantee a probability error of 2^-100.

## Running the script from source
### You will need:
* [Roswell](https://roswell.github.io/Home.html): Common Lisp environment setup Utility - with [SBCL](https://www.sbcl.org/)

### To run the script:

Clone the repository:

<pre><code>$ git clone https://github.com/joelcanepa/lisp-rsa.git</code></pre>

Run `rsa.ros`:

<pre><code>$ ./rsa.ros (key length in bits) </code></pre>

Example:

<pre><code>$ ./rsa.ros 1024 </code></pre>

# Dependecies

The project includes some open source packages, with modifications to fit the project's needs, to deal wih modular arithmetic and the miller rabin test:

* [cl-rsm-mod](https://sources.debian.org/src/cl-rsm-mod/1.4/mod.lisp/)
* cl-primality
* [LisPrime - coprime.lisp](https://github.com/dradisavljevic/LisPrime/blob/master/coprime.lisp)