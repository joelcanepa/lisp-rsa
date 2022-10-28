# lisp-rsa

## Description
Common Lisp implementation of the asymmetric encryption algorithm RSA for a university project submitted to CoNaIISI 2022.

## Steps to create a digital signature of a file
### To digitally sign a file:
1. Generate a pair of RSA keys:
<pre><code>$ ./rsa 1024 </code></pre>

2. Calculate the hash of the desired file to sign:
Linux:
<pre><code>$ md5sum file </code></pre>
Windows PowerShell:
<pre><code>$ Get-FileHash file </code></pre>

3. Encrypt with the private key the hash (copy and paste the hash) and write it to file.sig:
<pre><code>$ ./rsa encrypt private.key hash >> file.sig</code></pre>

Send the file and it's signature.

### To check the integrity and authenticity of the file:

1. Decrypt the signature with the sender's public key (in signature paste the file.sig content):
<pre><code>$ ./rsa decrypt public.key signature</code></pre>

2. Compare the previous command output with the hash of the file you received:
<pre><code>$ md5sum file </code></pre>

If the calculated hash and the hash obtained from decrypting the signature with the sender's public key are equal the file has maintained integrity and is authentic.

## Running the executable binary

### Linux
Download the latest release and run `rsa`:

<pre><code>$ ./rsa (key length in bits) </code></pre>

Example:

<pre><code>$ ./rsa 1024 </code></pre>

### Windows
Download the latest release and run `rsa.exe` via PowerShell:

<pre><code>> .\rsa.exe (key length in bits) </code></pre>

Example:

<pre><code>> .\rsa.exe 1024 </code></pre>

Three files containing the key pair generated and the exponent will be created:
* public.key
  * public key exponent
  * modulus
* private.key
  * private key exponent
  * modulus

## Recommendation
**It is recommended you choose one of the following key lengths:**

* 512
* 1024
* 1536
* 2048
* 4096

These key lengths (except for 4096) are covered in NIST FIPS 186-5 and they have the necessary rounds of miller rabin test coded to guarantee a probability error of 2^-100.

## Extra options

Run the binary or script with arguments: "--show" to print all generated and calculated rsa parameters.

Linux example:
<pre><code>$ ./rsa 1024 --show</code></pre>

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

# Dependencies

The project includes some open source packages, with modifications to fit the project's needs, to deal wih modular arithmetic and the miller rabin test:

* [cl-rsm-mod](https://sources.debian.org/src/cl-rsm-mod/1.4/mod.lisp/)
* cl-primality
* [LisPrime - coprime.lisp](https://github.com/dradisavljevic/LisPrime/blob/master/coprime.lisp)
