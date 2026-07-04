(define-library (hydra overlay scheme lib hashing)
  (import (scheme base)
          (srfi 151))         ;; Bitwise operations (chibi-compatible)
  (export hydra_lib_hashing_sha256
          hydra_lib_hashing_sha256_hex)
  (begin

    ;; Scheme (R7RS) implementations of hydra.lib.hashing primitives (#524).
    ;;
    ;; SHA-256 over raw bytes (binary). These pair with hydra.lib.files.readFile, which is
    ;; byte-oriented, to hash file contents. Pure and total.
    ;;
    ;; Runtime representations (Scheme target):
    ;;   binary/bytes  : a Scheme vector of byte ints (e.g. #(65 66)), matching the
    ;;                   hydra.lib.literals binary representation. The helpers below convert to/from
    ;;                   a bytevector (and accept a list or bytevector defensively).
    ;;
    ;; Implementation: a self-contained, portable SHA-256 (FIPS 180-4) built on SRFI 151 bitwise ops
    ;; (as used by literals.scm). R7RS has no built-in hashing, so the algorithm is hand-rolled here.

    (define mask32 #xFFFFFFFF)

    ;; Convert a Hydra binary value (vector of byte ints; also tolerates a list or bytevector) to a
    ;; bytevector.
    (define (binary->bytevector b)
      (cond
        ((bytevector? b) b)
        ((vector? b)
         (let ((bv (make-bytevector (vector-length b))))
           (do ((i 0 (+ i 1)))
               ((= i (vector-length b)) bv)
             (bytevector-u8-set! bv i (vector-ref b i)))))
        ((list? b)
         (let ((bv (make-bytevector (length b))))
           (let loop ((i 0) (xs b))
             (if (null? xs)
                 bv
                 (begin (bytevector-u8-set! bv i (car xs))
                        (loop (+ i 1) (cdr xs)))))))
        (else (error "binary->bytevector: unsupported binary representation" b))))

    ;; Convert a bytevector to the Hydra binary representation (a vector of byte ints).
    (define (bytevector->binary bv)
      (let ((v (make-vector (bytevector-length bv))))
        (do ((i 0 (+ i 1)))
            ((= i (bytevector-length bv)) v)
          (vector-set! v i (bytevector-u8-ref bv i)))))

    (define (add32 . xs)
      (bitwise-and (apply + xs) mask32))

    ;; Rotate the 32-bit word x right by n bits.
    (define (rotr x n)
      (bitwise-and
        (bitwise-ior (arithmetic-shift x (- n)) (arithmetic-shift x (- 32 n)))
        mask32))

    (define (shr x n) (arithmetic-shift x (- n)))

    (define k
      #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
        #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
        #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
        #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
        #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
        #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
        #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
        #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

    ;; Compute the raw 32-byte SHA-256 digest of a byte sequence, returned as a bytevector.
    (define (sha256-digest data)
      (let* ((msg (binary->bytevector data))
             (mlen (bytevector-length msg))
             (bitlen (* mlen 8))
             ;; Padded length: message + 1 (0x80) + zeros to 56 mod 64 + 8 (length).
             (padlen (let ((r (modulo (+ mlen 1) 64)))
                       (+ mlen 1 (modulo (- 56 r) 64) 8)))
             (buf (make-bytevector padlen 0)))
        (bytevector-copy! buf 0 msg 0 mlen)
        (bytevector-u8-set! buf mlen #x80)
        ;; Append the 64-bit big-endian bit length in the final 8 bytes.
        (do ((i 0 (+ i 1)))
            ((= i 8))
          (bytevector-u8-set! buf (+ (- padlen 8) i)
            (bitwise-and (arithmetic-shift bitlen (* -8 (- 7 i))) #xFF)))
        (let ((h (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                         #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
              (w (make-vector 64 0)))
          (do ((base 0 (+ base 64)))
              ((>= base padlen))
            ;; Message schedule.
            (do ((tt 0 (+ tt 1)))
                ((= tt 16))
              (let ((j (+ base (* tt 4))))
                (vector-set! w tt
                  (bitwise-ior
                    (arithmetic-shift (bytevector-u8-ref buf j) 24)
                    (arithmetic-shift (bytevector-u8-ref buf (+ j 1)) 16)
                    (arithmetic-shift (bytevector-u8-ref buf (+ j 2)) 8)
                    (bytevector-u8-ref buf (+ j 3))))))
            (do ((tt 16 (+ tt 1)))
                ((= tt 64))
              (let ((s0 (bitwise-xor (rotr (vector-ref w (- tt 15)) 7)
                                     (rotr (vector-ref w (- tt 15)) 18)
                                     (shr (vector-ref w (- tt 15)) 3)))
                    (s1 (bitwise-xor (rotr (vector-ref w (- tt 2)) 17)
                                     (rotr (vector-ref w (- tt 2)) 19)
                                     (shr (vector-ref w (- tt 2)) 10))))
                (vector-set! w tt
                  (add32 (vector-ref w (- tt 16)) s0 (vector-ref w (- tt 7)) s1))))
            ;; Compression.
            (let ((a (vector-ref h 0)) (b (vector-ref h 1)) (c (vector-ref h 2)) (d (vector-ref h 3))
                  (e (vector-ref h 4)) (f (vector-ref h 5)) (g (vector-ref h 6)) (hh (vector-ref h 7)))
              (do ((tt 0 (+ tt 1)))
                  ((= tt 64))
                (let* ((big-s1 (bitwise-xor (rotr e 6) (rotr e 11) (rotr e 25)))
                       (ch (bitwise-xor (bitwise-and e f) (bitwise-and (bitwise-xor e mask32) g)))
                       (temp1 (add32 hh big-s1 ch (vector-ref k tt) (vector-ref w tt)))
                       (big-s0 (bitwise-xor (rotr a 2) (rotr a 13) (rotr a 22)))
                       (maj (bitwise-xor (bitwise-and a b) (bitwise-and a c) (bitwise-and b c)))
                       (temp2 (add32 big-s0 maj)))
                  (set! hh g) (set! g f) (set! f e) (set! e (add32 d temp1))
                  (set! d c) (set! c b) (set! b a) (set! a (add32 temp1 temp2))))
              (vector-set! h 0 (add32 (vector-ref h 0) a))
              (vector-set! h 1 (add32 (vector-ref h 1) b))
              (vector-set! h 2 (add32 (vector-ref h 2) c))
              (vector-set! h 3 (add32 (vector-ref h 3) d))
              (vector-set! h 4 (add32 (vector-ref h 4) e))
              (vector-set! h 5 (add32 (vector-ref h 5) f))
              (vector-set! h 6 (add32 (vector-ref h 6) g))
              (vector-set! h 7 (add32 (vector-ref h 7) hh))))
          ;; Serialize the eight 32-bit words as 32 big-endian bytes.
          (let ((out (make-bytevector 32 0)))
            (do ((i 0 (+ i 1)))
                ((= i 8))
              (do ((b 0 (+ b 1)))
                  ((= b 4))
                (bytevector-u8-set! out (+ (* i 4) b)
                  (bitwise-and (arithmetic-shift (vector-ref h i) (* -8 (- 3 b))) #xFF))))
            out))))

    (define hex-chars "0123456789abcdef")

    ;; sha256 :: binary -> binary
    (define hydra_lib_hashing_sha256
      (lambda (data)
        (bytevector->binary (sha256-digest data))))

    ;; sha256Hex :: binary -> string
    (define hydra_lib_hashing_sha256_hex
      (lambda (data)
        (let* ((digest (sha256-digest data))
               (out (make-string (* 2 (bytevector-length digest)))))
          (do ((i 0 (+ i 1)))
              ((= i (bytevector-length digest)) out)
            (let ((byte (bytevector-u8-ref digest i)))
              (string-set! out (* i 2) (string-ref hex-chars (arithmetic-shift byte -4)))
              (string-set! out (+ (* i 2) 1) (string-ref hex-chars (bitwise-and byte #xF))))))))))
