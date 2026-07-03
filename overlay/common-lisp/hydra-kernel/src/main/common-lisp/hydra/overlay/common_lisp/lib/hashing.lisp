(in-package :cl-user)

;; Common Lisp implementations of hydra.lib.hashing primitives (#524).
;;
;; SHA-256 over raw bytes (binary). These pair with hydra.lib.files.readFile, which is
;; byte-oriented, to hash file contents. Pure and total.
;;
;; Runtime representations (Common Lisp target):
;;   binary/bytes  : a vector of (unsigned-byte 8). Inputs are coerced, so any byte sequence works.
;;
;; Implementation: a self-contained, portable SHA-256 (FIPS 180-4). The CL host runs under SBCL,
;; which has no built-in SHA-256 and no Ironclad dependency wired into the runtime, so we hand-roll
;; the algorithm here (mirroring the portable-fallback precedent in text.lisp). No external deps.

;; ---- Helpers (not primitives) ----

(defconstant +hydra-sha256-mask32+ #xFFFFFFFF)

(defparameter +hydra-sha256-k+
  (make-array 64 :element-type '(unsigned-byte 32)
    :initial-contents
    '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
      #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
      #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
      #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
      #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
      #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
      #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
      #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))

(declaim (inline hydra-sha256-rotr hydra-sha256-add))

(defun hydra-sha256-rotr (x n)
  "Rotate the 32-bit word X right by N bits."
  (logand (logior (ash x (- n)) (ash x (- 32 n))) +hydra-sha256-mask32+))

(defun hydra-sha256-add (&rest xs)
  "Sum 32-bit words modulo 2^32."
  (logand (apply #'+ xs) +hydra-sha256-mask32+))

(defun hydra-sha256-digest (data)
  "Compute the raw 32-byte SHA-256 digest of a byte sequence, returned as a vector of
   (unsigned-byte 8)."
  (let* ((msg (coerce data 'list))
         (mlen (length msg))
         (bitlen (* mlen 8))
         ;; Padding: append 0x80, then zeros, then the 64-bit big-endian bit length.
         (padded (append msg (list #x80)))
         (k (mod (- 56 (mod (1+ mlen) 64)) 64)))
    (setf padded (append padded (make-list k :initial-element 0)))
    (dotimes (i 8)
      (setf padded (append padded (list (logand (ash bitlen (* -8 (- 7 i))) #xFF)))))
    (let ((bytes (coerce padded '(vector (unsigned-byte 8))))
          (h (make-array 8 :element-type '(unsigned-byte 32)
               :initial-contents '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                                    #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
          (w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0)))
      (loop for base from 0 below (length bytes) by 64 do
        ;; Prepare the message schedule.
        (dotimes (tt 16)
          (let ((j (+ base (* tt 4))))
            (setf (aref w tt)
                  (logior (ash (aref bytes j) 24)
                          (ash (aref bytes (+ j 1)) 16)
                          (ash (aref bytes (+ j 2)) 8)
                          (aref bytes (+ j 3))))))
        (loop for tt from 16 below 64 do
          (let ((s0 (logxor (hydra-sha256-rotr (aref w (- tt 15)) 7)
                            (hydra-sha256-rotr (aref w (- tt 15)) 18)
                            (ash (aref w (- tt 15)) -3)))
                (s1 (logxor (hydra-sha256-rotr (aref w (- tt 2)) 17)
                            (hydra-sha256-rotr (aref w (- tt 2)) 19)
                            (ash (aref w (- tt 2)) -10))))
            (setf (aref w tt)
                  (hydra-sha256-add (aref w (- tt 16)) s0 (aref w (- tt 7)) s1))))
        ;; Compression.
        (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
              (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
          (dotimes (tt 64)
            (let* ((big-s1 (logxor (hydra-sha256-rotr e 6)
                                   (hydra-sha256-rotr e 11)
                                   (hydra-sha256-rotr e 25)))
                   (ch (logxor (logand e f) (logand (logxor e +hydra-sha256-mask32+) g)))
                   (temp1 (hydra-sha256-add hh big-s1 ch (aref +hydra-sha256-k+ tt) (aref w tt)))
                   (big-s0 (logxor (hydra-sha256-rotr a 2)
                                   (hydra-sha256-rotr a 13)
                                   (hydra-sha256-rotr a 22)))
                   (maj (logxor (logand a b) (logand a c) (logand b c)))
                   (temp2 (hydra-sha256-add big-s0 maj)))
              (setf hh g g f f e e (hydra-sha256-add d temp1)
                    d c c b b a a (hydra-sha256-add temp1 temp2))))
          (setf (aref h 0) (hydra-sha256-add (aref h 0) a)
                (aref h 1) (hydra-sha256-add (aref h 1) b)
                (aref h 2) (hydra-sha256-add (aref h 2) c)
                (aref h 3) (hydra-sha256-add (aref h 3) d)
                (aref h 4) (hydra-sha256-add (aref h 4) e)
                (aref h 5) (hydra-sha256-add (aref h 5) f)
                (aref h 6) (hydra-sha256-add (aref h 6) g)
                (aref h 7) (hydra-sha256-add (aref h 7) hh))))
      ;; Serialize the eight 32-bit words as 32 big-endian bytes.
      (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
        (dotimes (i 8)
          (dotimes (b 4)
            (setf (aref out (+ (* i 4) b))
                  (logand (ash (aref h i) (* -8 (- 3 b))) #xFF))))
        out))))

;; ---- Primitives ----

;; sha256 :: binary -> binary
(defvar hydra_overlay_common_lisp_lib_hashing_sha256
  (lambda (data)
    (hydra-sha256-digest data)))

;; sha256Hex :: binary -> string
(defvar hydra_overlay_common_lisp_lib_hashing_sha256_hex
  (lambda (data)
    (let ((digest (hydra-sha256-digest data)))
      (string-downcase
        (with-output-to-string (s)
          (loop for byte across digest do (format s "~2,'0X" byte)))))))
