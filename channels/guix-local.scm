;; -*- mode: scheme -*-
(use-modules (guix ci)
             (guix channels))

(list

 (channel
  (name 'guix)
  (url (format #f "file://~a/guix" (getenv "PWD")))
  (branch "master")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))

 (channel
  (name 'rde)
  (url (format #f "file://~a/rde" (getenv "PWD")))
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))

 (channel
  (name 'nonguix)
  (url (format #f "file://~a/nonguix" (getenv "PWD")))
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

 ;; (channel
 ;;  (name 'guix-binary)
 ;;  (url "/home/samuel/git/sys/guix-binary")
 ;;  (introduction
 ;;   (make-channel-introduction
 ;;    "0c171cf43849f86516e1ee23aec34e586654a3a7"
 ;;    (openpgp-fingerprint
 ;;     "F695 F39E C625 E081 33B5  759F 0FC6 8703 75EF E2F5"))))
 )
