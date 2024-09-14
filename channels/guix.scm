;; -*- mode: scheme -*-
(use-modules (guix ci)
             (guix channels))

(list
 %default-guix-channel

 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))

 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

 ;;; for hyprland
 ;; (channel
 ;; (name 'rosenthal)
 ;; (url "https://codeberg.org/hako/rosenthal.git")
 ;; (branch "trunk")
 ;; (introduction
 ;;  (make-channel-introduction
 ;;   "7677db76330121a901604dfbad19077893865f35"
 ;;   (openpgp-fingerprint
 ;;    "13E7 6CD6 E649 C28C 3385  4DF5 5E5A A665 6149 17F7"))))

 ;;; build of /gnu/store/59vmvlzgmksqryyvr45kp43v3wmz36qd-guix-package-cache.drv failed
 ;; (channel
 ;;  (name 'guix-binary)
 ;;  (url "https://git.sr.ht/~declantsien/guix-channel")
 ;;  (introduction
 ;;   (make-channel-introduction
 ;;    "0c171cf43849f86516e1ee23aec34e586654a3a7"
 ;;    (openpgp-fingerprint
 ;;     "F695 F39E C625 E081 33B5  759F 0FC6 8703 75EF E2F5"))))
 )
