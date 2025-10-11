;;; -*- lexical-binding: t; -*-

;; (global-unset-key (kbd "C-z"))
(define-key key-translation-map (kbd "C-n") (kbd "C-x"))
(define-key key-translation-map (kbd "C-x") (kbd "C-n"))
(define-key key-translation-map (kbd "M-n") (kbd "M-x"))
(define-key key-translation-map (kbd "M-x") (kbd "M-n"))
(define-key key-translation-map (kbd "M-N") (kbd "M-X"))
(define-key key-translation-map (kbd "M-X") (kbd "M-N"))
