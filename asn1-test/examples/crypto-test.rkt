;; Copyright 2017-2018 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/file asn1 "crypto.rkt")
(provide (all-defined-out))

;; Generate RSA keypair
;;   openssl genrsa -out privkey.pem 1024
;;   openssl rsa -in privkey.pem -outform DER -out privkey.der
;;   openssl rsa -in privkey.pem -outform DER -pubout -out pubkey.der

(define rsa-pub (file->bytes "crypto-data/rsa-pubkey.der"))
(define rsa-priv (file->bytes "crypto-data/rsa-privkey.der"))

(bytes->asn1/DER SubjectPublicKeyInfo rsa-pub)
(bytes->asn1/DER RSAPrivateKey rsa-priv)

;; Generate DSA parameters
;;   openssl dsaparam -outform DER -out dsa-param.der 512

(bytes->asn1/DER Dss-Parms (file->bytes "crypto-data/dsa-param.der"))

;; Generate DSA keypair
;;   openssl dsaparam -inform DER -in dsa-param.der -outform PEM -out dsa-param.pem
;;   openssl gendsa -out dsa-privkey.pem dsa-param.pem 
;;   openssl dsa -inform PEM -in dsa-privkey.pem -outform DER -out dsa-privkey.der
;;   openssl dsa -inform PEM -in dsa-privkey.pem -outform DER -pubout -out dsa-pubkey.der

(bytes->asn1/DER DSAPrivateKey (file->bytes "crypto-data/dsa-privkey.der"))
(bytes->asn1/DER SubjectPublicKeyInfo (file->bytes "crypto-data/dsa-pubkey.der"))
