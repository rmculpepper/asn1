#lang racket/base
(require asn1)
(provide (all-defined-out))

;; Compare with Standards for Efficient Cryptography 1 (SEC 1):
;;   Elliptic Curve Cryptography (v2), Certicom Research
;; http://www.secg.org/download/aid-780/sec1-v2.pdf

;; ============================================================
;; First, some utilities

;; get-type : Key (listof (list Key Type)) -> Type
(define (get-type key typemap)
  (cond [(assoc key typemap)
         => (lambda (e)
              (unless (pair? (cdr e))
                (error 'get-type "key has no associated type\n  key: ~e" key))
              (cadr e))]
        [else (error 'get-type "key not found\n  key: ~e" key)]))

;; ============================================================
;; Organization:

;; The easy way:
;; - put OIDs and constant definitions first
;; - then types, using define-asn1-type (unless parameterized)
;; - then info object sets

;; ============================================================
;; All OIDs 

(define SEC1-v1-9
  (OID (iso 1) (identified-organization 3) (certicom 132) (module 1) (ver 2)))

(define ansi-X9-62 (OID (iso 1) (member-body 2) (us 840) 10045))
(define id-fieldType (build-OID ansi-X9-62 (fieldType 1)))
(define prime-field (build-OID id-fieldType 1))
(define characteristic-two-field (build-OID id-fieldType 2))

(define id-characteristic-two-basis
  (build-OID characteristic-two-field (basisType 3)))
(define gnBasis (build-OID id-characteristic-two-basis 1))
(define tpBasis (build-OID id-characteristic-two-basis 2))
(define ppBasis (build-OID id-characteristic-two-basis 3))

(define sha-1
  (OID (iso 1) (identified-organization 3) (oiw 14) (secsig 3) (algorithm 2) 2))
(define id-sha
  (OID (joint-iso-itu-t 2) (country 16) (us 840)
       (organization 1) (gov 101) (csor 3) (nistalgorithm 4) (hashalgs 2)))
(define id-sha224 (build-OID id-sha 4))
(define id-sha256 (build-OID id-sha 1))
(define id-sha384 (build-OID id-sha 2))
(define id-sha512 (build-OID id-sha 3))

(define id-publicKeyType (build-OID ansi-X9-62 (keyType 2)))
(define id-ecPublicKey (build-OID id-publicKeyType 1))
(define id-ecPublicKeyTypeRestricted
  (build-OID id-publicKeyType (restricted 2)))

(define secg-scheme
  (OID (iso 1) (identified-organization 3) (certicom 132) (schemes 1)))
(define id-ecPublicKeyTypeSupplemented
  (build-OID secg-scheme (supplementalPoints 0)))

(define id-ecSigType (build-OID ansi-X9-62 (signatures 4)))
(define ecdsa-with-SHA1 (build-OID id-ecSigType (sha1 1)))
(define ecdsa-with-Recommended (build-OID id-ecSigType (recommended 2)))
(define ecdsa-with-Specified (build-OID id-ecSigType (specified 3)))
(define ecdsa-with-Sha224 (build-OID id-ecSigType (specified 3) 1))
(define ecdsa-with-Sha256 (build-OID id-ecSigType (specified 3) 2))
(define ecdsa-with-Sha384 (build-OID id-ecSigType (specified 3) 3))
(define ecdsa-with-Sha512 (build-OID id-ecSigType (specified 3) 4))

(define x9-63-scheme
  (OID (iso 1) (member-body 2) (us 840) (ansi-x9-63 63) (schemes 0)))
(define dhSinglePass-stdDH-sha1kdf (build-OID x9-63-scheme 2))
(define dhSinglePass-cofactorDH-sha1kdf (build-OID x9-63-scheme 3))
(define mqvSinglePass-sha1kdf (build-OID x9-63-scheme 16))
(define mqvFull-sha1kdf (build-OID x9-63-scheme 17))
(define dhSinglePass-cofactorDH-recommendedKDF (build-OID secg-scheme 1))
(define dhSinglePass-cofactorDH-specifiedKDF (build-OID secg-scheme 2))
(define ecdh (build-OID secg-scheme 12))
(define dhSinglePass-stdDH-sha256kdf-scheme (build-OID secg-scheme 11 1))
(define dhSinglePass-stdDH-sha384kdf-scheme (build-OID secg-scheme 11 2))
(define dhSinglePass-stdDH-sha224kdf-scheme (build-OID secg-scheme 11 0))
(define dhSinglePass-stdDH-sha512kdf-scheme (build-OID secg-scheme 11 3))
(define dhSinglePass-cofactorDH-sha256kdf-scheme (build-OID secg-scheme 14 1))
(define dhSinglePass-cofactorDH-sha384kdf-scheme (build-OID secg-scheme 14 2))
(define dhSinglePass-cofactorDH-sha224kdf-scheme (build-OID secg-scheme 14 0))
(define dhSinglePass-cofactorDH-sha512kdf-scheme (build-OID secg-scheme 14 3))
(define mqvSinglePass-recommendedKDF (build-OID secg-scheme 3))
(define mqvSinglePass-specifiedKDF (build-OID secg-scheme 4))
(define mqvFull-recommendedKDF (build-OID secg-scheme 5))
(define mqvFull-specifiedKDF (build-OID secg-scheme 6))
(define ecmqv (build-OID secg-scheme 13))
(define mqvSinglePass-sha256kdf-scheme (build-OID secg-scheme 15 1))
(define mqvSinglePass-sha384kdf-scheme (build-OID secg-scheme 15 2))
(define mqvSinglePass-sha224kdf-scheme (build-OID secg-scheme 15 0))
(define mqvSinglePass-sha512kdf-scheme (build-OID secg-scheme 15 3))
(define mqvFull-sha256kdf-scheme (build-OID secg-scheme 16 1))
(define mqvFull-sha384kdf-scheme (build-OID secg-scheme 16 2))
(define mqvFull-sha224kdf-scheme (build-OID secg-scheme 16 0))
(define mqvFull-sha512kdf-scheme (build-OID secg-scheme 16 3))

(define x9-63-kdf (build-OID secg-scheme 17 0))
(define nist-concatenation-kdf (build-OID secg-scheme 17 1))
(define tls-kdf (build-OID secg-scheme 17 2))
(define ikev2-kdf (build-OID secg-scheme 17 3))

(define ecies-recommendedParameters (build-OID secg-scheme 7))
(define ecies-specifiedParameters (build-OID secg-scheme 8))

(define xor-in-ecies (build-OID secg-scheme 18))
(define tdes-cbc-in-ecies (build-OID secg-scheme 19))
(define aes128-cbc-in-ecies (build-OID secg-scheme 20 0))
(define aes192-cbc-in-ecies (build-OID secg-scheme 20 1))
(define aes256-cbc-in-ecies (build-OID secg-scheme 20 2))
(define aes128-ctr-in-ecies (build-OID secg-scheme 21 0))
(define aes192-ctr-in-ecies (build-OID secg-scheme 21 1))
(define aes256-ctr-in-ecies (build-OID secg-scheme 21 2))
(define hmac-full-ecies (build-OID secg-scheme 22))
(define hmac-half-ecies (build-OID secg-scheme 23))
(define cmac-aes128-ecies (build-OID secg-scheme 24 0))
(define cmac-aes192-ecies (build-OID secg-scheme 24 1))
(define cmac-aes256-ecies (build-OID secg-scheme 24 2))

(define ecwkt-recommendedParameters (build-OID secg-scheme 9))
(define ecwkt-specifiedParameters (build-OID secg-scheme 10))

(define aes128-key-wrap (build-OID secg-scheme 25 0))
(define aes192-key-wrap (build-OID secg-scheme 25 1))
(define aes256-key-wrap (build-OID secg-scheme 25 2))


;; ============================================================
;; Types

;; EXPLICIT TAGS

(define (FieldID typemap)
  (Sequence [fieldType OBJECT-IDENTIFIER]
            [parameters #:dependent (get-type fieldType typemap)]))

(define Prime-p INTEGER) ;; -- Field of size p.

(define Characteristic-two
  (Sequence [m INTEGER] ;; -- Field size 2m
            [basis OBJECT-IDENTIFIER]
            [parameters #:dependent (get-type basis BasisTypes)]))

(define Trinomial INTEGER)

(define Pentanomial
  (Sequence [k1 INTEGER]   ;; -- k1 > 0
            [k2 INTEGER]   ;; -- k2 > k1
            [k3 INTEGER])) ;; -- k3 > k2

(define FieldElement OCTET-STRING)

(define (ECDomainParameters IOSet) ;; IOSet : ECDOMAIN
  (Choice [specified SpecifiedECDomain]
          [named OBJECT-IDENTIFIER] ;; ECDOMAIN.&id({IOSet})
          [implicitCA NULL]))

(define-asn1-type SpecifiedECDomain
  (Sequence [version SpecifiedECDomainVersion]
            [fieldID (FieldID FieldTypes)]
            [curve Curve]
            [base ECPoint]
            [order INTEGER]
            [cofactor INTEGER #:optional]
            [hash HashAlgorithm #:optional]))

(define SpecifiedECDomainVersion INTEGER)
(define ecdpVer1 1)
(define ecdpVer2 2)
(define ecdpVer3 3)

(define Curve
  (Sequence [a FieldElement]
            [b FieldElement]
            ;; -- Shall be present if used in SpecifiedECDomain
            ;; -- with version equal to ecdpVer2 or ecdpVer3
            [seed BIT-STRING #:optional]))

(define ECPoint OCTET-STRING)

;; ----------------------------------------

(define-asn1-type SubjectPublicKeyInfo
  (Sequence [algorithm (AlgorithmIdentifier ECPKAlgorithms)]
            ;;   (WITH COMPONENTS {algorithm, parameters})
            [subjectPublicKey BIT-STRING]))

(define-asn1-type ECPrivateKey
  (Sequence [version INTEGER] ;; (ecPrivkeyVer1)
            [privateKey OCTET-STRING]
            [parameters #:explicit 0 (ECDomainParameters SECGCurveNames) #:optional]
            [publicKey  #:explicit 1 BIT-STRING #:optional]))

(define ecPrivkeyVer1 1)

(define (AlgorithmIdentifier typemap)
  (Sequence [algorithm OBJECT-IDENTIFIER]
            [parameters #:dependent (get-type algorithm typemap) #:optional]))

(define-asn1-type HashAlgorithm (AlgorithmIdentifier HashFunctions))

(define-asn1-type ECCAlgorithm (AlgorithmIdentifier ECCAlgorithmSet))
(define ECCAlgorithms (SequenceOf ECCAlgorithm))

(define-asn1-type ECPKRestrictions
  (Sequence [ecDomain (ECDomainParameters SECGCurveNames)]
            [eccAlgorithms ECCAlgorithms]))

(define NamedMultiples
  (Sequence [multiples OBJECT-IDENTIFIER]
            [points (SequenceOf ECPoint)]))

(define SpecifiedMultiples
  (SequenceOf
   (Sequence [multiple INTEGER]
             [point ECPoint])))

(define ECCSupplements
  (Choice [namedMultiples     #:explicit 0  NamedMultiples]
          [specifiedMultiples #:explicit 1  SpecifiedMultiples]))

(define-asn1-type ECPKSupplements
  (Sequence [ecDomain (ECDomainParameters SECGCurveNames)]
            [eccAlgorithms ECCAlgorithms]
            [eccSupplements ECCSupplements]))

;; ----

(define-asn1-type ECIESParameters
  (Sequence [kdf #:explicit 0 KeyDerivationFunction #:optional]
            [sym #:explicit 1 SymmetricEncryption #:optional]
            [mac #:explicit 2 MessageAuthenticationCode #:optional]))

(define-asn1-type KeyDerivationFunction (AlgorithmIdentifier KDFSet))
(define-asn1-type SymmetricEncryption (AlgorithmIdentifier SYMENCSet))
(define-asn1-type MessageAuthenticationCode (AlgorithmIdentifier MACSet))

(define-asn1-type ECWKTParameters
  (Sequence [kdf  #:explicit 0 KeyDerivationFunction #:optional]
            [wrap #:explicit 1 KeyWrapFunction #:optional]))

(define-asn1-type KeyWrapFunction (AlgorithmIdentifier KeyWrapSet))

(define-asn1-type ECDSA-Signature
  (Choice [two-ints-plus ECDSA-Sig-Value]
          [point-int #:explicit 0 ECDSA-Full-R]))

(define ECDSA-Sig-Value
  (Sequence [r INTEGER]
            [s INTEGER]
            [a INTEGER #:optional]
            [y (Choice [b BOOLEAN] [f FieldElement]) #:optional]))

(define ECDSA-Full-R
  (Sequence [r ECPoint]
            [s INTEGER]))

(define ECIES-Ciphertext-Value
  (Sequence [ephemeralPublicKey ECPoint]
            [symmetricCiphertext OCTET-STRING]
            [macTag OCTET-STRING]))

#|
(define ASN1SharedInfo
  (Sequence [keyInfo (AlgorithmIdentifier ???)]
            [entityUInfo  #:explicit 0 OCTET-STRING #:optional]
            [entityVInfo  #:explicit 1 OCTET-STRING #:optional]
            [suppPubInfo  #:explicit 2 OCTET-STRING #:optional]
            [suppPrivInfo #:explicit 3 OCTET-STRING #:optional]))

(define SEC1-PDU
  (Choice [privateKey #:explicit 0 ECPrivateKey]
          [spki       #:explicit 1 SubjectPublicKeyInfo]
          [ecdsa      #:explicit 2 ECDSA-Signature]
          [ecies      #:explicit 3 ECIES-Ciphertext-Value]
          [sharedinfo #:explicit 4 ASN1SharedInfo]))
|#


;; ============================================================
;; Info Object Sets

;; FIELD-ID ::= TYPE-IDENTIFIER
(define FieldTypes ;; FIELD-ID
  (list (list prime-field              Prime-p)
        (list characteristic-two-field Characteristic-two)))

;; CHARACTERISTIC-TWO ::= TYPE-IDENTIFIER
(define (BasisTypes) ;; CHARACTERISTIC-TWO
  (list (list gnBasis NULL)
        (list tpBasis Trinomial)
        (list ppBasis Pentanomial)))

;; ECDOMAIN ::= CLASS { &id OBJECT IDENTIFIER UNIQUE } WITH SYNTAX { ID &id }

(define SECGCurveNames
  (list))

(define HashFunctions ;; ALGORITHM
  (list (list sha-1     NULL)
        (list id-sha224 NULL)
        (list id-sha256 NULL)
        (list id-sha384 NULL)
        (list id-sha512 NULL)))

(define ecPublicKeyType ;; ALGORITHM
  (list (list id-ecPublicKey (ECDomainParameters SECGCurveNames))))

(define ecPublicKeyTypeRestricted ;; ALGORITHM
  (list (list id-ecPublicKeyTypeRestricted ECPKRestrictions)))

(define ecPublicKeyTypeSupplemented ;; ALGORITHM
  (list (list id-ecPublicKeyTypeSupplemented ECPKSupplements)))

(define ECPKAlgorithms ;; ALGORITHM
  (append ecPublicKeyType
          ecPublicKeyTypeRestricted
          ecPublicKeyTypeSupplemented
          (list (list ecdh  (ECDomainParameters SECGCurveNames))
                (list ecmqv (ECDomainParameters SECGCurveNames)))))

(define ECDSAAlgorithmSet ;; ALGORITHM
  (list (list ecdsa-with-SHA1        NULL)
        (list ecdsa-with-Recommended NULL)
        (list ecdsa-with-Specified   HashAlgorithm)
        (list ecdsa-with-Sha224      NULL)
        (list ecdsa-with-Sha256      NULL)
        (list ecdsa-with-Sha384      NULL)
        (list ecdsa-with-Sha512      NULL)))

(define ECDHAlgorithmSet ;; ALGORITHM
  (list (list dhSinglePass-stdDH-sha1kdf           NULL)
        (list dhSinglePass-cofactorDH-sha1kdf      NULL)
        (list dhSinglePass-cofactorDH-recommendedKDF)
        (list dhSinglePass-cofactorDH-specifiedKDF KeyDerivationFunction)
        (list ecdh)
        (list dhSinglePass-stdDH-sha256kdf-scheme)
        (list dhSinglePass-stdDH-sha384kdf-scheme)
        (list dhSinglePass-stdDH-sha224kdf-scheme)
        (list dhSinglePass-stdDH-sha512kdf-scheme)
        (list dhSinglePass-cofactorDH-sha256kdf-scheme)
        (list dhSinglePass-cofactorDH-sha384kdf-scheme)
        (list dhSinglePass-cofactorDH-sha224kdf-scheme)
        (list dhSinglePass-cofactorDH-sha512kdf-scheme)))

(define ECMQVAlgorithmSet ;; ALGORITHM
  (list (list mqvSinglePass-sha1kdf)
        (list mqvSinglePass-recommendedKDF)
        (list mqvSinglePass-specifiedKDF     KeyDerivationFunction)
        (list mqvFull-sha1kdf)
        (list mqvFull-recommendedKDF)
        (list mqvFull-specifiedKDF           KeyDerivationFunction)
        (list ecmqv)
        (list mqvSinglePass-sha256kdf-scheme)
        (list mqvSinglePass-sha384kdf-scheme)
        (list mqvSinglePass-sha224kdf-scheme)
        (list mqvSinglePass-sha512kdf-scheme)
        (list mqvFull-sha256kdf-scheme)
        (list mqvFull-sha384kdf-scheme)
        (list mqvFull-sha224kdf-scheme)
        (list mqvFull-sha512kdf-scheme)))

(define KDFSet ;; ALGORITHM
  (list (list x9-63-kdf              HashAlgorithm)
        (list nist-concatenation-kdf HashAlgorithm)
        (list tls-kdf                HashAlgorithm)
        (list ikev2-kdf              HashAlgorithm)))

(define ECIESAlgorithmSet ;; ALGORITHM
  (list (list ecies-recommendedParameters)
        (list ecies-specifiedParameters ECIESParameters)))

(define SYMENCSet ;; ALGORITHM
  (list (list xor-in-ecies)
        (list tdes-cbc-in-ecies)
        (list aes128-cbc-in-ecies)
        (list aes192-cbc-in-ecies)
        (list aes256-cbc-in-ecies)
        (list aes128-ctr-in-ecies)
        (list aes192-ctr-in-ecies)
        (list aes256-ctr-in-ecies)))

(define MACSet ;; ALGORITHM
  (list (list hmac-full-ecies HashAlgorithm)
        (list hmac-half-ecies HashAlgorithm)
        (list cmac-aes128-ecies)
        (list cmac-aes192-ecies)
        (list cmac-aes256-ecies)))

(define ECWKTAlgorithmSet ;; ALGORITHM
  (list (list ecwkt-recommendedParameters)
        (list ecwkt-specifiedParameters ECWKTParameters)))

(define ECCAlgorithmSet ;; ALGORITHM
  (append ECDSAAlgorithmSet
          ECDHAlgorithmSet
          ECMQVAlgorithmSet
          ECIESAlgorithmSet
          ECWKTAlgorithmSet))

(define KeyWrapSet ;; ALGORITHM
  (list (list aes128-key-wrap)
        (list aes192-key-wrap)
        (list aes256-key-wrap)))
