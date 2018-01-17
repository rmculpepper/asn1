#lang racket/base
(require asn1)
(provide (all-defined-out))

;; Compare with PKCS #1 Appendix C: ASN.1 module
;; http://www.ietf.org/rfc/rfc3447.txt

;; ============================================================
;; First, some utilities

;; get-type : Key (listof (list Key Type)) -> Type
(define (get-type key typemap)
  (cond [(assoc key typemap)
         => cadr]
        [else (error 'get-type "no type for key: ~e" key)]))

;; ============================================================
;; Organization:

;; The hard way:
;; - locally reordered definitions
;; - used define-asn1-type in a few places

;; ============================================================

(define PKCS-1
  (OID (iso 1) (member-body 2) (us 840) (rsadsi 113549)
       (pkcs 1) (pkcs-1 1) (modules 0) (pkcs-1 1)))

(begin
  ;; Useful for later OIDs
  (define rsadsi
    (OID (iso 1) (member-body 2) (us 840) (rsadsi 113549))))

;; IMPORTS
;; id-sha256, id-sha384, id-sha512
;;     FROM NIST-SHA2 {
;;         joint-iso-itu-t(2) country(16) us(840) organization(1)
;;         gov(101) csor(3) nistalgorithm(4) modules(0) sha2(1)
;;     };

(begin
  ;; Some OID definitions from
  ;; - http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/documents/aes1.asn
  (define csor
    (OID (joint-iso-ccitt 2) (country 16) (us 840) (organization 1) (gov 101) (csor 3)))
  (define nistAlgorithms (build-OID csor (nistalgorithms 4)))
  ;; Some OID definitions from
  ;; - http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/algorithms.html
  (define hashAlgs (build-OID nistAlgorithms 2))
  (define id-sha256 (build-OID hashAlgs 1))
  (define id-sha384 (build-OID hashAlgs 2))
  (define id-sha512 (build-OID hashAlgs 3))
  (define id-sha224 (build-OID hashAlgs 4))
  (define id-sha512-224 (build-OID hashAlgs 5))
  (define id-sha512-256 (build-OID hashAlgs 6)))

;; ============================
;;   Basic object identifiers
;; ============================

;; The DER encoding of this in hexadecimal is:
;; (0x)06 08
;;        2A 86 48 86 F7 0D 01 01
;;
(define pkcs-1 (build-OID rsadsi (pkcs 1) 1))

;; When rsaEncryption is used in an AlgorithmIdentifier the
;; parameters MUST be present and MUST be NULL.
(define rsaEncryption (build-OID pkcs-1 1))

;; When id-RSAES-OAEP is used in an AlgorithmIdentifier the
;; parameters MUST be present and MUST be RSAES-OAEP-params.
(define id-RSAES-OAEP (build-OID pkcs-1 7))

;; When id-pSpecified is used in an AlgorithmIdentifier the
;; parameters MUST be an OCTET STRING.
(define id-pSpecified (build-OID pkcs-1 9))

;; When id-RSASSA-PSS is used in an AlgorithmIdentifier the
;; parameters MUST be present and MUST be RSASSA-PSS-params.
(define id-RSASSA-PSS (build-OID pkcs-1 10))

;; When the following OIDs are used in an AlgorithmIdentifier the
;; parameters MUST be present and MUST be NULL.
(define md2WithRSAEncryption (build-OID pkcs-1 2))
(define md5WithRSAEncryption (build-OID pkcs-1 4))
(define sha1WithRSAEncryption (build-OID pkcs-1 5))
(define sha256WithRSAEncryption (build-OID pkcs-1 11))
(define sha384WithRSAEncryption (build-OID pkcs-1 12))
(define sha512WithRSAEncryption (build-OID pkcs-1 13))

;; This OID really belongs in a module with the secsig OIDs.
(define id-sha1
  (OID (iso 1) (identified-organization 3) (oiw 14) (secsig 3) (algorithms 2) 26))

;; OIDs for MD2 and MD5, allowed only in EMSA-PKCS1-v1_5.

(define id-md2
  (build-OID rsadsi (digestAlgorithm 2) 2))

(define id-md5
  (build-OID rsadsi (digestAlgorithm 2) 5))

;; When id-mgf1 is used in an AlgorithmIdentifier the parameters MUST
;; be present and MUST be a HashAlgorithm, for example sha1.
(define id-mgf1 (build-OID pkcs-1 8))

;; ================
;;   Useful types
;; ================

;; ALGORITHM-IDENTIFIER ::= CLASS {
;;     &id    OBJECT IDENTIFIER  UNIQUE,
;;     &Type  OPTIONAL
;; }
;;     WITH SYNTAX { OID &id [PARAMETERS &Type] }

;; -- Note: the parameter InfoObjectSet in the following definitions
;; -- allows a distinct information object set to be specified for sets
;; -- of algorithms such as:
;; -- DigestAlgorithms    ALGORITHM-IDENTIFIER ::= {
;; --     { OID id-md2  PARAMETERS NULL }|
;; --     { OID id-md5  PARAMETERS NULL }|
;; --     { OID id-sha1 PARAMETERS NULL }
;; -- }

;; AlgorithmIdentifier { ALGORITHM-IDENTIFIER:InfoObjectSet } ::=
;; SEQUENCE {
;;     algorithm  ALGORITHM-IDENTIFIER.&id({InfoObjectSet}),
;;     parameters
;;         ALGORITHM-IDENTIFIER.&Type({InfoObjectSet}{@.algorithm})
;;             OPTIONAL
;; }

(define (AlgorithmIdentifier typemap)
  (SEQUENCE [algorithm OBJECT-IDENTIFIER]
            [parameters #:dependent (get-type algorithm typemap) #:optional]))

;; ==============
;;   Algorithms
;; ==============

;; Allowed EME-OAEP and EMSA-PSS digest algorithms.
(define OAEP-PSSDigestAlgorithms
  (list (list id-sha1   NULL)
        (list id-sha256 NULL)
        (list id-sha384 NULL)
        (list id-sha512 NULL)))

;; Allowed EMSA-PKCS1-v1_5 digest algorithms.
(define PKCS1-v1-5DigestAlgorithms
  (list (list id-md2    NULL)
        (list id-md5    NULL)
        (list id-sha1   NULL)
        (list id-sha256 NULL)
        (list id-sha384 NULL)
        (list id-sha512 NULL)))

;; When id-md2 and id-md5 are used in an AlgorithmIdentifier the
;; parameters MUST be present and MUST be NULL.

;; When id-sha1, id-sha256, id-sha384 and id-sha512 are used in an
;; AlgorithmIdentifier the parameters (which are optional) SHOULD
;; be omitted. However, an implementation MUST also accept
;; AlgorithmIdentifier values where the parameters are NULL.

(define HashAlgorithm (AlgorithmIdentifier OAEP-PSSDigestAlgorithms))

(define SHA1Parameters NULL)

(define sha1 (hasheq 'algorithm id-sha1 'parameters #f))
;; parameters included for compat. with existing impls.

;; Allowed mask generation function algorithms.
;; If the identifier is id-mgf1, the parameters are a HashAlgorithm.
(define PKCS1MGFAlgorithms
  (list (list id-mgf1 HashAlgorithm)))

(define MaskGenAlgorithm (AlgorithmIdentifier PKCS1MGFAlgorithms))

;; Default AlgorithmIdentifier for id-RSAES-OAEP.maskGenAlgorithm and
;; id-RSASSA-PSS.maskGenAlgorithm.
(define mgf1SHA1 ;;  MaskGenAlgorithm
  (hasheq 'algorithm id-mgf1 'parameters sha1))

(define EncodingParameters OCTET-STRING) ;; SIZE(0..MAX)

;; Allowed algorithms for pSourceAlgorithm.
(define PKCS1PSourceAlgorithms
  (list (list id-pSpecified EncodingParameters)))

;; This identifier means that the label L is an empty string, so the
;; digest of the empty string appears in the RSA block before
;; masking.
(define PSourceAlgorithm (AlgorithmIdentifier PKCS1PSourceAlgorithms))
(define emptyString #"") ;; EncodingParameters
(define pSpecifiedEmpty ;; PSourceAlgorithm
  (hasheq 'algorithm id-pSpecified 'parameters #""))

;; Type identifier definitions for the PKCS #1 OIDs.
(define PKCS1Algorithms
  (list* (list rsaEncryption              NULL)
         (list md2WithRSAEncryption       NULL)
         (list md5WithRSAEncryption       NULL)
         (list sha1WithRSAEncryption      NULL)
         (list sha256WithRSAEncryption    NULL)
         (list sha384WithRSAEncryption    NULL)
         (list sha512WithRSAEncryption    NULL)
         (list id-RSAES-OAEP              (DELAY RSAES-OAEP-params))
         (list id-RSASSA-PSS              (DELAY RSASSA-PSS-params))
         PKCS1PSourceAlgorithms))

;; ===================
;;   Main structures
;; ===================

(define-asn1-type RSAPublicKey
  (SEQUENCE [modulus           INTEGER]    ;; n
            [publicExponent    INTEGER]))  ;; e

;; Representation of RSA private key with information for the CRT
;; algorithm.
(define-asn1-type RSAPrivateKey
  (SEQUENCE [version           Version]
            [modulus           INTEGER] ;; n
            [publicExponent    INTEGER] ;; e
            [privateExponent   INTEGER] ;; d
            [prime1            INTEGER] ;; p
            [prime2            INTEGER] ;; q
            [exponent1         INTEGER] ;; d mod (p-1)
            [exponent2         INTEGER] ;; d mod (q-1)
            [coefficient       INTEGER] ;; (inverse of q) mod p
            [otherPrimeInfos   OtherPrimeInfos #:optional]))

(define Version INTEGER)
;; two-prime(0), multi(1); version must be multi if otherPrimeInfos present

(define-asn1-type OtherPrimeInfos
  (SEQUENCE-OF OtherPrimeInfo)) ;; SIZE(1..MAX)

(define OtherPrimeInfo
  (SEQUENCE [prime             INTEGER] ;; ri
            [exponent          INTEGER] ;; di
            [coefficient       INTEGER])) ;; ti

;; AlgorithmIdentifier.parameters for id-RSAES-OAEP.
;; Note that the tags in this Sequence are explicit.
(define RSAES-OAEP-params
  (SEQUENCE [hashAlgorithm    #:explicit 0 HashAlgorithm    #:default sha1]
            [maskGenAlgorithm #:explicit 1 MaskGenAlgorithm #:default mgf1SHA1]
            [pSourceAlgorithm #:explicit 2 PSourceAlgorithm #:default pSpecifiedEmpty]))

;; -- Identifier for default RSAES-OAEP algorithm identifier.
;; -- The DER Encoding of this is in hexadecimal:
;; -- (0x)30 0D
;; --        06 09
;; --           2A 86 48 86 F7 0D 01 01 07
;; --        30 00
;; -- Notice that the DER encoding of default values is "empty".

(define rSAES-OAEP-Default-Identifier  ;; RSAES-AlgorithmIdentifier
  (hasheq 'algorithm id-RSAES-OAEP
          'parameters (hasheq 'hashAlgorithm sha1
                              'maskGenAlgorithm mgf1SHA1
                              'pSourceAlgorithm pSpecifiedEmpty)))

(define RSAES-AlgorithmIdentifier (AlgorithmIdentifier PKCS1Algorithms))

;; AlgorithmIdentifier.parameters for id-RSASSA-PSS.
;; Note that the tags in this Sequence are explicit.
(define-asn1-type RSASSA-PSS-params
  (SEQUENCE [hashAlgorithm    #:explicit 0  HashAlgorithm    #:default sha1]
            [maskGenAlgorithm #:explicit 1  MaskGenAlgorithm #:default mgf1SHA1]
            [saltLength       #:explicit 2  INTEGER          #:default 20]
            [trailerField     #:explicit 3  TrailerField     #:default trailerFieldBC]))

(define TrailerField INTEGER)
(define trailerFieldBC 1)

;; -- Identifier for default RSASSA-PSS algorithm identifier
;; -- The DER Encoding of this is in hexadecimal:
;; -- (0x)30 0D
;; --        06 09
;; --           2A 86 48 86 F7 0D 01 01 0A
;; --        30 00
;; -- Notice that the DER encoding of default values is "empty".

(define rSASSA-PSS-Default-Identifier  ;; RSASSA-AlgorithmIdentifier
  (hasheq 'algorithm id-RSASSA-PSS
          'parameters (hasheq 'hashAlgorithm sha1
                              'maskGenAlgorithm mgf1SHA1
                              'saltLength 20
                              'trailerField trailerFieldBC)))

(define RSASSA-AlgorithmIdentifier (AlgorithmIdentifier PKCS1Algorithms))

;; Syntax for the EMSA-PKCS1-v1_5 hash identifier.
(define-asn1-type DigestInfo
  (SEQUENCE [digestAlgorithm DigestAlgorithm]
            [digest OCTET-STRING]))

(define DigestAlgorithm (AlgorithmIdentifier PKCS1-v1-5DigestAlgorithms))
