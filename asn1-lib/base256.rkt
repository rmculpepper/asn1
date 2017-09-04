;; Copyright 2014 Ryan Culpepper
;; 
;; This library is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library.  If not, see <http://www.gnu.org/licenses/>.

#lang racket/base
(require racket/contract/base
         "private/base256.rkt")
(provide (contract-out
          [signed->base256
           (-> exact-integer? bytes?)]
          [base256->signed
           (-> bytes? exact-integer?)]
          [unsigned->base256
           (-> exact-nonnegative-integer? bytes?)]
          [base256->unsigned
           (-> bytes? exact-nonnegative-integer?)]))
