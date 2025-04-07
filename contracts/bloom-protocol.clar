;; DataBloom Storage Protocol - Decentralized Data Reservation System

;; Administrative constants
(define-constant PROTOCOL_SUPERVISOR tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_MISSING_RESERVATION (err u101))
(define-constant ERR_STATUS_CONFLICT (err u102))
(define-constant ERR_OPERATION_FAILED (err u103))
(define-constant ERR_INVALID_IDENTIFIER (err u104))
(define-constant ERR_INVALID_PARAMETER (err u105))
(define-constant ERR_INVALID_ORIGINATOR (err u106))
(define-constant ERR_RESERVATION_TIMEOUT (err u107))
(define-constant RESERVATION_DURATION_BLOCKS u1008) 

;; Reservation data structure
(define-map ReservationIndex
  { reservation-identifier: uint }
  {
    originator: principal,
    beneficiary: principal,
    data-identifier: uint,
    allocation: uint,
    reservation-status: (string-ascii 10),
    creation-block: uint,
    termination-block: uint
  }
)


;; Auxiliary functions
(define-private (valid-beneficiary? (beneficiary principal))
  (and 
    (not (is-eq beneficiary tx-sender))
    (not (is-eq beneficiary (as-contract tx-sender)))
  )
)

(define-private (valid-reservation-id? (reservation-identifier uint))
  (<= reservation-identifier (var-get latest-reservation-id))
)

;; System tracking variable
(define-data-var latest-reservation-id uint u0)

