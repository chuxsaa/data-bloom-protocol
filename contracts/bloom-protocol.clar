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

;; Operational functions

;; Complete reservation handover to beneficiary
(define-public (finalize-reservation-transfer (reservation-identifier uint))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (beneficiary (get beneficiary reservation-record))
        (allocation (get allocation reservation-record))
        (data-id (get data-identifier reservation-record))
      )
      (asserts! (or (is-eq tx-sender PROTOCOL_SUPERVISOR) (is-eq tx-sender (get originator reservation-record))) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get reservation-status reservation-record) "pending") ERR_STATUS_CONFLICT)
      (asserts! (<= block-height (get termination-block reservation-record)) ERR_RESERVATION_TIMEOUT)
      (match (as-contract (stx-transfer? allocation tx-sender beneficiary))
        success
          (begin
            (map-set ReservationIndex
              { reservation-identifier: reservation-identifier }
              (merge reservation-record { reservation-status: "completed" })
            )
            (print {action: "reservation_transferred", reservation-identifier: reservation-identifier, beneficiary: beneficiary, data-identifier: data-id, allocation: allocation})
            (ok true)
          )
        error ERR_OPERATION_FAILED
      )
    )
  )
)

;; Revert allocated resources to originator
(define-public (revert-reservation-allocation (reservation-identifier uint))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record))
        (allocation (get allocation reservation-record))
      )
      (asserts! (is-eq tx-sender PROTOCOL_SUPERVISOR) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get reservation-status reservation-record) "pending") ERR_STATUS_CONFLICT)
      (match (as-contract (stx-transfer? allocation tx-sender originator))
        success
          (begin
            (map-set ReservationIndex
              { reservation-identifier: reservation-identifier }
              (merge reservation-record { reservation-status: "reverted" })
            )
            (print {action: "allocation_reverted", reservation-identifier: reservation-identifier, originator: originator, allocation: allocation})
            (ok true)
          )
        error ERR_OPERATION_FAILED
      )
    )
  )
)

;; Originator requests reservation termination
(define-public (terminate-reservation (reservation-identifier uint))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record))
        (allocation (get allocation reservation-record))
      )
      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get reservation-status reservation-record) "pending") ERR_STATUS_CONFLICT)
      (asserts! (<= block-height (get termination-block reservation-record)) ERR_RESERVATION_TIMEOUT)
      (match (as-contract (stx-transfer? allocation tx-sender originator))
        success
          (begin
            (map-set ReservationIndex
              { reservation-identifier: reservation-identifier }
              (merge reservation-record { reservation-status: "terminated" })
            )
            (print {action: "reservation_terminated", reservation-identifier: reservation-identifier, originator: originator, allocation: allocation})
            (ok true)
          )
        error ERR_OPERATION_FAILED
      )
    )
  )
)

;; Extend reservation timeframe
(define-public (extend-reservation-timeframe (reservation-identifier uint) (additional-blocks uint))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (asserts! (> additional-blocks u0) ERR_INVALID_PARAMETER)
    (asserts! (<= additional-blocks u1440) ERR_INVALID_PARAMETER) ;; Max ~10 days extension
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record)) 
        (beneficiary (get beneficiary reservation-record))
        (current-end (get termination-block reservation-record))
        (revised-end (+ current-end additional-blocks))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_SUPERVISOR)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get reservation-status reservation-record) "pending") (is-eq (get reservation-status reservation-record) "accepted")) ERR_STATUS_CONFLICT)
      (map-set ReservationIndex
        { reservation-identifier: reservation-identifier }
        (merge reservation-record { termination-block: revised-end })
      )
      (print {action: "reservation_extended", reservation-identifier: reservation-identifier, requestor: tx-sender, new-termination-block: revised-end})
      (ok true)
    )
  )
)