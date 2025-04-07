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

;; Reclaim expired reservation allocations
(define-public (reclaim-expired-reservation (reservation-identifier uint))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record))
        (allocation (get allocation reservation-record))
        (expiry (get termination-block reservation-record))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_SUPERVISOR)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get reservation-status reservation-record) "pending") (is-eq (get reservation-status reservation-record) "accepted")) ERR_STATUS_CONFLICT)
      (asserts! (> block-height expiry) (err u108)) ;; Must be expired
      (match (as-contract (stx-transfer? allocation tx-sender originator))
        success
          (begin
            (map-set ReservationIndex
              { reservation-identifier: reservation-identifier }
              (merge reservation-record { reservation-status: "expired" })
            )
            (print {action: "expired_reservation_reclaimed", reservation-identifier: reservation-identifier, originator: originator, allocation: allocation})
            (ok true)
          )
        error ERR_OPERATION_FAILED
      )
    )
  )
)

;; Initiate reservation challenge
(define-public (challenge-reservation (reservation-identifier uint) (justification (string-ascii 50)))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record))
        (beneficiary (get beneficiary reservation-record))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get reservation-status reservation-record) "pending") (is-eq (get reservation-status reservation-record) "accepted")) ERR_STATUS_CONFLICT)
      (asserts! (<= block-height (get termination-block reservation-record)) ERR_RESERVATION_TIMEOUT)
      (map-set ReservationIndex
        { reservation-identifier: reservation-identifier }
        (merge reservation-record { reservation-status: "challenged" })
      )
      (print {action: "reservation_challenged", reservation-identifier: reservation-identifier, challenger: tx-sender, justification: justification})
      (ok true)
    )
  )
)

;; Add cryptographic verification
(define-public (append-cryptographic-verification (reservation-identifier uint) (cryptographic-proof (buff 65)))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record))
        (beneficiary (get beneficiary reservation-record))
      )
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get reservation-status reservation-record) "pending") (is-eq (get reservation-status reservation-record) "accepted")) ERR_STATUS_CONFLICT)
      (print {action: "cryptographic_verification_complete", reservation-identifier: reservation-identifier, verifier: tx-sender, cryptographic-proof: cryptographic-proof})
      (ok true)
    )
  )
)

;; Register alternate contact
(define-public (register-alternate-contact (reservation-identifier uint) (alternate-contact principal))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record))
      )
      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      (asserts! (not (is-eq alternate-contact tx-sender)) (err u111)) ;; Alternate contact must be different
      (asserts! (is-eq (get reservation-status reservation-record) "pending") ERR_STATUS_CONFLICT)
      (print {action: "alternate_registered", reservation-identifier: reservation-identifier, originator: originator, alternate: alternate-contact})
      (ok true)
    )
  )
)

;; Schedule maintenance with delay
(define-public (schedule-maintenance-procedure (procedure-type (string-ascii 20)) (procedure-params (list 10 uint)))
  (begin
    (asserts! (is-eq tx-sender PROTOCOL_SUPERVISOR) ERR_UNAUTHORIZED)
    (asserts! (> (len procedure-params) u0) ERR_INVALID_PARAMETER)
    (let
      (
        (execution-time (+ block-height u144)) ;; 24 hours delay
      )
      (print {action: "procedure_scheduled", procedure-type: procedure-type, procedure-params: procedure-params, execution-time: execution-time})
      (ok execution-time)
    )
  )
)

;; Resolve challenge through mediation
(define-public (mediate-challenge (reservation-identifier uint) (originator-percentage uint))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (asserts! (is-eq tx-sender PROTOCOL_SUPERVISOR) ERR_UNAUTHORIZED)
    (asserts! (<= originator-percentage u100) ERR_INVALID_PARAMETER) ;; Percentage must be 0-100
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record))
        (beneficiary (get beneficiary reservation-record))
        (allocation (get allocation reservation-record))
        (originator-allocation (/ (* allocation originator-percentage) u100))
        (beneficiary-allocation (- allocation originator-allocation))
      )
      (asserts! (is-eq (get reservation-status reservation-record) "challenged") (err u112)) ;; Must be challenged
      (asserts! (<= block-height (get termination-block reservation-record)) ERR_RESERVATION_TIMEOUT)

      ;; Distribute originator's portion
      (unwrap! (as-contract (stx-transfer? originator-allocation tx-sender originator)) ERR_OPERATION_FAILED)

      ;; Distribute beneficiary's portion
      (unwrap! (as-contract (stx-transfer? beneficiary-allocation tx-sender beneficiary)) ERR_OPERATION_FAILED)

      (map-set ReservationIndex
        { reservation-identifier: reservation-identifier }
        (merge reservation-record { reservation-status: "mediated" })
      )
      (print {action: "challenge_mediated", reservation-identifier: reservation-identifier, originator: originator, beneficiary: beneficiary, 
              originator-allocation: originator-allocation, beneficiary-allocation: beneficiary-allocation, originator-percentage: originator-percentage})
      (ok true)
    )
  )
)

;; Add secondary verification for high-value reservations
(define-public (add-secondary-verification (reservation-identifier uint) (verifier principal))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record))
        (allocation (get allocation reservation-record))
      )
      ;; Only for high-value reservations (> 1000 STX)
      (asserts! (> allocation u1000) (err u120))
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender PROTOCOL_SUPERVISOR)) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get reservation-status reservation-record) "pending") ERR_STATUS_CONFLICT)
      (print {action: "verification_added", reservation-identifier: reservation-identifier, verifier: verifier, requestor: tx-sender})
      (ok true)
    )
  )
)

;; Suspend problematic reservation
(define-public (suspend-problematic-reservation (reservation-identifier uint) (justification (string-ascii 100)))
  (begin
    (asserts! (valid-reservation-id? reservation-identifier) ERR_INVALID_IDENTIFIER)
    (let
      (
        (reservation-record (unwrap! (map-get? ReservationIndex { reservation-identifier: reservation-identifier }) ERR_MISSING_RESERVATION))
        (originator (get originator reservation-record))
        (beneficiary (get beneficiary reservation-record))
      )
      (asserts! (or (is-eq tx-sender PROTOCOL_SUPERVISOR) (is-eq tx-sender originator) (is-eq tx-sender beneficiary)) ERR_UNAUTHORIZED)
      (asserts! (or (is-eq (get reservation-status reservation-record) "pending") 
                   (is-eq (get reservation-status reservation-record) "accepted")) 
                ERR_STATUS_CONFLICT)
      (map-set ReservationIndex
        { reservation-identifier: reservation-identifier }
        (merge reservation-record { reservation-status: "suspended" })
      )
      (print {action: "reservation_suspended", reservation-identifier: reservation-identifier, reporter: tx-sender, justification: justification})
      (ok true)
    )
  )
)
