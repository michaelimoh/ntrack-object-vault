;; NTrack Object Vault: Decentralized Objective Tracking Protocol
;;
;; A blockchain-based system for managing personal and organizational
;; objectives through cryptographic verification and temporal constraints.
;; Provides immutable record-keeping for commitment fulfillment tracking.
;;

;; Error response definitions for various failure scenarios
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_INPUT (err u400))
(define-constant ERR_NOT_FOUND (err u404))

;; Core pledge storage mapping - tracks fundamental objective data
(define-map objective-registry
    principal  ;; Entity identifier (transaction sender)
    {
        description: (string-ascii 100),  ;; Textual representation of objective
        completed: bool                   ;; Binary completion flag
    }
)

;; Temporal tracking storage - manages time-based constraints
(define-map temporal-constraints
    principal  ;; Links to objective-registry principal
    {
        deadline-block: uint,         ;; Target completion block height
        alert-dispatched: bool        ;; Alert status flag
    }
)

;; Priority classification storage - manages importance levels
(define-map priority-classifications
    principal  ;; Links to objective-registry principal
    {
        importance-level: uint        ;; Numerical priority indicator (1-3)
    }
)

;; Internal validation helper for string content verification
;; Ensures non-empty string inputs across the system
;; @param input-string: String value to validate
;; @returns: Boolean validation result
(define-private (verify-string-content (input-string (string-ascii 100)))
    (> (len input-string) u0)
)

;; Internal validation helper for numerical boundary checking
;; Validates numeric inputs within acceptable ranges
;; @param value: Numeric value to validate
;; @param lower-bound: Minimum acceptable value
;; @param upper-bound: Maximum acceptable value
;; @returns: Boolean validation result
(define-private (verify-numeric-boundaries 
    (value uint) 
    (lower-bound uint) 
    (upper-bound uint))
    (and (>= value lower-bound) (<= value upper-bound))
)

;; Primary objective registration endpoint
;; Creates new objectives in the system registry
;; @param objective-description: Human-readable objective text
;; @returns: Success message or error code
(define-public (register-new-objective 
    (objective-description (string-ascii 100)))
    (let
        (
            (requester-principal tx-sender)
            (current-record (map-get? objective-registry requester-principal))
        )
        (if (is-none current-record)
            (begin
                (if (is-eq objective-description "")
                    (err ERR_INVALID_INPUT)
                    (begin
                        (map-set objective-registry requester-principal
                            {
                                description: objective-description,
                                completed: false
                            }
                        )
                        (ok "New objective successfully registered in vault system.")
                    )
                )
            )
            (err ERR_ALREADY_EXISTS)
        )
    )
)

;; Objective modification endpoint
;; Allows updating existing objective parameters
;; @param updated-description: Modified objective description
;; @param completion-flag: Updated completion status
;; @returns: Success confirmation or error response
(define-public (modify-existing-objective
    (updated-description (string-ascii 100))
    (completion-flag bool))
    (let
        (
            (requester-principal tx-sender)
            (current-record (map-get? objective-registry requester-principal))
        )
        (if (is-some current-record)
            (begin
                (if (is-eq updated-description "")
                    (err ERR_INVALID_INPUT)
                    (begin
                        (if (or (is-eq completion-flag true) (is-eq completion-flag false))
                            (begin
                                (map-set objective-registry requester-principal
                                    {
                                        description: updated-description,
                                        completed: completion-flag
                                    }
                                )
                                (ok "Objective parameters successfully modified in vault.")
                            )
                            (err ERR_INVALID_INPUT)
                        )
                    )
                )
            )
            (err ERR_NOT_FOUND)
        )
    )
)

;; Objective assignment endpoint
;; Enables transferring objectives to other principals
;; @param recipient-principal: Target entity for assignment
;; @param assignment-description: Objective description for recipient
;; @returns: Assignment confirmation or error response
(define-public (assign-objective-to-entity
    (recipient-principal principal)
    (assignment-description (string-ascii 100)))
    (let
        (
            (recipient-record (map-get? objective-registry recipient-principal))
        )
        (if (is-none recipient-record)
            (begin
                (if (is-eq assignment-description "")
                    (err ERR_INVALID_INPUT)
                    (begin
                        (map-set objective-registry recipient-principal
                            {
                                description: assignment-description,
                                completed: false
                            }
                        )
                        (ok "Objective successfully assigned to target entity.")
                    )
                )
            )
            (err ERR_ALREADY_EXISTS)
        )
    )
)

;; System metadata accessor for version information
;; Provides contract version for compatibility verification
;; @returns: Version identifier string
(define-read-only (retrieve-system-version)
    "NexusPledgeVault v2.1.4"
)

;; System metadata accessor for priority level definitions
;; Provides available importance classifications
;; @returns: Map containing priority level definitions
(define-read-only (retrieve-priority-definitions)
    {
        minimal: u1,
        standard: u2,
        critical: u3
    }
)

;; Temporal constraint establishment endpoint
;; Sets deadline parameters for existing objectives
;; @param block-offset: Number of blocks until deadline
;; @returns: Constraint establishment confirmation or error
(define-public (configure-temporal-boundary (block-offset uint))
    (let
        (
            (requester-principal tx-sender)
            (current-record (map-get? objective-registry requester-principal))
            (target-block-height (+ block-height block-offset))
        )
        (if (is-some current-record)
            (if (> block-offset u0)
                (begin
                    (map-set temporal-constraints requester-principal
                        {
                            deadline-block: target-block-height,
                            alert-dispatched: false
                        }
                    )
                    (ok "Temporal boundary successfully configured for objective.")
                )
                (err ERR_INVALID_INPUT)
            )
            (err ERR_NOT_FOUND)
        )
    )
)

;; Priority level configuration endpoint
;; Establishes importance classification for objectives
;; @param priority-value: Importance level (1-3 scale)
;; @returns: Priority configuration confirmation or error
(define-public (configure-priority-level (priority-value uint))
    (let
        (
            (requester-principal tx-sender)
            (current-record (map-get? objective-registry requester-principal))
        )
        (if (is-some current-record)
            (if (and (>= priority-value u1) (<= priority-value u3))
                (begin
                    (map-set priority-classifications requester-principal
                        {
                            importance-level: priority-value
                        }
                    )
                    (ok "Priority classification successfully configured.")
                )
                (err ERR_INVALID_INPUT)
            )
            (err ERR_NOT_FOUND)
        )
    )
)

;; Objective status verification endpoint
;; Provides comprehensive objective metadata without state modification
;; @returns: Detailed objective status information
(define-public (query-objective-status)
    (let
        (
            (requester-principal tx-sender)
            (current-record (map-get? objective-registry requester-principal))
        )
        (if (is-some current-record)
            (let
                (
                    (objective-data (unwrap! current-record ERR_NOT_FOUND))
                    (description-content (get description objective-data))
                    (completion-status (get completed objective-data))
                )
                (ok {
                    registry-status: true,
                    content-length: (len description-content),
                    fulfillment-state: completion-status
                })
            )
            (ok {
                registry-status: false,
                content-length: u0,
                fulfillment-state: false
            })
        )
    )
)

