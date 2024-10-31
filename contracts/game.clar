;; MetaMythics Game Contract
;; Handles player registration, character creation, and basic game mechanics

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-reward (err u103))
(define-constant err-invalid-rarity (err u104))

;; Define data variables
(define-data-var min-level uint u1)
(define-data-var max-level uint u100)
(define-data-var base-reward uint u10)
(define-data-var min-reward uint u1)
(define-data-var max-reward uint u1000)
(define-data-var min-rarity uint u1)
(define-data-var max-rarity uint u5)

;; Define data maps
(define-map Players
    principal
    {
        name: (string-ascii 24),
        level: uint,
        experience: uint,
        rewards: uint,
        characters: (list 5 uint)
    }
)

(define-map Characters
    uint
    {
        id: uint,
        type: (string-ascii 12),
        power: uint,
        rarity: uint
    }
)

;; Initialize character counter
(define-data-var character-counter uint u0)

;; Read-only functions
(define-read-only (get-player-info (player principal))
    (map-get? Players player)
)

(define-read-only (get-character-info (character-id uint))
    (map-get? Characters character-id)
)

(define-read-only (get-reward-limits)
    (ok {
        min: (var-get min-reward),
        max: (var-get max-reward),
        current: (var-get base-reward)
    })
)

(define-read-only (get-rarity-limits)
    (ok {
        min: (var-get min-rarity),
        max: (var-get max-rarity)
    })
)

;; Helper functions for validation
(define-private (is-valid-reward (reward uint))
    (and 
        (>= reward (var-get min-reward))
        (<= reward (var-get max-reward))
    )
)

(define-private (is-valid-rarity (rarity uint))
    (and 
        (>= rarity (var-get min-rarity))
        (<= rarity (var-get max-rarity))
    )
)

(define-private (calculate-power (rarity uint))
    (* rarity u10)
)

;; Public functions
(define-public (register-player (name (string-ascii 24)))
    (let ((player tx-sender))
        (if (is-some (map-get? Players player))
            err-already-exists
            (ok (map-set Players 
                player 
                {
                    name: name,
                    level: u1,
                    experience: u0,
                    rewards: u0,
                    characters: (list)
                }
            ))
        )
    )
)

(define-public (mint-character (character-type (string-ascii 12)) (rarity uint))
    (let (
        (player tx-sender)
        (new-id (+ (var-get character-counter) u1))
    )
        (asserts! (is-valid-rarity rarity) err-invalid-rarity)
        (if (is-none (map-get? Players player))
            err-not-found
            (begin
                (var-set character-counter new-id)
                (map-set Characters
                    new-id
                    {
                        id: new-id,
                        type: character-type,
                        power: (calculate-power rarity),
                        rarity: rarity
                    }
                )
                (ok new-id)
            )
        )
    )
)

(define-public (earn-reward (character-id uint))
    (let (
        (player tx-sender)
        (player-info (unwrap! (map-get? Players player) err-not-found))
        (character (unwrap! (map-get? Characters character-id) err-not-found))
        (reward-amount (* (var-get base-reward) (get power character)))
    )
        (ok (map-set Players
            player
            (merge player-info
                {
                    rewards: (+ (get rewards player-info) reward-amount)
                }
            )
        ))
    )
)

(define-public (level-up)
    (let (
        (player tx-sender)
        (player-info (unwrap! (map-get? Players player) err-not-found))
        (current-level (get level player-info))
    )
        (if (>= current-level (var-get max-level))
            err-already-exists
            (ok (map-set Players
                player
                (merge player-info
                    {
                        level: (+ current-level u1),
                        experience: u0
                    }
                )
            ))
        )
    )
)

;; Admin functions with enhanced validation
(define-public (update-base-reward (new-reward uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-valid-reward new-reward) err-invalid-reward)
        (ok (var-set base-reward new-reward))
    )
)

;; Admin function to update reward limits
(define-public (update-reward-limits (new-min uint) (new-max uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (< new-min new-max) err-invalid-reward)
        (var-set min-reward new-min)
        (var-set max-reward new-max)
        (ok true)
    )
)

;; Admin function to update rarity limits
(define-public (update-rarity-limits (new-min uint) (new-max uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (< new-min new-max) err-invalid-rarity)
        (var-set min-rarity new-min)
        (var-set max-rarity new-max)
        (ok true)
    )
)