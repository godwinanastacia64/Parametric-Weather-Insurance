;; Community Risk Pools Contract
;; Allows multiple users to pool resources for shared weather insurance policies

;; Error constants
(define-constant err-owner-only (err u200))
(define-constant err-not-authorized (err u201))
(define-constant err-pool-not-found (err u202))
(define-constant err-pool-full (err u203))
(define-constant err-invalid-parameters (err u204))
(define-constant err-insufficient-contribution (err u205))
(define-constant err-pool-closed (err u206))
(define-constant err-pool-active (err u207))
(define-constant err-already-member (err u208))
(define-constant err-not-member (err u209))
(define-constant err-pool-not-ready (err u210))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Pool configuration
(define-data-var max-pool-members uint u50)
(define-data-var min-pool-contribution uint u500000) ;; 0.5 STX minimum
(define-data-var pool-creation-fee uint u100000) ;; 0.1 STX fee
(define-data-var total-pools-created uint u0)

;; Pool data structure
(define-map insurance-pools
  { pool-id: uint }
  {
    creator: principal,
    location-id: (string-ascii 64),
    weather-condition: (string-ascii 32),
    threshold: uint,
    target-coverage: uint,
    total-contributions: uint,
    member-count: uint,
    pool-status: (string-ascii 20), ;; "open", "active", "closed", "claimed"
    created-at: uint,
    activation-deadline: uint,
    policy-duration: uint
  }
)

;; Pool membership tracking
(define-map pool-members
  { pool-id: uint, member: principal }
  {
    contribution: uint,
    share-percentage: uint,
    joined-at: uint,
    eligible-for-payout: bool
  }
)

;; Pool member lists (for easier iteration)
(define-map pool-member-list
  { pool-id: uint }
  { members: (list 50 principal) }
)

;; Pool claims tracking
(define-map pool-claims
  { pool-id: uint }
  {
    claimed-at: uint,
    total-payout: uint,
    weather-trigger-value: uint,
    claimed-by: principal
  }
)

;; Create a new community risk pool
(define-public (create-pool
  (location-id (string-ascii 64))
  (weather-condition (string-ascii 32))
  (threshold uint)
  (target-coverage uint)
  (activation-deadline uint)
  (policy-duration uint))
  (let
    (
      (pool-id (+ (var-get total-pools-created) u1))
      (creation-fee (var-get pool-creation-fee))
    )
    (asserts! (> target-coverage u0) err-invalid-parameters)
    (asserts! (> activation-deadline stacks-block-height) err-invalid-parameters)
    (asserts! (and (>= policy-duration u1440) (<= policy-duration u43200)) err-invalid-parameters)
    
    ;; Pay creation fee
    (try! (stx-transfer? creation-fee tx-sender contract-owner))
    
    ;; Create the pool
    (map-set insurance-pools
      { pool-id: pool-id }
      {
        creator: tx-sender,
        location-id: location-id,
        weather-condition: weather-condition,
        threshold: threshold,
        target-coverage: target-coverage,
        total-contributions: u0,
        member-count: u0,
        pool-status: "open",
        created-at: stacks-block-height,
        activation-deadline: activation-deadline,
        policy-duration: policy-duration
      }
    )
    
    ;; Initialize empty member list
    (map-set pool-member-list
      { pool-id: pool-id }
      { members: (list) }
    )
    
    (var-set total-pools-created pool-id)
    (ok pool-id)
  )
)

;; Join an existing pool with a contribution
(define-public (join-pool (pool-id uint) (contribution uint))
  (let
    (
      (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) err-pool-not-found))
      (existing-member (map-get? pool-members { pool-id: pool-id, member: tx-sender }))
      (member-list (unwrap! (map-get? pool-member-list { pool-id: pool-id }) err-pool-not-found))
      (min-contribution (var-get min-pool-contribution))
      (max-members (var-get max-pool-members))
    )
    ;; Validation checks
    (asserts! (is-eq (get pool-status pool) "open") err-pool-closed)
    (asserts! (> (get activation-deadline pool) stacks-block-height) err-pool-closed)
    (asserts! (< (get member-count pool) max-members) err-pool-full)
    (asserts! (>= contribution min-contribution) err-insufficient-contribution)
    (asserts! (is-none existing-member) err-already-member)
    
    ;; Transfer contribution to contract
    (try! (stx-transfer? contribution tx-sender (as-contract tx-sender)))
    
    ;; Calculate share percentage
    (let
      (
        (new-total (+ (get total-contributions pool) contribution))
        (share-percentage (/ (* contribution u10000) (+ (get target-coverage pool) contribution)))
      )
      ;; Add member to pool
      (map-set pool-members
        { pool-id: pool-id, member: tx-sender }
        {
          contribution: contribution,
          share-percentage: share-percentage,
          joined-at: stacks-block-height,
          eligible-for-payout: true
        }
      )
      
      ;; Update pool data
      (map-set insurance-pools
        { pool-id: pool-id }
        (merge pool {
          total-contributions: new-total,
          member-count: (+ (get member-count pool) u1)
        })
      )
      
      ;; Update member list
      (map-set pool-member-list
        { pool-id: pool-id }
        { members: (unwrap! (as-max-len? (append (get members member-list) tx-sender) u50) err-pool-full) }
      )
      
      (ok true)
    )
  )
)

;; Activate a pool when ready (sufficient funds or deadline reached)
(define-public (activate-pool (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) err-pool-not-found))
    )
    (asserts! (or 
      (is-eq tx-sender (get creator pool))
      (>= stacks-block-height (get activation-deadline pool))
    ) err-not-authorized)
    (asserts! (is-eq (get pool-status pool) "open") err-pool-closed)
    (asserts! (>= (get total-contributions pool) (/ (get target-coverage pool) u2)) err-insufficient-contribution)
    
    ;; Update pool status
    (map-set insurance-pools
      { pool-id: pool-id }
      (merge pool { pool-status: "active" })
    )
    (ok true)
  )
)

;; Check if pool is eligible for claim based on weather conditions
(define-public (claim-pool-payout (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) err-pool-not-found))
      (weather-data (contract-call? .pwi get-latest-weather-data (get location-id pool)))
    )
    (asserts! (is-eq (get pool-status pool) "active") err-pool-not-ready)
    (asserts! (is-some weather-data) err-invalid-parameters)
    
    ;; Check if weather conditions trigger payout
    (asserts! (is-weather-trigger-met pool (unwrap! weather-data err-invalid-parameters)) err-not-authorized)
    
    ;; Execute pool payout
    (let
      (
        (member-list-data (unwrap! (map-get? pool-member-list { pool-id: pool-id }) err-pool-not-found))
        (total-payout (get total-contributions pool))
      )
      ;; Update pool status
      (map-set insurance-pools
        { pool-id: pool-id }
        (merge pool { pool-status: "claimed" })
      )
      
      ;; Record claim details
      (map-set pool-claims
        { pool-id: pool-id }
        {
          claimed-at: stacks-block-height,
          total-payout: total-payout,
          weather-trigger-value: (get-weather-metric-value (get weather-condition pool) (unwrap! weather-data err-invalid-parameters)),
          claimed-by: tx-sender
        }
      )
      
      ;; Distribute payouts to all eligible members
      (ok (fold distribute-member-payout (get members member-list-data) { pool-id: pool-id, total-payout: total-payout, success: true }))
    )
  )
)

;; Distribute payout to individual pool member
(define-private (distribute-member-payout 
  (member principal)
  (context { pool-id: uint, total-payout: uint, success: bool }))
  (let
    (
      (member-data (map-get? pool-members { pool-id: (get pool-id context), member: member }))
    )
    (match member-data
      data
      (if (get eligible-for-payout data)
        (let
          (
            (member-payout (/ (* (get total-payout context) (get share-percentage data)) u10000))
          )
          ;; Transfer payout to member
          (match (as-contract (stx-transfer? member-payout contract-caller member))
            success context
            error (merge context { success: false })
          )
        )
        context
      )
      context
    )
  )
)

;; Leave a pool before activation (get refund)
(define-public (leave-pool (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) err-pool-not-found))
      (member-data (unwrap! (map-get? pool-members { pool-id: pool-id, member: tx-sender }) err-not-member))
    )
    (asserts! (is-eq (get pool-status pool) "open") err-pool-closed)
    (asserts! (< stacks-block-height (get activation-deadline pool)) err-pool-closed)
    
    ;; Refund contribution (minus small penalty)
    (let
      (
        (refund-amount (/ (* (get contribution member-data) u95) u100)) ;; 5% penalty for leaving
      )
      ;; Remove member from pool
      (map-delete pool-members { pool-id: pool-id, member: tx-sender })
      
      ;; Update pool totals
      (map-set insurance-pools
        { pool-id: pool-id }
        (merge pool {
          total-contributions: (- (get total-contributions pool) (get contribution member-data)),
          member-count: (- (get member-count pool) u1)
        })
      )
      
      ;; Issue refund
      (try! (as-contract (stx-transfer? refund-amount contract-caller tx-sender)))
      (ok refund-amount)
    )
  )
)

;; Helper function to check if weather conditions meet pool trigger
(define-private (is-weather-trigger-met 
  (pool {creator: principal, location-id: (string-ascii 64), weather-condition: (string-ascii 32), threshold: uint, target-coverage: uint, total-contributions: uint, member-count: uint, pool-status: (string-ascii 20), created-at: uint, activation-deadline: uint, policy-duration: uint})
  (weather-data {temperature: int, rainfall: uint, wind-speed: uint, humidity: uint, updated-by: principal}))
  (let
    (
      (condition (get weather-condition pool))
      (threshold (get threshold pool))
    )
    (if (is-eq condition "rainfall")
      (>= (get rainfall weather-data) threshold)
      (if (is-eq condition "drought")
        (<= (get rainfall weather-data) threshold)
        (if (is-eq condition "temperature")
          (>= (to-uint (get temperature weather-data)) threshold)
          (if (is-eq condition "wind-speed")
            (>= (get wind-speed weather-data) threshold)
            (if (is-eq condition "humidity")
              (>= (get humidity weather-data) threshold)
              false)))))
  )
)

;; Helper function to get weather metric value
(define-private (get-weather-metric-value 
  (condition (string-ascii 32)) 
  (weather-data {temperature: int, rainfall: uint, wind-speed: uint, humidity: uint, updated-by: principal}))
  (if (is-eq condition "temperature")
    (to-uint (get temperature weather-data))
    (if (is-eq condition "rainfall")
      (get rainfall weather-data)
      (if (is-eq condition "wind-speed")
        (get wind-speed weather-data)
        (if (is-eq condition "humidity")
          (get humidity weather-data)
          u0))))
)

;; Admin function to update pool settings
(define-public (update-pool-settings (max-members uint) (min-contribution uint) (creation-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= max-members u2) (<= max-members u100)) err-invalid-parameters)
    (asserts! (>= min-contribution u100000) err-invalid-parameters)
    (var-set max-pool-members max-members)
    (var-set min-pool-contribution min-contribution)
    (var-set pool-creation-fee creation-fee)
    (ok true)
  )
)

;; Read-only functions

;; Get pool information
(define-read-only (get-pool (pool-id uint))
  (map-get? insurance-pools { pool-id: pool-id })
)

;; Get member information
(define-read-only (get-pool-member (pool-id uint) (member principal))
  (map-get? pool-members { pool-id: pool-id, member: member })
)

;; Get pool member list
(define-read-only (get-pool-members (pool-id uint))
  (map-get? pool-member-list { pool-id: pool-id })
)

;; Get pool claim information
(define-read-only (get-pool-claim (pool-id uint))
  (map-get? pool-claims { pool-id: pool-id })
)

;; Get system configuration
(define-read-only (get-pool-system-info)
  {
    max-pool-members: (var-get max-pool-members),
    min-pool-contribution: (var-get min-pool-contribution),
    pool-creation-fee: (var-get pool-creation-fee),
    total-pools-created: (var-get total-pools-created)
  }
)

;; Calculate member's potential payout share
(define-read-only (calculate-member-payout (pool-id uint) (member principal))
  (let
    (
      (pool (map-get? insurance-pools { pool-id: pool-id }))
      (member-data (map-get? pool-members { pool-id: pool-id, member: member }))
    )
    (match pool
      pool-info
      (match member-data
        member-info
        (some (/ (* (get total-contributions pool-info) (get share-percentage member-info)) u10000))
        none
      )
      none
    )
  )
)

;; Check if pool is ready for activation
(define-read-only (is-pool-ready-for-activation (pool-id uint))
  (match (map-get? insurance-pools { pool-id: pool-id })
    pool
    (and
      (is-eq (get pool-status pool) "open")
      (>= (get total-contributions pool) (/ (get target-coverage pool) u2))
      (>= (get member-count pool) u2)
    )
    false
  )
)

;; Check if pool is eligible for weather-based payout
(define-read-only (is-pool-eligible-for-payout (pool-id uint))
  (match (map-get? insurance-pools { pool-id: pool-id })
    pool
    (let
      (
        (weather-data (contract-call? .pwi get-latest-weather-data (get location-id pool)))
      )
      (match weather-data
        data
        (and
          (is-eq (get pool-status pool) "active")
          (is-weather-trigger-met pool data)
        )
        false
      )
    )
    false
  )
)
