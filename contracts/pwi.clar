(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-policy-not-found (err u102))
(define-constant err-policy-expired (err u103))
(define-constant err-already-claimed (err u104))
(define-constant err-invalid-parameters (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-oracle-only (err u107))
(define-constant err-policy-active (err u108))

(define-data-var oracle-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
(define-data-var min-policy-duration uint u1440)
(define-data-var max-policy-duration uint u43200)
(define-data-var protocol-fee-percent uint u5)
(define-data-var total-stx-locked uint u0)
(define-data-var total-policies-created uint u0)
(define-data-var total-policies-claimed uint u0)

(define-map policies
  { policy-id: uint }
  {
    owner: principal,
    premium: uint,
    coverage: uint,
    start-block: uint,
    end-block: uint,
    location-id: (string-ascii 64),
    weather-condition: (string-ascii 32),
    threshold: uint,
    claimed: bool,
    active: bool
  }
)

(define-map location-data
  { location-id: (string-ascii 64), block-heightt: uint }
  { 
    temperature: int,
    rainfall: uint,
    wind-speed: uint,
    humidity: uint,
    updated-by: principal
  }
)

(define-public (set-oracle (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set oracle-address new-oracle))
  )
)

(define-public (update-policy-duration-limits (new-min uint) (new-max uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (< new-min new-max) err-invalid-parameters)
    (var-set min-policy-duration new-min)
    (var-set max-policy-duration new-max)
    (ok true)
  )
)

(define-public (update-protocol-fee (new-fee-percent uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-percent u20) err-invalid-parameters)
    (var-set protocol-fee-percent new-fee-percent)
    (ok true)
  )
)

(define-public (create-policy 
  (premium uint) 
  (coverage uint) 
  (duration uint) 
  (location-id (string-ascii 64)) 
  (weather-condition (string-ascii 32)) 
  (threshold uint))
  (let 
    (
      (policy-id (+ (var-get total-policies-created) u1))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration))
      (protocol-fee (/ (* premium (var-get protocol-fee-percent)) u100))
    )
    (asserts! (>= coverage premium) err-invalid-parameters)
    (asserts! (and (>= duration (var-get min-policy-duration)) (<= duration (var-get max-policy-duration))) err-invalid-parameters)
    (asserts! (is-valid-weather-condition weather-condition) err-invalid-parameters)
    
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (try! (stx-transfer? protocol-fee (as-contract tx-sender) contract-owner))
    (var-set total-stx-locked (+ (var-get total-stx-locked) (- premium protocol-fee)))
    (var-set total-policies-created policy-id)
    
    (map-set policies
      { policy-id: policy-id }
      {
        owner: tx-sender,
        premium: premium,
        coverage: coverage,
        start-block: start-block,
        end-block: end-block,
        location-id: location-id,
        weather-condition: weather-condition,
        threshold: threshold,
        claimed: false,
        active: true
      }
    )
    (ok policy-id)
  )
)

(define-public (cancel-policy (policy-id uint))
  (let 
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-policy-not-found))
      (refund-amount (/ (* (get premium policy) u80) u100))
    )
    (asserts! (is-eq tx-sender (get owner policy)) err-not-authorized)
    (asserts! (get active policy) err-policy-not-found)
    (asserts! (not (get claimed policy)) err-already-claimed)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { active: false })
    )
    
    (var-set total-stx-locked (- (var-get total-stx-locked) (get premium policy)))
    (try! (as-contract (stx-transfer? refund-amount contract-caller (get owner policy))))
    (ok true)
  )
)

(define-public (submit-weather-data 
  (location-id (string-ascii 64)) 
  (temperature int) 
  (rainfall uint) 
  (wind-speed uint) 
  (humidity uint))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle-address)) err-oracle-only)
    (map-set location-data
      { location-id: location-id, block-heightt: stacks-block-height }
      {
        temperature: temperature,
        rainfall: rainfall,
        wind-speed: wind-speed,
        humidity: humidity,
        updated-by: tx-sender
      }
    )
    (ok true)
  )
)

(define-public (claim-policy (policy-id uint))
  (let 
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-policy-not-found))
      (latest-data (get-latest-weather-data (get location-id policy)))
    )
    (asserts! (is-eq tx-sender (get owner policy)) err-not-authorized)
    (asserts! (get active policy) err-policy-not-found)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (<= stacks-block-height (get end-block policy)) err-policy-expired)
    
    (asserts! (is-claim-valid policy latest-data) err-not-authorized)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { claimed: true })
    )
    
    (var-set total-stx-locked (- (var-get total-stx-locked) (get premium policy)))
    (var-set total-policies-claimed (+ (var-get total-policies-claimed) u1))
    
    (try! (as-contract (stx-transfer? (get coverage policy) contract-caller (get owner policy))))
    (ok true)
  )
)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-weather-data (location-id (string-ascii 64)) (heightt uint))
  (map-get? location-data { location-id: location-id, block-heightt: stacks-block-height })
)

(define-read-only (get-latest-weather-data (location-id (string-ascii 64)))
  (map-get? location-data { location-id: location-id, block-heightt: stacks-block-height })
)

(define-read-only (get-contract-info)
  {
    oracle: (var-get oracle-address),
    min-duration: (var-get min-policy-duration),
    max-duration: (var-get max-policy-duration),
    fee-percent: (var-get protocol-fee-percent),
    total-locked: (var-get total-stx-locked),
    policies-created: (var-get total-policies-created),
    policies-claimed: (var-get total-policies-claimed)
  }
)

(define-private (is-valid-weather-condition (condition (string-ascii 32)))
  (or
    (is-eq condition "rainfall")
    (is-eq condition "drought")
    (is-eq condition "temperature")
    (is-eq condition "wind-speed")
    (is-eq condition "humidity")
  )
)

(define-private (is-claim-valid (policy {
    owner: principal,
    premium: uint,
    coverage: uint,
    start-block: uint,
    end-block: uint,
    location-id: (string-ascii 64),
    weather-condition: (string-ascii 32),
    threshold: uint,
    claimed: bool,
    active: bool
  }) 
  (weather-data (optional {
    temperature: int,
    rainfall: uint,
    wind-speed: uint,
    humidity: uint,
    updated-by: principal
  })))
  (match weather-data
    data (check-threshold (get weather-condition policy) (get threshold policy) data)
    false
  )
)

(define-private (check-threshold (condition (string-ascii 32)) (threshold uint) (data {
    temperature: int,
    rainfall: uint,
    wind-speed: uint,
    humidity: uint,
    updated-by: principal
  }))
  (if (is-eq condition "rainfall")
    (>= (get rainfall data) threshold)
    (if (is-eq condition "drought")
      (<= (get rainfall data) threshold)
      (if (is-eq condition "temperature")
        (>= (to-uint (get temperature data)) threshold)
        (if (is-eq condition "wind-speed")
          (>= (get wind-speed data) threshold)
          (if (is-eq condition "humidity")
            (>= (get humidity data) threshold)
            false)))))
)




(define-constant err-transfer-failed (err u109))

(define-public (transfer-policy (policy-id uint) (recipient principal))
    (let 
        ((policy (unwrap! (map-get? policies { policy-id: policy-id }) err-policy-not-found)))
        (asserts! (is-eq tx-sender (get owner policy)) err-not-authorized)
        (asserts! (get active policy) err-policy-not-found)
        (asserts! (not (get claimed policy)) err-already-claimed)
        
        (map-set policies
            { policy-id: policy-id }
            (merge policy { owner: recipient })
        )
        (ok true)
    )
)


(define-map multi-location-policies
    { policy-id: uint }
    {
        owner: principal,
        premium: uint,
        coverage: uint,
        start-block: uint,
        end-block: uint,
        locations: (list 5 (string-ascii 64)),
        weather-condition: (string-ascii 32),
        threshold: uint,
        claimed: bool,
        active: bool
    }
)

(define-public (create-multi-location-policy 
    (premium uint) 
    (coverage uint) 
    (duration uint) 
    (locations (list 5 (string-ascii 64))) 
    (weather-condition (string-ascii 32)) 
    (threshold uint))
    (let 
        (
            (policy-id (+ (var-get total-policies-created) u1))
            (start-block stacks-block-height)
            (end-block (+ stacks-block-height duration))
            (protocol-fee (/ (* premium (var-get protocol-fee-percent)) u100))
        )
        (asserts! (>= coverage premium) err-invalid-parameters)
        (asserts! (and (>= duration (var-get min-policy-duration)) (<= duration (var-get max-policy-duration))) err-invalid-parameters)
        (asserts! (is-valid-weather-condition weather-condition) err-invalid-parameters)
        
        (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
        (try! (stx-transfer? protocol-fee (as-contract tx-sender) contract-owner))
        (var-set total-stx-locked (+ (var-get total-stx-locked) (- premium protocol-fee)))
        (var-set total-policies-created policy-id)
        
        (map-set multi-location-policies
            { policy-id: policy-id }
            {
                owner: tx-sender,
                premium: premium,
                coverage: coverage,
                start-block: start-block,
                end-block: end-block,
                locations: locations,
                weather-condition: weather-condition,
                threshold: threshold,
                claimed: false,
                active: true
            }
        )
        (ok policy-id)
    )
)


(define-constant err-payout-reward-too-high (err u110))
(define-constant err-policy-not-eligible (err u111))

(define-data-var automated-payout-reward uint u1000000)

(define-public (set-automated-payout-reward (reward uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= reward u10000000) err-payout-reward-too-high)
    (var-set automated-payout-reward reward)
    (ok true)
  )
)

(define-public (trigger-automated-payout (policy-id uint))
  (let 
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-policy-not-found))
      (latest-data (get-latest-weather-data (get location-id policy)))
      (reward (var-get automated-payout-reward))
    )
    (asserts! (get active policy) err-policy-not-found)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (<= stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (is-claim-valid policy latest-data) err-policy-not-eligible)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { claimed: true })
    )
    
    (var-set total-stx-locked (- (var-get total-stx-locked) (get premium policy)))
    (var-set total-policies-claimed (+ (var-get total-policies-claimed) u1))
    
    (try! (as-contract (stx-transfer? (get coverage policy) contract-caller (get owner policy))))
    (try! (as-contract (stx-transfer? reward contract-caller tx-sender)))
    (ok true)
  )
)

(define-read-only (is-policy-eligible-for-payout (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy 
    (let 
      (
        (latest-data (get-latest-weather-data (get location-id policy)))
      )
      (and 
        (get active policy)
        (not (get claimed policy))
        (<= stacks-block-height (get end-block policy))
        (is-claim-valid policy latest-data)
      )
    )
    false
  )
)

(define-read-only (get-automated-payout-info)
  {
    reward-amount: (var-get automated-payout-reward),
    contract-balance: (stx-get-balance (as-contract tx-sender))
  }
)