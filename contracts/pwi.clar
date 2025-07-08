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

(define-constant err-data-point-exists (err u112))
(define-constant err-invalid-timeframe (err u113))
(define-constant err-invalid-location (err u114))
(define-constant err-insufficient-data (err u115))
(define-constant err-invalid-metric (err u116))

(define-data-var max-historical-entries uint u1000)
(define-data-var data-access-fee uint u100000)
(define-data-var total-weather-entries uint u0)

(define-map weather-history
  { location-id: (string-ascii 64), timestamp: uint }
  {
    temperature: int,
    rainfall: uint,
    wind-speed: uint,
    humidity: uint,
    recorded-by: principal,
    verified: bool
  }
)

(define-map location-stats
  { location-id: (string-ascii 64) }
  {
    total-entries: uint,
    avg-temperature: int,
    avg-rainfall: uint,
    avg-wind-speed: uint,
    avg-humidity: uint,
    max-temperature: int,
    min-temperature: int,
    max-rainfall: uint,
    max-wind-speed: uint,
    max-humidity: uint,
    last-updated: uint
  }
)

(define-map risk-analysis
  { location-id: (string-ascii 64), metric: (string-ascii 32) }
  {
    risk-score: uint,
    volatility-index: uint,
    trend-direction: (string-ascii 10),
    confidence-level: uint,
    last-calculated: uint
  }
)

(define-map user-analytics-access
  { user: principal }
  {
    access-level: (string-ascii 20),
    queries-used: uint,
    last-query: uint,
    subscription-expires: uint
  }
)

(define-public (set-data-access-fee (fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= fee u1000000) err-invalid-parameters)
    (var-set data-access-fee fee)
    (ok true)
  )
)

(define-public (set-max-historical-entries (max-entries uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= max-entries u100) err-invalid-parameters)
    (var-set max-historical-entries max-entries)
    (ok true)
  )
)

(define-public (submit-historical-weather-data
  (location-id (string-ascii 64))
  (timestamp uint)
  (temperature int)
  (rainfall uint)
  (wind-speed uint)
  (humidity uint))
  (let
    (
      (current-entries (var-get total-weather-entries))
      (max-entries (var-get max-historical-entries))
    )
    (asserts! (is-eq tx-sender (var-get oracle-address)) err-oracle-only)
    (asserts! (< timestamp stacks-block-height) err-invalid-timeframe)
    (asserts! (< current-entries max-entries) err-insufficient-funds)
    (asserts! (is-none (map-get? weather-history { location-id: location-id, timestamp: timestamp })) err-data-point-exists)
    
    (map-set weather-history
      { location-id: location-id, timestamp: timestamp }
      {
        temperature: temperature,
        rainfall: rainfall,
        wind-speed: wind-speed,
        humidity: humidity,
        recorded-by: tx-sender,
        verified: true
      }
    )
    
    (var-set total-weather-entries (+ current-entries u1))
    (unwrap! (update-location-statistics location-id temperature rainfall wind-speed humidity) err-invalid-parameters)
    (ok true)
  )
)

(define-public (purchase-analytics-access (access-level (string-ascii 20)) (duration uint))
  (let
    (
      (access-fee (var-get data-access-fee))
      (total-fee (if (is-eq access-level "premium") (* access-fee u3) access-fee))
      (expiry-block (+ stacks-block-height duration))
    )
    (asserts! (or (is-eq access-level "basic") (is-eq access-level "premium")) err-invalid-parameters)
    (asserts! (and (>= duration u1440) (<= duration u525600)) err-invalid-timeframe)
    
    (try! (stx-transfer? total-fee tx-sender (as-contract tx-sender)))
    
    (map-set user-analytics-access
      { user: tx-sender }
      {
        access-level: access-level,
        queries-used: u0,
        last-query: u0,
        subscription-expires: expiry-block
      }
    )
    (ok true)
  )
)

(define-public (calculate-risk-score (location-id (string-ascii 64)) (metric (string-ascii 32)))
  (let
    (
      (user-access (map-get? user-analytics-access { user: tx-sender }))
      (location-stats-data (map-get? location-stats { location-id: location-id }))
    )
    (asserts! (is-some user-access) err-not-authorized)
    (asserts! (> (get subscription-expires (unwrap! user-access err-not-authorized)) stacks-block-height) err-not-authorized)
    (asserts! (is-some location-stats-data) err-invalid-location)
    (asserts! (is-valid-weather-condition metric) err-invalid-metric)
    
    (let
      (
        (stats (unwrap! location-stats-data err-invalid-location))
        (risk-score (calculate-risk-value metric stats))
        (volatility (calculate-volatility-index metric stats))
        (trend (determine-trend-direction metric stats))
        (confidence (calculate-confidence-level stats))
      )
      (map-set risk-analysis
        { location-id: location-id, metric: metric }
        {
          risk-score: risk-score,
          volatility-index: volatility,
          trend-direction: trend,
          confidence-level: confidence,
          last-calculated: stacks-block-height
        }
      )
      
      (map-set user-analytics-access
        { user: tx-sender }
        (merge (unwrap! user-access err-not-authorized) { queries-used: (+ (get queries-used (unwrap! user-access err-not-authorized)) u1) })
      )
      
      (ok { risk-score: risk-score, volatility-index: volatility, trend-direction: trend, confidence-level: confidence })
    )
  )
)

(define-private (update-location-statistics
  (location-id (string-ascii 64))
  (temperature int)
  (rainfall uint)
  (wind-speed uint)
  (humidity uint))
  (let
    (
      (existing-stats (map-get? location-stats { location-id: location-id }))
    )
    (match existing-stats
      stats
      (let
        (
          (total-entries (+ (get total-entries stats) u1))
          (new-avg-temp (/ (+ (* (get avg-temperature stats) (to-int (get total-entries stats))) temperature) (to-int total-entries)))
          (new-avg-rainfall (/ (+ (* (get avg-rainfall stats) (get total-entries stats)) rainfall) total-entries))
          (new-avg-wind (/ (+ (* (get avg-wind-speed stats) (get total-entries stats)) wind-speed) total-entries))
          (new-avg-humidity (/ (+ (* (get avg-humidity stats) (get total-entries stats)) humidity) total-entries))
        )
        (map-set location-stats
          { location-id: location-id }
          {
            total-entries: total-entries,
            avg-temperature: new-avg-temp,
            avg-rainfall: new-avg-rainfall,
            avg-wind-speed: new-avg-wind,
            avg-humidity: new-avg-humidity,
            max-temperature: (if (> temperature (get max-temperature stats)) temperature (get max-temperature stats)),
            min-temperature: (if (< temperature (get min-temperature stats)) temperature (get min-temperature stats)),
            max-rainfall: (if (> rainfall (get max-rainfall stats)) rainfall (get max-rainfall stats)),
            max-wind-speed: (if (> wind-speed (get max-wind-speed stats)) wind-speed (get max-wind-speed stats)),
            max-humidity: (if (> humidity (get max-humidity stats)) humidity (get max-humidity stats)),
            last-updated: stacks-block-height
          }
        )
      )
      (map-set location-stats
        { location-id: location-id }
        {
          total-entries: u1,
          avg-temperature: temperature,
          avg-rainfall: rainfall,
          avg-wind-speed: wind-speed,
          avg-humidity: humidity,
          max-temperature: temperature,
          min-temperature: temperature,
          max-rainfall: rainfall,
          max-wind-speed: wind-speed,
          max-humidity: humidity,
          last-updated: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-private (calculate-risk-value (metric (string-ascii 32)) (stats {
  total-entries: uint,
  avg-temperature: int,
  avg-rainfall: uint,
  avg-wind-speed: uint,
  avg-humidity: uint,
  max-temperature: int,
  min-temperature: int,
  max-rainfall: uint,
  max-wind-speed: uint,
  max-humidity: uint,
  last-updated: uint
}))
  (if (is-eq metric "temperature")
    (+ (/ (to-uint (- (get max-temperature stats) (get min-temperature stats))) u10) u10)
    (if (is-eq metric "rainfall")
      (+ (/ (get max-rainfall stats) u100) u5)
      (if (is-eq metric "wind-speed")
        (+ (/ (get max-wind-speed stats) u10) u5)
        (if (is-eq metric "humidity")
          (+ (/ (get max-humidity stats) u10) u5)
          u50))))
)

(define-private (calculate-volatility-index (metric (string-ascii 32)) (stats {
  total-entries: uint,
  avg-temperature: int,
  avg-rainfall: uint,
  avg-wind-speed: uint,
  avg-humidity: uint,
  max-temperature: int,
  min-temperature: int,
  max-rainfall: uint,
  max-wind-speed: uint,
  max-humidity: uint,
  last-updated: uint
}))
  (if (is-eq metric "temperature")
    (/ (to-uint (- (get max-temperature stats) (get min-temperature stats))) u5)
    (if (is-eq metric "rainfall")
      (/ (get max-rainfall stats) u50)
      (if (is-eq metric "wind-speed")
        (/ (get max-wind-speed stats) u5)
        (if (is-eq metric "humidity")
          (/ (get max-humidity stats) u5)
          u20))))
)

(define-private (determine-trend-direction (metric (string-ascii 32)) (stats {
  total-entries: uint,
  avg-temperature: int,
  avg-rainfall: uint,
  avg-wind-speed: uint,
  avg-humidity: uint,
  max-temperature: int,
  min-temperature: int,
  max-rainfall: uint,
  max-wind-speed: uint,
  max-humidity: uint,
  last-updated: uint
}))
  (if (is-eq metric "temperature")
    (if (> (get avg-temperature stats) (/ (+ (get max-temperature stats) (get min-temperature stats)) 2))
      "increasing"
      "decreasing")
    (if (is-eq metric "rainfall")
      (if (> (get avg-rainfall stats) (/ (get max-rainfall stats) u2))
        "increasing"
        "decreasing")
      (if (is-eq metric "wind-speed")
        (if (> (get avg-wind-speed stats) (/ (get max-wind-speed stats) u2))
          "increasing"
          "decreasing")
        (if (is-eq metric "humidity")
          (if (> (get avg-humidity stats) (/ (get max-humidity stats) u2))
            "increasing"
            "decreasing")
          "stable"))))
)

(define-private (calculate-confidence-level (stats {
  total-entries: uint,
  avg-temperature: int,
  avg-rainfall: uint,
  avg-wind-speed: uint,
  avg-humidity: uint,
  max-temperature: int,
  min-temperature: int,
  max-rainfall: uint,
  max-wind-speed: uint,
  max-humidity: uint,
  last-updated: uint
}))
  (if (>= (get total-entries stats) u100)
    u95
    (if (>= (get total-entries stats) u50)
      u80
      (if (>= (get total-entries stats) u20)
        u65
        u40)))
)

(define-read-only (get-weather-history (location-id (string-ascii 64)) (timestamp uint))
  (map-get? weather-history { location-id: location-id, timestamp: timestamp })
)

(define-read-only (get-location-statistics (location-id (string-ascii 64)))
  (map-get? location-stats { location-id: location-id })
)

(define-read-only (get-risk-analysis (location-id (string-ascii 64)) (metric (string-ascii 32)))
  (map-get? risk-analysis { location-id: location-id, metric: metric })
)

(define-read-only (get-user-analytics-access (user principal))
  (map-get? user-analytics-access { user: user })
)

(define-read-only (get-analytics-system-info)
  {
    max-historical-entries: (var-get max-historical-entries),
    data-access-fee: (var-get data-access-fee),
    total-weather-entries: (var-get total-weather-entries),
    system-active: true
  }
)