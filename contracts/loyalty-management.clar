;; Banking Customer Retention Loyalty Management Contract
;; Handles churn prediction, engagement strategies, win-back campaigns, and lifetime value optimization

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-CUSTOMER (err u101))
(define-constant ERR-INVALID-CAMPAIGN (err u102))

(define-data-var customer-counter uint u0)
(define-data-var campaign-counter uint u0)
(define-data-var total-ltv uint u0)

(define-map customers
  { customer-id: uint }
  {
    account: principal,
    balance: uint,
    churn-risk: uint,
    ltv: uint,
    last-activity: uint
  }
)

(define-map engagement-campaigns
  { campaign-id: uint }
  {
    name: (string-ascii 64),
    status: (string-ascii 16),
    target-segment: (string-ascii 32),
    created-at: uint
  }
)

(define-map customer-engagements
  { engagement-id: uint }
  {
    customer-id: uint,
    campaign-id: uint,
    engagement-score: uint
  }
)

(define-map winback-campaigns
  { winback-id: uint }
  {
    customer-id: uint,
    incentive-amount: uint,
    status: (string-ascii 16)
  }
)

(define-public (register-customer (initial-balance uint))
  (let
    (
      (new-id (+ (var-get customer-counter) u1))
    )
    (map-set customers
      { customer-id: new-id }
      {
        account: tx-sender,
        balance: initial-balance,
        churn-risk: u20,
        ltv: (calculate-ltv initial-balance),
        last-activity: burn-block-height
      }
    )
    (var-set customer-counter new-id)
    (ok new-id)
  )
)

(define-public (assess-churn-risk (customer-id uint) (activity-days uint))
  (let
    (
      (customer (map-get? customers { customer-id: customer-id }))
    )
    (match customer
      customer-data
      (begin
        (let
          (
            (risk (if (> activity-days u90) u80 (if (> activity-days u30) u50 u20)))
          )
          (map-set customers
            { customer-id: customer-id }
            (merge customer-data { churn-risk: risk })
          )
          (ok risk)
        )
      )
      ERR-INVALID-CUSTOMER
    )
  )
)

(define-public (create-engagement-campaign
  (name (string-ascii 64))
  (target-segment (string-ascii 32))
)
  (let
    (
      (new-id (+ (var-get campaign-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set engagement-campaigns
      { campaign-id: new-id }
      {
        name: name,
        status: "active",
        target-segment: target-segment,
        created-at: burn-block-height
      }
    )
    (var-set campaign-counter new-id)
    (ok new-id)
  )
)

(define-public (engage-customer (customer-id uint) (campaign-id uint) (score uint))
  (let
    (
      (engagement-id (+ (var-get campaign-counter) u1))
    )
    (asserts! (and (>= score u0) (<= score u100)) ERR-INVALID-CAMPAIGN)
    (map-set customer-engagements
      { engagement-id: engagement-id }
      {
        customer-id: customer-id,
        campaign-id: campaign-id,
        engagement-score: score
      }
    )
    (ok engagement-id)
  )
)

(define-public (launch-winback-campaign (customer-id uint) (incentive uint))
  (let
    (
      (winback-id (+ (var-get campaign-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set winback-campaigns
      { winback-id: winback-id }
      {
        customer-id: customer-id,
        incentive-amount: incentive,
        status: "active"
      }
    )
    (ok winback-id)
  )
)

(define-public (update-customer-balance (customer-id uint) (new-balance uint))
  (let
    (
      (customer (map-get? customers { customer-id: customer-id }))
    )
    (match customer
      customer-data
      (let
        (
          (new-ltv (calculate-ltv new-balance))
        )
        (map-set customers
          { customer-id: customer-id }
          (merge customer-data
            {
              balance: new-balance,
              ltv: new-ltv,
              last-activity: burn-block-height
            }
          )
        )
        (var-set total-ltv (+ (var-get total-ltv) new-ltv))
        (ok true)
      )
      ERR-INVALID-CUSTOMER
    )
  )
)

(define-private (calculate-ltv (balance uint))
  (/ (* balance u12) u100)
)

(define-read-only (get-customer (customer-id uint))
  (map-get? customers { customer-id: customer-id })
)

(define-read-only (get-campaign (campaign-id uint))
  (map-get? engagement-campaigns { campaign-id: campaign-id })
)

(define-read-only (get-engagement (engagement-id uint))
  (map-get? customer-engagements { engagement-id: engagement-id })
)

(define-read-only (get-total-ltv)
  (var-get total-ltv)
)

