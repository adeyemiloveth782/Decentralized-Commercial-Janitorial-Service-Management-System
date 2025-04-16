;; Facility Registration Contract
;; Records details of serviced properties

(define-data-var last-facility-id uint u0)

(define-map facilities
  { facility-id: uint }
  {
    name: (string-utf8 100),
    address: (string-utf8 200),
    size-sqft: uint,
    contact-name: (string-utf8 100),
    contact-info: (string-utf8 100),
    owner: principal,
    active: bool
  }
)

;; Register a new facility
(define-public (register-facility
    (name (string-utf8 100))
    (address (string-utf8 200))
    (size-sqft uint)
    (contact-name (string-utf8 100))
    (contact-info (string-utf8 100)))
  (let
    ((new-id (+ (var-get last-facility-id) u1)))
    (begin
      (asserts! (> (len name) u0) (err u1)) ;; Name cannot be empty
      (asserts! (> (len address) u0) (err u2)) ;; Address cannot be empty
      (asserts! (> size-sqft u0) (err u3)) ;; Size must be positive

      (map-set facilities
        { facility-id: new-id }
        {
          name: name,
          address: address,
          size-sqft: size-sqft,
          contact-name: contact-name,
          contact-info: contact-info,
          owner: tx-sender,
          active: true
        }
      )

      (var-set last-facility-id new-id)
      (ok new-id)
    )
  )
)

;; Get facility details
(define-read-only (get-facility (facility-id uint))
  (map-get? facilities { facility-id: facility-id })
)

;; Update facility details
(define-public (update-facility
    (facility-id uint)
    (name (string-utf8 100))
    (address (string-utf8 200))
    (size-sqft uint)
    (contact-name (string-utf8 100))
    (contact-info (string-utf8 100)))
  (let
    ((facility (unwrap! (map-get? facilities { facility-id: facility-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get owner facility)) (err u403)) ;; Only owner can update
      (asserts! (> (len name) u0) (err u1)) ;; Name cannot be empty
      (asserts! (> (len address) u0) (err u2)) ;; Address cannot be empty
      (asserts! (> size-sqft u0) (err u3)) ;; Size must be positive

      (map-set facilities
        { facility-id: facility-id }
        {
          name: name,
          address: address,
          size-sqft: size-sqft,
          contact-name: contact-name,
          contact-info: contact-info,
          owner: (get owner facility),
          active: (get active facility)
        }
      )
      (ok true)
    )
  )
)

;; Deactivate a facility
(define-public (deactivate-facility (facility-id uint))
  (let
    ((facility (unwrap! (map-get? facilities { facility-id: facility-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get owner facility)) (err u403)) ;; Only owner can deactivate

      (map-set facilities
        { facility-id: facility-id }
        (merge facility { active: false })
      )
      (ok true)
    )
  )
)

;; Reactivate a facility
(define-public (reactivate-facility (facility-id uint))
  (let
    ((facility (unwrap! (map-get? facilities { facility-id: facility-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get owner facility)) (err u403)) ;; Only owner can reactivate

      (map-set facilities
        { facility-id: facility-id }
        (merge facility { active: true })
      )
      (ok true)
    )
  )
)
