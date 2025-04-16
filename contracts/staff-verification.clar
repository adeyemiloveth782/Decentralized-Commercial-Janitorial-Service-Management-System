;; Staff Verification Contract
;; Validates qualified cleaning personnel

(define-data-var last-staff-id uint u0)

(define-map staff-members
  { staff-id: uint }
  {
    name: (string-utf8 100),
    contact-info: (string-utf8 100),
    certifications: (list 10 (string-utf8 50)),
    background-check: bool,
    verified: bool,
    verification-date: (optional uint),
    employer: principal,
    active: bool
  }
)

;; Register a new staff member
(define-public (register-staff
    (name (string-utf8 100))
    (contact-info (string-utf8 100))
    (certifications (list 10 (string-utf8 50)))
    (background-check bool))
  (let
    ((new-id (+ (var-get last-staff-id) u1)))
    (begin
      (asserts! (> (len name) u0) (err u1)) ;; Name cannot be empty
      (asserts! (> (len contact-info) u0) (err u2)) ;; Contact info cannot be empty

      (map-set staff-members
        { staff-id: new-id }
        {
          name: name,
          contact-info: contact-info,
          certifications: certifications,
          background-check: background-check,
          verified: false,
          verification-date: none,
          employer: tx-sender,
          active: true
        }
      )

      (var-set last-staff-id new-id)
      (ok new-id)
    )
  )
)

;; Get staff details
(define-read-only (get-staff (staff-id uint))
  (map-get? staff-members { staff-id: staff-id })
)

;; Verify a staff member
(define-public (verify-staff (staff-id uint))
  (let
    ((staff (unwrap! (map-get? staff-members { staff-id: staff-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get employer staff)) (err u403)) ;; Only employer can verify

      (map-set staff-members
        { staff-id: staff-id }
        (merge staff {
          verified: true,
          verification-date: (some (unwrap! (get-block-info? time (- block-height u1)) (err u500)))
        })
      )
      (ok true)
    )
  )
)

;; Update staff certifications
(define-public (update-certifications
    (staff-id uint)
    (certifications (list 10 (string-utf8 50))))
  (let
    ((staff (unwrap! (map-get? staff-members { staff-id: staff-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get employer staff)) (err u403)) ;; Only employer can update

      (map-set staff-members
        { staff-id: staff-id }
        (merge staff {
          certifications: certifications,
          verified: false,
          verification-date: none
        })
      )
      (ok true)
    )
  )
)

;; Update background check status
(define-public (update-background-check
    (staff-id uint)
    (background-check bool))
  (let
    ((staff (unwrap! (map-get? staff-members { staff-id: staff-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get employer staff)) (err u403)) ;; Only employer can update

      (map-set staff-members
        { staff-id: staff-id }
        (merge staff {
          background-check: background-check,
          verified: false,
          verification-date: none
        })
      )
      (ok true)
    )
  )
)

;; Deactivate a staff member
(define-public (deactivate-staff (staff-id uint))
  (let
    ((staff (unwrap! (map-get? staff-members { staff-id: staff-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get employer staff)) (err u403)) ;; Only employer can deactivate

      (map-set staff-members
        { staff-id: staff-id }
        (merge staff { active: false })
      )
      (ok true)
    )
  )
)

;; Reactivate a staff member
(define-public (reactivate-staff (staff-id uint))
  (let
    ((staff (unwrap! (map-get? staff-members { staff-id: staff-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get employer staff)) (err u403)) ;; Only employer can reactivate

      (map-set staff-members
        { staff-id: staff-id }
        (merge staff { active: true })
      )
      (ok true)
    )
  )
)
