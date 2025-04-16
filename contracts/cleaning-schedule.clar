;; Cleaning Schedule Contract
;; Defines required tasks and frequency

(define-data-var last-schedule-id uint u0)

(define-map cleaning-schedules
  { schedule-id: uint }
  {
    facility-id: uint,
    task-name: (string-utf8 100),
    description: (string-utf8 200),
    frequency: (string-utf8 50), ;; daily, weekly, monthly, etc.
    day-of-week: (optional uint), ;; 1-7 for weekly tasks
    day-of-month: (optional uint), ;; 1-31 for monthly tasks
    created-by: principal,
    active: bool
  }
)

;; Create a new cleaning schedule
(define-public (create-schedule
    (facility-id uint)
    (task-name (string-utf8 100))
    (description (string-utf8 200))
    (frequency (string-utf8 50))
    (day-of-week (optional uint))
    (day-of-month (optional uint)))
  (let
    ((new-id (+ (var-get last-schedule-id) u1)))
    (begin
      (asserts! (> (len task-name) u0) (err u1)) ;; Task name cannot be empty
      (asserts! (> (len frequency) u0) (err u2)) ;; Frequency cannot be empty

      ;; Validate day-of-week if provided
      (if (is-some day-of-week)
        (asserts! (and (>= (unwrap! day-of-week (err u3)) u1)
                      (<= (unwrap! day-of-week (err u3)) u7))
                  (err u4)) ;; Day of week must be 1-7
        true)

      ;; Validate day-of-month if provided
      (if (is-some day-of-month)
        (asserts! (and (>= (unwrap! day-of-month (err u5)) u1)
                      (<= (unwrap! day-of-month (err u5)) u31))
                  (err u6)) ;; Day of month must be 1-31
        true)

      (map-set cleaning-schedules
        { schedule-id: new-id }
        {
          facility-id: facility-id,
          task-name: task-name,
          description: description,
          frequency: frequency,
          day-of-week: day-of-week,
          day-of-month: day-of-month,
          created-by: tx-sender,
          active: true
        }
      )

      (var-set last-schedule-id new-id)
      (ok new-id)
    )
  )
)

;; Get schedule details
(define-read-only (get-schedule (schedule-id uint))
  (map-get? cleaning-schedules { schedule-id: schedule-id })
)

;; Get all schedules for a facility
(define-read-only (get-facility-schedules (facility-id uint))
  (filter get-schedule-by-facility-id
    (map unwrap-schedule-id (get-schedule-ids))
  )

  (define-private (get-schedule-ids)
    (map-keys cleaning-schedules)
  )

  (define-private (unwrap-schedule-id (entry {schedule-id: uint}))
    (get schedule-id entry)
  )

  (define-private (get-schedule-by-facility-id (schedule-id uint))
    (match (map-get? cleaning-schedules {schedule-id: schedule-id})
      schedule (is-eq facility-id (get facility-id schedule))
      false
    )
  )
)

;; Update a cleaning schedule
(define-public (update-schedule
    (schedule-id uint)
    (task-name (string-utf8 100))
    (description (string-utf8 200))
    (frequency (string-utf8 50))
    (day-of-week (optional uint))
    (day-of-month (optional uint)))
  (let
    ((schedule (unwrap! (map-get? cleaning-schedules { schedule-id: schedule-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get created-by schedule)) (err u403)) ;; Only creator can update
      (asserts! (> (len task-name) u0) (err u1)) ;; Task name cannot be empty
      (asserts! (> (len frequency) u0) (err u2)) ;; Frequency cannot be empty

      ;; Validate day-of-week if provided
      (if (is-some day-of-week)
        (asserts! (and (>= (unwrap! day-of-week (err u3)) u1)
                      (<= (unwrap! day-of-week (err u3)) u7))
                  (err u4)) ;; Day of week must be 1-7
        true)

      ;; Validate day-of-month if provided
      (if (is-some day-of-month)
        (asserts! (and (>= (unwrap! day-of-month (err u5)) u1)
                      (<= (unwrap! day-of-month (err u5)) u31))
                  (err u6)) ;; Day of month must be 1-31
        true)

      (map-set cleaning-schedules
        { schedule-id: schedule-id }
        {
          facility-id: (get facility-id schedule),
          task-name: task-name,
          description: description,
          frequency: frequency,
          day-of-week: day-of-week,
          day-of-month: day-of-month,
          created-by: (get created-by schedule),
          active: (get active schedule)
        }
      )
      (ok true)
    )
  )
)

;; Deactivate a schedule
(define-public (deactivate-schedule (schedule-id uint))
  (let
    ((schedule (unwrap! (map-get? cleaning-schedules { schedule-id: schedule-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get created-by schedule)) (err u403)) ;; Only creator can deactivate

      (map-set cleaning-schedules
        { schedule-id: schedule-id }
        (merge schedule { active: false })
      )
      (ok true)
    )
  )
)

;; Reactivate a schedule
(define-public (reactivate-schedule (schedule-id uint))
  (let
    ((schedule (unwrap! (map-get? cleaning-schedules { schedule-id: schedule-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (get created-by schedule)) (err u403)) ;; Only creator can reactivate

      (map-set cleaning-schedules
        { schedule-id: schedule-id }
        (merge schedule { active: true })
      )
      (ok true)
    )
  )
)
