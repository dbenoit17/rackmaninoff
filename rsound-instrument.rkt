#lang racket

(require rsound
         rsound/single-cycle
         "note.rkt"
         "beat.rkt"
         "harmony.rkt")

(provide (except-out (all-defined-out)
                     conversion-proc-safety-wrapper))


;; Needs test
(struct instrument [name conversion-proc]
  #:guard (lambda (name proc t)
            (if (and (string? name)
                     (procedure? proc))
              (values name (conversion-proc-safety-wrapper proc))
              (error "expected args of type: <#string> <#procedure>"))))


;; Needs test
(define/contract 
  (conversion-proc-safety-wrapper conversion-proc)
  (-> procedure? procedure?)
  (lambda (n tempo #:maybe-signal [maybe-signal #f])
    (let ([result
            (cond 
                  ((rest? n) (silence 
                              (beat-value-frames 
                                ((note-duration n) tempo))))
                  ((harmony? n) 
                   (rs-overlay*
                     (map (lambda (x)
                            (display "this-is-a-harmony\n")
                            (conversion-proc x tempo))
                          (harmony-notes n))))
                  ((rsound? n) n)
                  (else 
                    (conversion-proc n tempo)))]) result)))
;      (cond [(rsound? result) result]
;            [(signal? result)
;             (if (not (maybe-signal)) 
;               (signal->rsound (note-frames n tempo))
;               result)]
;            [else 
;              (error "conversion procedure returned non rsound or signal")]))))

;; Needs test
(define (vgame-synth-instrument spec)
  (instrument 
    (string-append "vgame synth: " (number->string spec))
    (lambda (n tempo)
      (synth-note "vgame" 
                  spec 
                  (note-midi-number n) 
                  (beat-value-frames ((note-duration n) tempo))))))

;; Needs test
(define (main-synth-instrument spec)
  (instrument 
    (string-append "main synth: " (number->string spec))
    (lambda (n tempo)
      (synth-note "main" 
                  spec 
                  (note-midi-number n) 
                  (beat-value-frames ((note-duration n) tempo))))))

;; Needs test
(define (path-synth-instrument spec)
  (instrument 
    (string-append "path synth: " (number->string spec))
    (lambda (n tempo)
      (synth-note "path" 
                  spec 
                  (note-midi-number n) 
                  (beat-value-frames ((note-duration n) tempo))))))

