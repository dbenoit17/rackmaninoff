#lang racket
(require "util.rkt"
         "beat.rkt")

(provide (all-defined-out))

(struct note [letter octave duration]
  #:guard 
  (lambda 
    (letter octave duration name)
    (let ([valid-letters 
            (list 'C 'C# 'C#/Db
                  'Db 'D  'D# 
                  'D#/Eb 'Rest
                  'Eb 'E 'F
                  'F# 'F#/Gb 'Gb
                  'G 'G# 'G#/Ab 
                  'Ab 'A 'A#
                  'A#/Bb 'Bb 'B)])
      (if (and (member letter valid-letters)
               (integer? octave)
               (>= octave -1)
               (<= octave 8)
               (procedure? duration))
        (values letter octave duration)
        (error "<#procedure:note> invalid arguments")))))

(define/contract (note-pitch-class n)
  (-> note? exact-positive-integer?)
  (note-letter-to-base-number (note-letter n)))

(define/contract (note->list n)
  (-> note? list?)
  (list
    (note-letter n)
    (note-octave n)
    (note-duration n)))

(define/contract (note-pitch-enharm-eq? note1 note2)
  (-> note? note? boolean?)
  (eq? (note-midi-number note1) (note-midi-number note2)))

(define/contract (note-pitch-class-enharm-eq? note1 note2)
  (-> note? note? boolean?)
  (eq? (note-midi-number (note (note-letter note1) 0 null-beat)) 
       (note-midi-number (note (note-letter note2) 0 null-beat))))

(define/contract (note-duration-eq? note1 note2)
  (-> note? note? boolean?)
  (eq? (note-duration note1) (note-duration note2)))

(define/contract (note-enharm-equal? note1 note2)
  (-> note? note? boolean?)
  (and (note-pitch-enharm-eq? note1 note2)
       (note-duration-eq? note1 note2)))

(define/contract (rest? r)
  (-> any/c boolean?)               
  (and (note? r)
       (equal? (note-letter r) 'Rest)))

(define/contract (make-rest duration)
  (-> beat-value-procedure? rest?)
  (note 'Rest 0 duration))

(define/contract (non-rest-note? n)
  (-> any/c boolean?)               
  (and (note? n)
       (not (rest? n))))

(define (whole-rest)
  (make-rest whole-beat))
(define (half-rest)
  (make-rest half-beat))
(define (quarter-rest)
  (make-rest quarter-beat))
(define (eighth-rest)
  (make-rest eighth-beat))
(define (sixteenth-rest)
  (make-rest sixteenth-beat))
(define (thirtysecond-rest)
  (make-rest thirtysecond-beat))

(define (dotted-whole-rest)
  (make-rest dotted-whole-beat))
(define (dotted-half-rest)
  (make-rest dotted-half-beat))
(define (dotted-quarter-rest)
  (make-rest dotted-quarter-beat))
(define (dotted-eighth-rest)
  (make-rest dotted-eighth-beat))
(define (dotted-sixteenth-rest)
  (make-rest dotted-sixteenth-beat))

(define (double-dotted-whole-rest)
  (make-rest double-dotted-whole-beat))
(define (double-dotted-half-rest)
  (make-rest double-dotted-half-beat))
(define (double-dotted-quarter-rest)
  (make-rest double-dotted-quarter-beat))
(define (double-dotted-eighth-rest)
  (make-rest double-dotted-eighth-beat))

(define (whole-note letter octave)
  (note letter octave whole-beat))
(define (half-note letter octave)
  (note letter octave half-beat))
(define (quarter-note letter octave)
  (note letter octave quarter-beat))
(define (eighth-note letter octave)
  (note letter octave eighth-beat))
(define (sixteenth-note letter octave)
  (note letter octave sixteenth-beat))
(define (thirtysecond-note letter octave)
  (note letter octave thirtysecond-beat))

(define (dotted-whole-note letter octave)
  (note letter octave dotted-whole-beat))
(define (dotted-half-note letter octave)
  (note letter octave dotted-half-beat))
(define (dotted-quarter-note letter octave)
  (note letter octave dotted-quarter-beat))
(define (dotted-eighth-note letter octave)
  (note letter octave dotted-eighth-beat))
(define (dotted-sixteenth-note letter octave)
  (note letter octave dotted-sixteenth-beat))

(define (double-dotted-whole-note letter octave)
  (note letter octave double-dotted-whole-beat))
(define (double-dotted-half-note letter octave)
  (note letter octave double-dotted-half-beat))
(define (double-dotted-quarter-note letter octave)
  (note letter octave double-dotted-quarter-beat))
(define (double-dotted-eighth-note letter octave)
  (note letter octave double-dotted-eighth-beat))

(define/contract (note-frames n tempo)
  (-> note? exact-positive-integer? exact-nonnegative-integer?)
  (inexact->exact (beat-value-frames ((note-duration n) tempo))))



(define/contract (make-note-from-midi-num num duration)
  (-> (lambda (n)
        (and
          (exact-positive-integer? n)
          (< n 128)))
      beat-value-procedure?
      note?)
  (note (midi-number-letter num)
        (midi-number-octave num)
        duration))

(define/contract (note-midi-number n)
  (-> non-rest-note? exact-nonnegative-integer?)
  (letter-and-octave-to-midi 
    (note-letter n)
    (note-octave n)))

(define/contract (note-freq n)
  (-> note? positive?)
  (if (rest? n) 0               
    (letter-and-octave-to-freq 
      (note-letter n)
      (note-octave n))))

(define/contract (note-interval-up n interval)
  (-> note? symbol? note?)
  (cond ((rest? n) n)
        ((eq? interval 'Unison) n)
        ((eq? interval 'AugmentedUnison)
         (make-note-from-midi-num
           (+ (note-midi-number n) 1)
           (note-duration n)))
        ((eq? interval 'Minor2nd)
         (make-note-from-midi-num
           (+ (note-midi-number n) 1)
           (note-duration n)))
        ((eq? interval 'Major2nd)
         (make-note-from-midi-num
           (+ (note-midi-number n) 2)
           (note-duration n)))
        ((eq? interval 'Augmented2nd)
         (make-note-from-midi-num
           (+ (note-midi-number n) 3)
           (note-duration n)))
        ((eq? interval 'Minor3rd)
         (make-note-from-midi-num
           (+ (note-midi-number n) 3)
           (note-duration n)))
        ((eq? interval 'Major3rd)
         (make-note-from-midi-num
           (+ (note-midi-number n) 4)
           (note-duration n)))
        ((eq? interval 'Perfect4th)
         (make-note-from-midi-num
           (+ (note-midi-number n) 5)
           (note-duration n)))
        ((eq? interval 'Augmented4th)
         (make-note-from-midi-num
           (+ (note-midi-number n) 6)
           (note-duration n)))
        ((eq? interval 'Diminished5th)
         (make-note-from-midi-num
           (+ (note-midi-number n) 6)
           (note-duration n)))
        ((eq? interval 'Perfect5th)
         (make-note-from-midi-num
           (+ (note-midi-number n) 7)
           (note-duration n)))
        ((eq? interval 'Augmented5th)
         (make-note-from-midi-num
           (+ (note-midi-number n) 8)
           (note-duration n)))
        ((eq? interval 'Minor6th)
         (make-note-from-midi-num
           (+ (note-midi-number n) 8)
           (note-duration n)))
        ((eq? interval 'Major6th)
         (make-note-from-midi-num
           (+ (note-midi-number n) 9)
           (note-duration n)))
        ((eq? interval 'Minor7th)
         (make-note-from-midi-num
           (+ (note-midi-number n) 10) 
           (note-duration n)))
        ((eq? interval 'Major7th)
         (make-note-from-midi-num
           (+ (note-midi-number n) 11) 
           (note-duration n)))
        ((eq? interval 'PerfectOctave)
         (make-note-from-midi-num
           (+ (note-midi-number n) 12)
           (note-duration n)))
        (else
          (error "<procedure#note-interval-up> invalid symbol"))))

(define/contract (note-interval-down n interval)
  (-> note? symbol? note?)
  (cond ((rest? n) n)
        ((eq? interval 'Unison) n)
        ((eq? interval 'AugmentedUnison)
         (make-note-from-midi-num
           (- (note-midi-number n) 1)
           (note-duration n)))
        ((eq? interval 'Minor2nd)
         (make-note-from-midi-num
           (- (note-midi-number n) 1)
           (note-duration n)))
        ((eq? interval 'Major2nd)
         (make-note-from-midi-num
           (- (note-midi-number n) 2)
           (note-duration n)))
        ((eq? interval 'Augmented2nd)
         (make-note-from-midi-num
           (- (note-midi-number n) 3)
           (note-duration n)))
        ((eq? interval 'Minor3rd)
         (make-note-from-midi-num
           (- (note-midi-number n) 3)
           (note-duration n)))
        ((eq? interval 'Major3rd)
         (make-note-from-midi-num
           (- (note-midi-number n) 4)
           (note-duration n)))
        ((eq? interval 'Perfect4th)
         (make-note-from-midi-num
           (- (note-midi-number n) 5)
           (note-duration n)))
        ((eq? interval 'Augmented4th)
         (make-note-from-midi-num
           (- (note-midi-number n) 6)
           (note-duration n)))
        ((eq? interval 'Diminished5th)
         (make-note-from-midi-num
           (- (note-midi-number n) 6)
           (note-duration n)))
        ((eq? interval 'Perfect5th)
         (make-note-from-midi-num
           (- (note-midi-number n) 7)
           (note-duration n)))
        ((eq? interval 'Augmented5th)
         (make-note-from-midi-num
           (- (note-midi-number n) 8)
           (note-duration n)))
        ((eq? interval 'Minor6th)
         (make-note-from-midi-num
           (- (note-midi-number n) 8)
           (note-duration n)))
        ((eq? interval 'Major6th)
         (make-note-from-midi-num
           (- (note-midi-number n) 9)
           (note-duration n)))
        ((eq? interval 'Minor7th)
         (make-note-from-midi-num
           (- (note-midi-number n) 10)
           (note-duration n)))
        ((eq? interval 'Major7th)
         (make-note-from-midi-num
           (- (note-midi-number n) 11)
           (note-duration n)))
        ((eq? interval 'PerfectOctave)
         (make-note-from-midi-num
           (- (note-midi-number n) 12)
           (note-duration n)))
        (else
          (error "<procedure#note-interval-down> invalid symbol"))
         ))
