#|
Prime Numbers Activity
Miguel Angel Medina Ruperto
A01023656
Diana Melo
Emilio 
26/05/2021|#

#lang racket

(require racket/trace)


; function to check if a number is prime, returns a boolean.
(define (is-prime-number n)
  ; check if "n" is exactly divisable by 2 and the following values until the ceil round up of the square root of n.
  (define stop (sqrt n))
  (let loop
    ([i 2])
    (cond
      [(< n 2)
       #f]
      [(> i stop)
       #t]
      [(zero? (remainder n i))
       #f]
      [else (loop (add1 i))])))
        
; function to sum prime numbers until a limit is reached. (must try to accelerate this process)
(define (sum-primes limit)
  ; loop from 0 to limit
  (let loop
    ([i 0] [sum 0])
    (cond
      ; if i reaches limit, return the accumulated sum.
      [(> i limit)
       sum]
      ; check each number for primality and add to total if it's prime.
      [(is-prime-number i)
       ;(+ sum i)
       (loop (add1 i) (+ sum i))]
      [else (loop (add1 i) sum)])))

; another recurse idea to check for a prime number:
#|
1-receive n = 2 as the lowest prime number is 2
2-add 2 to a list
3-inLoop: add 1 to n
4-inLoop: check if it is divided exactly by any number in the list
5-inLoop: if it is not, add the number to the list / if it is, do nothing
6 reach 5million, end.|#


; ======================== Parallel ==========================================


; The function is-prime-number does not require to be altered for parallel, so it is used for both processes to avoid unnecessary extra lines of code.

; make futures given a range (start - end)
(define (make-future start limit)
  ; return a new future that loops 'limit' times.
  (future (lambda()
             ; loop from 0 to limit
            (let loop
              ([i start] [sum 0])
              (cond
                ; if i reaches limit, return the accumulated sum.
                [(> i limit)
                 sum]
                ; check each number for primality and add to total if it's prime.
                [(is-prime-number i)
                 (loop (add1 i) (+ sum i))]
                [else (loop (add1 i) sum)])))))
            

; Function to divide into segments a number "n" using the number of cores of the computer.
(define (use-all-cores cores n)
  (define how-many-futures (floor(/ n cores)))
  (let loop
    ; i acts as a reverse counter.
    ([i cores] [starts empty] [limits empty] [begin 1] [end how-many-futures] [range how-many-futures])
    (cond
      [(zero? i)
       (assign-range starts limits)]
      [(< i how-many-futures)
       (loop (sub1 i) (append starts (list begin)) (append limits (list end)) (+ begin range) (+ end range) range)]
      [else (loop (sub1 i) (append starts (list begin)) (append limits (list end)) (+ begin (sub1 range)) (+ end range) range)]))) ; if i = futures, it is added range -1, range is added normally every other lap.


; Function to iterate over the limit and start lists and give values to the futures.
(define (assign-range lst-start lst-limit)
  (let loop
    ([lstS lst-start] [lstL lst-limit] [f-lst empty])
    (cond
      [(not(empty? lstS))
       (loop (cdr lstS) (cdr lstL) (append f-lst (list (make-future (car lstS) (car lstL)))))]
      [else f-lst])))

        
; Main function. 
(define (main how-many-cores num)
  (define futures (use-all-cores how-many-cores num))
  ; Launch all futures in the list.
  (map touch futures))


; normal: 121013308 cpu time: 173 | real time: 186 | gc time: 8
; parallel: 121013308 cpu time: 349 | real time: 156 | gc time: 12
; Conclusion: using futures to take advantage of the computer's total number of cores, the process was accelerated.

;(trace assign-range)