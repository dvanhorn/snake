#lang typed/racket

;;   ___|              |         
;; \___ \  __ \   _` | |  /  _ \ 
;;       | |   | (   |   <   __/ 
;; _____/ _|  _|\__,_|_|\_\\___| 

;; Copyright (c) 2008, 2010 David Van Horn
;; Licensed under the Academic Free License version 3.0

;; (at dvanhorn (dot ccs neu edu))

;; Caveat emptor: this program is intentionally written in such a way
;; that if you plagiarize it, it will be obvious to all who know which
;; way the wind blows.

(provide (all-defined-out))
(require 2htdp/universe)
(require (planet dvanhorn/typed:1:5/util)
         (planet dvanhorn/typed:1:5/list)
         (planet dvanhorn/typed:1:5/2htdp/image)
         (planet dvanhorn/typed:1:5/lang/posn))         


;;-------------------------------------------------------------------
;; Typed Racket machinations

;; Assign singleton type
(define-syntax-rule (s x) (ann x x))

(define-type-alias Scene Image)
(define-type-alias KeyEvent String)

;;-------------------------------------------------------------------
;; Data Definitions

;; A Seg is a (make-Posn Integer Integer).
(define-type-alias Seg Posn)

;; A Food is a (make-food Integer Integer Integer).
;; Interpretation: the food is at (x,y), and will "rot"
;; in t ticks.
(define-struct: food ([x : Nat] [y : Nat] [t : Nat]))
(define-type-alias Food food)

;; A Dir is one of "up" "down" "left" "right".
(define-type-alias Dir (U "up" "down" "left" "right"))
(define-predicate direction? Dir)

;; A Snake is a (make-snake Dir (cons Seg [Listof Seg])).
(define-struct: snake ([dir  : Dir] 
                       [segs : (Pair Seg (Listof Seg))]))
(define-type-alias Snake snake)
;; Interpretation: the first segment is designated as the
;; head of the snake, and every snake has a head (hence the
;; non-empty list).

;; A World is a (make-world Snake Food).
(define-struct: world ([snake : Snake] 
                       [food : (Listof Food)]
                       [level : Level]
                       [blocks : (Listof Block)]))
(define-type-alias World world)

;; A Level is a Integer.
(define-type-alias Level Nat)

;; A Block is a Posn.
(define-type-alias Block Posn)

;;-------------------------------------------------------------------
;; Constants

(define SEG-SIZE 10)
(define WIDTH  (* SEG-SIZE 30))
(define HEIGHT (* SEG-SIZE 30))
(define FOOD-LIFE 50)

(define food0  
  (list (make-food (* 4 SEG-SIZE) (* 4 SEG-SIZE) FOOD-LIFE)))

(define snake0 
  (make-snake "right"
              (list (make-posn SEG-SIZE SEG-SIZE))))

(define world0
  (make-world snake0 empty 0 empty))


;;-------------------------------------------------------------------
;; Posn helpers

;; Determine if two posns are equal.
(: posn=? (Posn Posn -> Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;; Move the position by dx, dy.
(: posn-move (Posn Integer Integer -> Posn))
(define (posn-move p dx dy)
  (make-posn (+ (posn-x p) dx)
             (+ (posn-y p) dy)))


;;-------------------------------------------------------------------
;; Collision detection

;; The head of the snake is the first element in the list of segments.

;; Each segment of a snake is located with:
;;  - x in (0,WIDTH),
;;  - y in (0,HEIGHT).
;; And is SEG-SIZE aligned (x and y are multiples of SEG-SIZE).

;; Access the head position of the snake.
(: snake-head (Snake -> Seg))
(define (snake-head snake)
  (first (snake-segs snake)))

;; Compute the next head position of the snake.
(: next-head (Snake -> Seg))
(define (next-head snake)
  (move-seg (first (snake-segs snake)) 
            (snake-dir snake)))

;; Move the given segment in the given direction.
(: move-seg (Seg Dir -> Seg))
(define (move-seg seg dir)
  (cond [(string=? dir (s "up"))    (posn-move seg 0 (- SEG-SIZE))]
        [(string=? dir (s "down"))  (posn-move seg 0 SEG-SIZE)]
        [(string=? dir (s "left"))  (posn-move seg (- SEG-SIZE) 0)]
        [(string=? dir (s "right")) (posn-move seg SEG-SIZE 0)]))

;; Determine if the snake is eating the food.
(: eating? (Snake Food -> Boolean))
(define (eating? snake food)
  (posn=? (snake-head snake) 
          (make-posn (food-x food) (food-y food))))

;; Determine if the snake is colliding with itself.
(: self-colliding? (Snake -> Boolean))
(define (self-colliding? snake)
  (ormap (lambda: ([s : Seg]) (posn=? (next-head snake) s)) 
         (rest (snake-segs snake))))

;; Determine if the snake is colliding with any of the walls.
(: wall-colliding? (Snake -> Boolean))
(define (wall-colliding? snake)
  (let ((x (posn-x (snake-head snake)))
        (y (posn-y (snake-head snake))))
    (or (= 0 x) (= x WIDTH)
        (= 0 y) (= y HEIGHT))))

;; Determine if the snake is colliding with the block.
(: block-colliding? (Snake Block -> Boolean))
(define (block-colliding? s b)
  (posn=? (next-head s) b))


;;-------------------------------------------------------------------
;; Snake movement

;; Slither the snake forward one segment.
(: snake-slither (Snake -> Snake))
(define (snake-slither snake)
  (make-snake (snake-dir snake)
              (ann (cons (next-head snake)
                         (all-but-last (snake-segs snake)))
                   (Pair Seg (Listof Seg)))))

;; Grow the snake one segment.
(: snake-grow (Snake -> Snake))
(define (snake-grow snake)
  (make-snake (snake-dir snake)
              (ann (cons (next-head snake)
                         (snake-segs snake))
                   (Pair Seg (Listof Seg)))))

;; Change the direction of the snake.
(: snake-change-direction (Snake Dir -> Snake))
(define (snake-change-direction snake dir)
  (make-snake dir (snake-segs snake)))


;;-------------------------------------------------------------------
;; Visualization

;; Visual constants.
(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define FOOD-IMG 
  (overlay (circle SEG-SIZE 'outline 'black)
           (circle SEG-SIZE 'solid 'green)))
(define SEG-IMG  
  (overlay (circle SEG-SIZE 'outline 'black)
           (circle SEG-SIZE 'solid 'red)))
(define BLOCK-IMG 
  (overlay (circle SEG-SIZE 'outline 'black)
           (circle SEG-SIZE 'solid 'gray)))

(: level+scene (Level Scene -> Scene))
(define (level+scene level scene)
  (overlay/align 'left 'top
                 (text (number->string level) 30 'gray) 
                 scene))

(: snake+scene (Snake Scene -> Scene))
(define (snake+scene snake scene)
  (head+scene (first (snake-segs snake))
              (snake-dir snake)
              (foldr seg+scene scene (rest (snake-segs snake)))))

(: dir->string (Dir -> String))
(define (dir->string dir)
  (cond [(string=? dir (s "up")) "↑"]
        [(string=? dir (s "down")) "↓"]
        [(string=? dir (s "left")) "←"]
        [(string=? dir (s "right")) "→"]))

(: head+scene (Seg Dir Scene -> Scene))
(define (head+scene s d scene)
  (place-image 
   (let ((t (text (dir->string d) 16 'black)))
     (overlay t SEG-IMG))
   (posn-x s)
   (posn-y s)
   scene))

(: seg+scene (Seg Scene -> Scene))
(define (seg+scene seg scene)
  (img+scene seg SEG-IMG scene))

(: food+scene (Food Scene -> Scene))
(define (food+scene f scene)
  (place-image FOOD-IMG (food-x f) (food-y f) scene))

(: blocks+scene ([Listof Block] Scene -> Scene))
(define (blocks+scene bs scene)
  (foldr (lambda: ([b : Block] [s : Scene])
           (img+scene b BLOCK-IMG s))
         scene
         bs))

(: img+scene (Posn Image Scene -> Scene))
(define (img+scene posn img scene)
  (place-image img (posn-x posn) (posn-y posn) scene))


;;-------------------------------------------------------------------
;; World

;; We allow the food to be placed on the snake, but could easily 
;; refine that here by first checking that we didn't pick a location
;; occuppied by the snake.
(: eat-food (Snake [Listof Food] -> [Listof Food]))
(define (eat-food s lof)
  (filter (lambda: ([f : Food]) (not (eating? s f)))
          lof))

(: maybe-new-food ([Listof Food] -> [Listof Food]))
(define (maybe-new-food lof)
  (cond [(zero? (random FOOD-LIFE)) 
         (cons (new-food) lof)]
        [else lof]))

;; IMPROVE ME: Abstract.

(: new-food (-> Food))
(define (new-food)
  (make-food 
   (* SEG-SIZE (add1 (random (sub1 (quotient WIDTH SEG-SIZE)))))
   (* SEG-SIZE (add1 (random (sub1 (quotient HEIGHT SEG-SIZE)))))
   ;; Since (exact-floor positive?) : Nat
   ;;       (random Nat) : Nat
   ;;       (+ Nat Nat) : Nat.
   (cast exact-nonnegative-integer?
         (+ (exact-floor (* 1/2 FOOD-LIFE)) (random FOOD-LIFE)))))

(: new-block (-> Block))
(define (new-block)
  (make-posn
   (* SEG-SIZE (add1 (random (sub1 (quotient WIDTH SEG-SIZE)))))
   (* SEG-SIZE (add1 (random (sub1 (quotient HEIGHT SEG-SIZE)))))))

;; Grow the snake and create new food.
(: eat-and-grow (World -> World))
(define (eat-and-grow w)
  (make-world (snake-grow (world-snake w))
              (cons (new-food) (eat-food (world-snake w) (world-food w)))
              (world-level w)
              (world-blocks w)))

;; Assumes food is not rotten.
(: food-decay (Food -> Food))
(define (food-decay f)
  (make-food (food-x f)
             (food-y f)
             ;; By assumption.
             (cast exact-nonnegative-integer? (sub1 (food-t f)))))

(: food-rotten? (Food -> Boolean))
(define (food-rotten? f)
  (zero? (food-t f)))

;;-------------------------------------------------------------------
;; Levels

(: next-level (World -> World))
(define (next-level w)
  (make-world (truncate-snake (world-snake w))
              (world-food w)
              (add1 (world-level w))
              (cons (new-block) (world-blocks w))))

(: truncate-snake (Snake -> Snake))
(define (truncate-snake s)
  (make-snake (snake-dir s)
              (list (first (snake-segs s)))))

(: level-complete? (World -> Boolean))
(define (level-complete? w)
  (> (length (snake-segs (world-snake w)))
     (exact-floor (expt 2 (world-level w)))))
              
;;-------------------------------------------------------------------
;; Animation and Interaction

(: food-not-rotten? (Food -> Boolean))
(define (food-not-rotten? f)
  (not (food-rotten? f)))

(: show (World -> Image))
(define (show w)
  (snake+scene (world-snake w)
               (blocks+scene
                (world-blocks w)
                (foldr food+scene 
                       (level+scene (world-level w) MT-SCENE)
                       (world-food w)))))

(: tock (World -> World))
(define (tock w)
  (cond [(ormap (lambda: ([f : Food]) (eating? (world-snake w) f)) (world-food w))
         (eat-and-grow w)]
        [(level-complete? w)
         (next-level w)]
        [else 
         (make-world 
          (snake-slither (world-snake w))
          (maybe-new-food
           (filter food-not-rotten? (map food-decay (world-food w))))
          (world-level w)
          (world-blocks w))]))

(: peck (World KeyEvent -> World))
(define (peck w ke)
  (cond [(direction? ke) 
         (make-world (snake-change-direction (world-snake w) ke)
                     (world-food w)
                     (world-level w)
                     (world-blocks w))]
        [else w]))
  
(: over? (World -> Boolean))
(define (over? w)
  (or (self-colliding? (world-snake w))
      (wall-colliding? (world-snake w))
      (ormap (lambda: ([b : Seg]) (block-colliding? (world-snake w) b))
             (world-blocks w))))
