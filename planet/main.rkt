#lang racket
(require "snake.rkt")
(require 2htdp/universe)

(big-bang world0
  (on-draw show)  
  (on-tick tock 1/10)  
  (on-key peck)
  (stop-when over?))