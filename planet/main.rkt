#lang racket
(require "snake.ss")
(require 2htdp/universe)

(big-bang world0
  (on-draw show)  
  (on-tick tock 1/10)  
  (on-key peck)
  (stop-when over?))