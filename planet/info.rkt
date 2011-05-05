#lang setup/infotab
(define name "Snake.")
(define categories '(misc))
(define required-core-version "4.2.5")
(define repositories (list "4.x"))
(define primary-file 
  '("main.ss"))
(define blurb
  (list '(div "Snake: the snake game, written in Typed Scheme.")))
(define release-notes 
  (list 
   '(div "Moved more code into typed module. "
         "Ported from htdp/image to 2htdp/image. "
	 "Used dvanhorn/typed.")))
