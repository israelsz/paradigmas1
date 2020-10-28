#lang scheme
(require "StackTDA.rkt")


;Register
;descripción: Funcion que añade a un nuevo usuario al stack.
;dom: stack x string x string
;rec: list
(define (register stack usuario password)
  (addUserPassword stack usuario password)
  )