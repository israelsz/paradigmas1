#lang scheme
(provide (all-defined-out))

;Implementacion del TDA usuarios

;Representacion
;(string X string X(list entero)X entero)
;(list usuario password (list preguntas) reputacion)

;Constructor
;descripción: Permite crear un usuario, junto a su contraseña correspondiente
;dom: string X string
;rec: lista
(define crearUsuario
  (lambda (username password)
    (if (and (string? username) (string? password))
        (list username password (list) 500) ;cada user empieza con 500 de reputacion
        (raise "Solo se permite strings para el username y la password")
        )
    )
  )

;Selectores

;descripción: Funcion que retorna el username de una lista de usuario
;dom: lista
;rec: string
(define userGetUsername
  (lambda (usuario)
    (list-ref usuario 0)
    )
  )

;descripción: Funcion que retorna la password de una lista de usuario
;dom: lista
;rec: string
(define userGetPassword
  (lambda (usuario)
    (list-ref usuario 1)
    )
  )

;descripción: Funcion que retorna la lista con preguntas hechas por un usuario de una lista de usuario
;dom: lista
;rec: lista
(define userGetQuestions
  (lambda (usuario)
    (list-ref usuario 2)
    )
  )

;descripción: Funcion que retorna la reputacion de una lista de usuario
;dom: lista
;rec: entero
(define userGetReputation
  (lambda (usuario)
    (list-ref usuario 3)
    )
  )
