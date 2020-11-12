#lang scheme
(require "preguntaTDA.rkt")
(require "StackTDA.rkt")
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
        (list username password (list) 100) ;cada user empieza con 100 de reputacion
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


;Modificadores
;descripción: Funcion que agrega un id de pregunta a un usuario, retorna una lista actualizada
;dom: lista x lista x stack
;rec: lista
;tipo recursion: natural
(define addQuestionToUser
  (lambda (listaUsuarios username stack)
    (if (null? listaUsuarios);Caso base alcanzado al llegar al final de la lista o si la lista se encuentra vacia
        #f ;No se encontro al usuario
        (if (equal? (car(car listaUsuarios)) username) ;Segundo caso base, compara el nombre del usuario de la posicion del stack con username
            (list (userGetUsername (car listaUsuarios)) (userGetPassword (car listaUsuarios)) (append (userGetQuestions (car listaUsuarios)) (list (crearIdPregunta (stackGetQuestions stack)))) (userGetReputation (car listaUsuarios)));Se agrega la pregunta al usuario
            (yaEstaRegistrado (cdr listaUsuarios) username) ;Descomposicion recursiva, se pasara al siguiente usuario
            )
        )
    )
  )