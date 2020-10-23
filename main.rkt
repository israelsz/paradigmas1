#lang scheme

;Implementacion del TDA stack

;Representacion
;(list X list X list X list)
;(list preguntas respuestas usuarios nombreUsuarioActivo)

;Constructores

;descripción: Permite Crear un Stack vacio
;dom: ----
;rec: lista
(define (crearStack)
  (list(list)(list)(list)(list)
       )
  )
;Pertenencia

;Selectores

(define (selecListaPreguntas lista)
  (list-ref lista 0)
  )

;Modificadores
(define (agregarLista lista x)
  (append lista (list x)))

(define (agregarListaPos0 stack x)
  (list(modificarPos0 stack x)(list)(list)(list)
       )
  )
  
(define (modificarPos0 stack x)
  (append(selecListaPreguntas stack)(list x)
         )
  )
       
;Otras Operaciones
