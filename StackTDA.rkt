#lang scheme
(provide (all-defined-out))

;Implementacion del TDA stack

;Representacion
;(list X list X list X list)
;(list preguntas respuestas usuarios nombreUsuarioActivo)

;Constructores

;descripción: Permite Crear un Stack vacio
;dom: ----
;rec: lista
(define (crearStack)
  (list(list)(list)(list(list)(list))(list)
       )
  )

;Pertenencia

;Selectores

;POR CONSTRUIR ELEGIR PREGUNTAS
; (list-ref(list-ref stack 0)1) REEMPLAZAR EL 1 POR NUMERO DE PREGUNTA A ELEGIR

;descripción: Funcion que retorna la lista con preguntas
;dom: list
;rec: list
(define (selecListaPreguntas lista)
  (list-ref lista 0)
  )

;descripción: Funcion que retorna la lista con respuestas
;dom: list
;rec: list
(define (selecListaRespuestas lista)
  (list-ref lista 1)
  )

;descripción: Funcion que retorna la lista con usuarios
;dom: list
;rec: list
(define (selecListaUsuarios lista)
  (list-ref lista 2)
  )

;descripción: Funcion que retorna la lista con Nombres de usuarios activos.
;dom: list
;rec: list
(define (selecListaUsuariosActivos lista)
  (list-ref lista 3)
  )

;descripción: Funcion que retorna la lista con Nombres de usuarios registrados.
;dom: list
;rec: list
(define(selecListaUsuariosRegistrados stack)
  (list-ref(list-ref stack 2)0)
  )

;descripción: Funcion que retorna la lista con Contraseñas de los usuarios registrados.
;dom: list
;rec: list
(define(selecListaPasswords stack)
  (list-ref(list-ref stack 2)1)
  )


;Modificadores

;descripción: Agrega una pregunta al stack
;dom: list X elemento
;rec: list
(define (addQuestion stack x)
  (list(modificarPos0 stack x)(selecListaRespuestas stack)(selecListaUsuarios stack)(selecListaUsuariosActivos stack)
       )
  )

;descripción: Agrega una respuesta al stack
;dom: list X elemento
;rec: list
(define (addAnswer stack x)
  (list(selecListaPreguntas stack)(modificarPos1 stack x)(selecListaUsuarios stack)(selecListaUsuariosActivos stack)
       )
  )

;descripción: Agrega un usuario y su contraseña al stack
;dom: list X string x string
;rec: list
(define(addUserPassword stack nombre password)
  (list(selecListaPreguntas stack)(selecListaRespuestas stack)(list(modificarPos2_0 stack nombre)(modificarPos2_1 stack password))(selecListaUsuariosActivos stack)
       )
  )

;Otras Operaciones


;LAS SIGUIENTES FUNCIONES SON COMPLEMENTARIAS/AUXILIARES AL TDA. NO FORMAN PARTE DEL TDA STACK, PERO
;EL TDA FECHA LAS EMPLEA PARA PODER REALIZAR SU TRABAJO. ESTAS FUNCIONES DE IGUAL FORMA PUEDEN
;SER EMPLEADAS INDEPENDIENTEMENTE DE LA EXISTENCIA DEL TDA STACK, POR LO QUE NO EXISTE ACOPLAMIENTO CON EL TDA.
;POR OTRO LADO, EL TDA SEGUN LA IMPLEMENTACION REALIZADA A CONTINUACION, ESTA ACOPLADO A ESTAS FUNCIONES.


;descripción: Agrega un elemento a la posicion de Preguntas del stack
;dom: list X elemento
;rec: list
(define (modificarPos0 stack x)
  (append(selecListaPreguntas stack)(list x)
         )
  )

;descripción: Agrega un elemento a la posicion de Respuestas del stack
;dom: list X elemento
;rec: list
(define (modificarPos1 stack x)
  (append(selecListaRespuestas stack)(list x)
         )
  )

;descripción: Agrega un elemento a la posicion de usuarios del stack
;dom: list X elemento
;rec: list
(define(modificarPos2_0 stack nombre)
  (append(selecListaUsuariosRegistrados stack)(list nombre)
         )
  )

;descripción: Agrega un elemento a la posicion de contraseñas del stack
;dom: list X elemento
;rec: list
(define(modificarPos2_1 stack password)
  (append(selecListaPasswords stack)(list password)
         )
  )