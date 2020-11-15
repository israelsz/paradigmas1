#lang scheme
(provide (all-defined-out))

;Implementacion del TDA respuesta

;Representación

;(string X entero X entero X string X date X labels X string X entero)
;(list respuesta idPreguntaQueResponde idRespuesta autor (fecha) (etiquetas) estadoRespuesta votosTotales)

;Constructor

;descripción: Permite crear una respuesta
;dom: string X entero X string X date x labels
;rec: lista
(define crearRespuesta
  (lambda (respuesta idPreguntaQueResponde autor date labels stackRespuestas)
    (if (and (string? respuesta) (list? labels) (integer? idPreguntaQueResponde))
        (list respuesta idPreguntaQueResponde (crearIdRespuesta stackRespuestas) autor date labels "pendiente" 0)
        (raise "Verifique que la respuesta sea un string y el id un entero")
        )
    )
  )

;Selectores

;descripción: Funcion que retorna la respuesta de una lista respuesta
;dom: lista
;rec: string
(define answerGetRespuesta
  (lambda (respuesta)
    (list-ref respuesta 0)
    )
  )

;descripción: Funcion que retorna el id de la pregunta a la que responde la pregunta
;dom: lista
;rec: entero
(define answerGetIdPregunta
  (lambda (respuesta)
    (list-ref respuesta 1)
    )
  )

;descripción: Funcion que retorna el id de una respuesta
;dom: lista
;rec: entero
(define answerGetId
  (lambda (respuesta)
    (list-ref respuesta 2)
    )
  )

;descripción: Funcion que retorna el autor de una respuesta
;dom: lista
;rec: string
(define answerGetUser
  (lambda (respuesta)
    (list-ref respuesta 3)
    )
  )

;descripción: Funcion que retorna la fecha de una respuesta
;dom: lista
;rec: date
(define answerGetDate
  (lambda (respuesta)
    (list-ref respuesta 4)
    )
  )

;descripción: Funcion que retorna las etiquetas de una respuesta
;dom: lista
;rec: lista
(define answerGetLabels
  (lambda (respuesta)
    (list-ref respuesta 5)
    )
  )

;descripción: Funcion que retorna el estado de una respuesta
;dom: lista
;rec: string
(define answerGetStatus
  (lambda (respuesta)
    (list-ref respuesta 6)
    )
  )

;descripción: Funcion que retorna los votos totales de una respuesta
;dom: lista
;rec: string
(define answerGetVotes
  (lambda (respuesta)
    (list-ref respuesta 7)
    )
  )


;Otras funciones

;descripción: Permite crear un id para una respuesta
;dom: lista
;rec: entero
(define crearIdRespuesta
  (lambda (stackRespuestas)
    (if (null? stackRespuestas);Si no hay respuestas en el stack
        1 ;el id de la respuesta sera 1
        ;En caso de haber respuestas en el stack
        (+(length stackRespuestas)1);se conseguira el numero de respuestas y se le añadira 1 para crear un nuevo id.
        )
    )
  )