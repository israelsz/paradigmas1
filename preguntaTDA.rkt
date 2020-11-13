#lang scheme
(provide (all-defined-out))

;Implementacion del TDA pregunta

;Representación

;(string X entero X string x date x labels x string x entero x entero)
;(list pregunta idPregunta autor (fecha) (etiquetas) estadoPregunta recompensaOfrecida votosTotales) 

;CONSTRUCTOR
;descripción: Permite crear una pregunta
;dom: string X entero X string X date x labels x lista
;rec: lista
(define crearPregunta
  (lambda (pregunta autor date labels stackPreguntas)
    (if (and (string? pregunta) (list? labels))
        (list pregunta (crearIdPregunta stackPreguntas) autor date labels "abierta" 0 0)
        (raise "Verifique que la pregunta sea un string y los labels los ingrese como una lista")
        )
    )
  )

;Selectores

;descripción: Funcion que retorna lo preguntado en una Pregunta
;dom: lista
;rec: string
(define QuestionGetPregunta
  (lambda (pregunta)
    (list-ref pregunta 0)
    )
  )

;descripción: Funcion que retorna el id de una pregunta
;dom: lista
;rec: entero
(define QuestionGetId
  (lambda (pregunta)
    (list-ref pregunta 1)
    )
  )

;descripción: Funcion que retorna el autor de una pregunta
;dom: lista
;rec: string
(define QuestionGetUser
  (lambda (pregunta)
    (list-ref pregunta 2)
    )
  )

;descripción: Funcion que retorna la fecha de una pregunta
;dom: lista
;rec: date
(define QuestionGetDate
  (lambda (pregunta)
    (list-ref pregunta 3)
    )
  )

;descripción: Funcion que retorna las etiquetas de una pregunta
;dom: lista
;rec: lista
(define QuestionGetLabels
  (lambda (pregunta)
    (list-ref pregunta 4)
    )
  )

;descripción: Funcion que retorna el status de una pregunta
;dom: lista
;rec: string
(define QuestionGetStatus
  (lambda (pregunta)
    (list-ref pregunta 5)
    )
  )

;descripción: Funcion que retorna la recompensa ofrecida por la pregunta
;dom: lista
;rec: entero
(define QuestionGetRecompensa
  (lambda (pregunta)
    (list-ref pregunta 6)
    )
  )

;descripción: Funcion que retorna los votos totales de una pregunta
;dom: lista
;rec: entero
(define QuestionGetVotes
  (lambda (pregunta)
    (list-ref pregunta 7)
    )
  )

;Otras funciones

;Esta funcion es usada por el constructor del tda para funcionar, pero tambien funciona independientemente

;descripción: Permite crear un id para una pregunta
;dom: lista
;rec: entero
(define crearIdPregunta
  (lambda (stackPreguntas)
    (if (null? stackPreguntas);Si no hay preguntas en el stack
        1 ;el id de la pregunta sera 1
        ;En caso de haber preguntas en el stack
        (+(length stackPreguntas)1);se conseguira el numero de preguntas y se le añadira 1 para crear un nuevo id.
        )
    )
  )

