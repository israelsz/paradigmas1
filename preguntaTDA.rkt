#lang scheme
(provide (all-defined-out))

;Implementacion del TDA pregunta

;Representación

;(string X entero X string x date x labels x string)
;(list pregunta idPregunta autor (fecha) (etiquetas) estadoPregunta) 

;CONSTRUCTOR
;descripción: Permite crear una pregunta
;dom: string X entero X string X date x labels
;rec: lista
(define crearPregunta
  (lambda (pregunta autor date labels stackPreguntas)
    (if (and (string? pregunta) (list? labels))
        (list pregunta (crearIdPregunta stackPreguntas) autor date labels "abierta")
        (raise "Verifique que la pregunta sea un string y los labels los ingrese como una lista")
        )
    )
  )


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
