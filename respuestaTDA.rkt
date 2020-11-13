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
        (list respuesta idPreguntaQueResponde (crearIdRespuesta stackRespuestas) autor date labels "abierta" 0)
        (raise "Verifique que la respuesta sea un string y el id un entero")
        )
    )
  )

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