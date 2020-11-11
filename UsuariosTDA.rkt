#lang scheme
(provide (all-defined-out))

;Implementacion del TDA usuarios

;Representacion
;((string X string) (string))
;(list usuario password (list preguntas))

;Constructor
;descripción: Permite crear un usuario, junto a su contraseña correspondiente
;dom: string X string
;rec: lista
(define crearUsuario
  (lambda (username password)
    (if (and (string? username) (string? password))
        (list username password (list))
        (raise "Solo se permite strings para el username y la password")
        )
    )
  )
