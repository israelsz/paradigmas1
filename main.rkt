#lang scheme
(require "StackTDA.rkt")
(require "UsuariosTDA.rkt")


;descripción: Permite saber si un usuario ya se encuentra dentro del stack.
;dom: lista X string
;rec: booleano
(define yaEstaRegistrado
  (lambda (listaUsuarios username)
    (if (null? listaUsuarios)
        #f ;No esta registrado
        (if (equal? (car(car listaUsuarios)) username)
            #t ; Ya se encuentra registrado
            (yaEstaRegistrado (cdr listaUsuarios) username)
            )
        )
    )
  )

;descripción: Permite crear un usuario, junto a su contraseña correspondiente
;dom: string X string
;rec: lista
(define register
  (lambda (stack username password)
    ;En primer lugar se debe verificar si el usuario que se quiere registrar ya se encuentra registrado dentro del stack
    (if (equal? (yaEstaRegistrado (stackGetUsers stack) username) #t) ;Si ya esta registrado
        stack ;Se retorna el mismo stack entrante sin cambios
        ;Si no esta registrado, se registra al usuario
        (list (stackGetQuestions stack) (stackGetAnswers stack) (append (stackGetUsers stack) (list (crearUsuario username password))) (stackGetActiveUsers stack))
        )
    )
  )