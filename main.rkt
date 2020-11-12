#lang scheme
(require "StackTDA.rkt")
(require "UsuariosTDA.rkt")


;descripción: Permite crear un usuario, junto a su contraseña correspondiente
;dom: stack X string X string
;rec: stack
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

;descripción: Permite crear un usuario, junto a su contraseña correspondiente
;dom: stack X string X string X function
;rec: stack
(define login
  (lambda (stack username password operation)
    (if (equal? (verificarUserPassword (stackGetUsers stack) username password) #t);Si se encuentra el usuario y su password es la misma registrada
        ;se debe conectar al usuario y retornar la funcion currificada entregandole el stack con el usuario conectado ya como parametro
        (operation (list (stackGetQuestions stack) (stackGetAnswers stack) (stackGetUsers stack) (append (stackGetActiveUsers stack) (list username))))
        ;de lo contrario solo se debe retornar operation
        (operation)
        )
    )
  )

;descripción: Permite a un usuario logueado realizar una pregunta
;dom: stack X date X string X (list string)
;rec: stack
(define ask
  (lambda (stack)
    (lambda (date)
      (lambda (pregunta
      (list-ref stack date)
      )
    )
  )
