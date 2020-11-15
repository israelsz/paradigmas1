#lang scheme
(require "StackTDA.rkt")
(require "UsuariosTDA.rkt")
(require "preguntaTDA.rkt")
(require "fechaTDA.rkt")
(require "respuestaTDA.rkt")


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
    (lambda (fecha)
      (lambda (pregunta . labels)
        ;En primer lugar se verifica que el usuario se encuentre logueado
        (if (equal? (isUserLoggedIn? (stackGetActiveUsers stack)) #f)
            stack ;si no esta loggeado, se retorna el stack sin cambios
            ;En caso de encontrarse loggeado, se crea la nueva pregunta y se deslogea al usuario
            (list (append (stackGetQuestions stack) (list(crearPregunta pregunta (stackGetLoggedUser stack) fecha labels (stackGetQuestions stack)))) (stackGetAnswers stack) (addQuestionToUser (stackGetUsers stack) (stackGetLoggedUser stack) stack) (list))
            )
        )
      )
    )
  )

;descripción: Permite a un usuario logueado ofrecer una recompensa por una pregunta resuelta
;dom: stack X entero X entero
;rec: stack
(define reward
  (lambda (stack)
    (lambda (idPregunta)
      (lambda (recompensa)
        ;En primer lugar se verifica que el usuario se encuentre logueado
        (if (equal? (isUserLoggedIn? (stackGetActiveUsers stack)) #f)
            stack ;si no esta loggeado, se retorna el stack sin cambios
            ;En caso de estar loggeado se revisara que el id y la recompensa sean enteros
            (if (not(and (integer? idPregunta) (integer? recompensa)))
                stack ;no son enteros, se retorna el mismo stack sin cambios
                (if (equal? (puedeDarReputacion? (stackGetUsers stack) (stackGetLoggedUser stack) recompensa) #t)
                    (traspasarRecompensa (stackGetUsers stack) (stackGetQuestions stack) (stackGetLoggedUser stack) idPregunta recompensa stack)
                     stack ;no puede dar la recompensa, entonces se retorna el stack sin cambios
                     )
                )
            )
        )
      )
    )
  )

;descripción: Permite a un usuario logueado responder una pregunta
;dom: stack X date X entero X string X (list string)
;rec: stack
(define answer
  (lambda (stack)
    (lambda (fecha)
      (lambda (idPreguntaQueResponde)
        (lambda (respuesta . labels)
          ;En primer lugar se verifica que el usuario se encuentre logueado
          (if (equal? (isUserLoggedIn? (stackGetActiveUsers stack)) #f)
              stack ;si no esta loggeado, se retorna el stack sin cambios
              ;En caso de estar loggeado, registra la pregunta, retornando un stack con la pregunta
              (list (stackGetQuestions stack) (append (stackGetAnswers stack) (list(crearRespuesta respuesta idPreguntaQueResponde (stackGetLoggedUser stack) fecha labels (stackGetAnswers stack)))) (stackGetUsers stack) (list))
              )
          )
        )
      )
    )
  )

;descripción: Permite a un usuario logueado aceptar una respuesta a una de sus preguntas
;dom: stack X entero X entero
;rec: stack
(define accept
  (lambda (stack)
    (lambda (idPregunta)
      (lambda (idRespuesta)
        ;En primer lugar se verifica que el usuario se encuentre logueado
        (if (equal? (isUserLoggedIn? (stackGetActiveUsers stack)) #f)
            stack ;si no esta loggeado, se retorna el stack sin cambios
            ;En caso de estar loggeado se revisara que el id de pregunta ingresado corresponda a una pregunta hecha por el usuario loggeado.
            (if (equal? (isPreguntaOfUser? (stackGetUsers stack) (stackGetLoggedUser stack) idPregunta) #f)
                stack ;si la pregunta no fue hecha por el usuario ingresado, retorna el stack sin cambios
                ;En caso que la pregunta si sea una pregunta del usuario ingresado, se verificara si la respuesta que se quiere recompensar, sea una respuesta a la pregunta seleccionada
                (if (equal? (respuestaApuntaAPregunta? (stackGetAnswers stack) idPregunta idRespuesta) #f)
                    stack ;el id respuesta ingresado no apunta a la respuesta que se quiere aceptar, devuelve el stack sin cambios
                    ;En caso que el id de la respuesta si apunte al id de la pregunta ingresada, se efectuara la actualizacion de puntaje correspondiente
                    (distribuirReputacionYCerrarPregunta (stackGetUsers stack) (stackGetQuestions stack) (stackGetAnswers stack) (stackGetLoggedUser stack) idPregunta idRespuesta)
                    )
                )
            )
        )
      )
    )
  )

;descripción: Funcion que recibe el stack y genera un string de manera de poder visualizar el stack de manera comprensible
;dom: stack
;rec: string
(define stack->string
  (lambda (stack)
    ;En primer lugar se verifica que el usuario se encuentre logueado, o sea, esta funcion fue invocada a traves del login
    (if (equal? (isUserLoggedIn? (stackGetActiveUsers stack)) #f)
        ;Si no fue invocada a traves del login, imprime todo el stack
        (string-append (string-join (preguntas->string (stackGetQuestions stack) stack) "") "Usuarios Registrados en el Foro: \n" (string-join (usuarios->string (stackGetUsers stack)) ""))
        ;Si fue invocada a traves del login, imprime todo el stack, aparte de la informacion del usuario conectado
        (string-append (string-join (preguntas->string (stackGetQuestions stack) stack) "") "Usuarios Registrados en el Foro: \n" (string-join (usuarios->string (stackGetUsers stack)) "") "\n" "Usuarios Conectados: \n" (stackGetLoggedUser stack) "\n")
        )
    )
  )

;descripción: Funcion que permite a un usuario votar por una pregunta o respuesta, el voto puede ser positivo o negativo, retorna un stack actualizado de acuerdo al tipo de voto
;dom: stack X function X entero X booleano
;rec: stack
(define vote
  (lambda (stack)
    (lambda (funcion)
      (lambda(idPreguntaORespuesta)
        (lambda(booleano)
          (if (equal? booleano true);se restara o sumara un  voto ?
              ;ejecutar sumar puntaje
              (votoPositivo stack (car ((funcion idPreguntaORespuesta) stack)) idPreguntaORespuesta)
              (if (equal? booleano false)
                  ;ejecuta restar puntaje
                  (votoNegativo stack (car ((funcion idPreguntaORespuesta) stack)) idPreguntaORespuesta)
                  stack; no se ingreso ni true ni false, retorna el stack sin cambios
                  )
              )
          )
        )
      )
    )
  )

  








            
