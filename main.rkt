#lang scheme
(require "StackTDA.rkt")
(require "UsuariosTDA.rkt")
(require "preguntaTDA.rkt")
(require "fechaTDA.rkt")
(require "respuestaTDA.rkt")

;Para el correcto funcionamiento de las funciones a continuación, todas usan el constructor de stack
;Ejemplo del creador de stack vacio:
;(crearStack)

;Para poder ejemplificar las funciones de una manera entendible y sin tener extensiones enormes de funciones dentro de funciones, se usara define para ir re definiendo el stack, entonces
;Se pide ir ejecutando linealmente los ejemplos y definiciones a medida que se vayan presentando las funciones y sus ejemplos, ya que toda funcion necesita de las funciones previas para su funcionamiento.
;En primer lugar se define el stack:

;(define stackOverflow(crearStack))


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

;Ejemplos de la funcion register

;Registra a Pedro en el stack:

;(define stackOverflow(register stackOverflow "pedro" "123"))

;Registra a israel en el stack, donde ya esta registrado Pedro

;(define stackOverflow(register stackOverflow "israel" "qwerty"))

;Registra a maria en el stack, donde ya estan registrados Pedro e Israel

;(define stackOverflow(register stackOverflow "maria" "456"))

;Para visualizar los cambios de una manera comprensible se puede hacer uso de la funcion stack->string, definida mas adelante, junto con un display de la misma

;(display(stack->string stackOverflow))



;descripción: Permite a un usuario registrado, conectarse y ejecutar una funcion en el stack
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

;Ejemplos de la función login:
;La función login funciona con argumento otra operacion del main, por si sola "conecta" al usuario
;Para ver esto se usara como ejemplo la funcion stack->string loggeando a uno de los usuarios registrados anteriormente

;Se conecta al usuario israel Y ejecuta la funcion stack->string:
;(login stackOverflow "israel" "qwerty" stack->string)

;Se conecta a la usuaria maria Y ejecuta la funcion stack->string:
;(login stackOverflow "maria" "456" stack->string)

;Se conecta al usuario pedro y ejecuta stack->string
;(login stackOverflow "pedro" "123" stack->string)


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

;Ejemplos de la funcion ask:

;El usuario israel hace una pregunta
;(define stackOverflow(((login stackOverflow "israel" "qwerty" ask)(date 16 11 2020))"Hola ¿como puedo hacer hola mundo?" "python"))

;El usuario pedro hace una pregunta
;(define stackOverflow(((login stackOverflow "pedro" "123" ask)(date 17 11 2020))"Soy nuevo, como puedo usar el foro ?" "ayuda" "usuario nuevo"))

;La usuaria maria hace una pregunta
;(define stackOverflow(((login stackOverflow "maria" "456" ask)(date 17 11 2020))"Como puedo hacer iteraciones en scheme ?" "paradigma funcional" "scheme"))

;Para visualizar de mejor manera las tres preguntas registradas en el stack, aparte de todo lo registrado anteriormente, se recomienda el uso de la funcion stack->string con un display de la misma:
;(display (stack->string stackOverflow))




;descripción: Permite a un usuario logueado ofrecer una recompensa por una pregunta presente en el stack
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

;Ejemplos de la funcion reward:

;El usuario pedro ofrece 300 de recompensa por la pregunta con id 2:

;(define stackOverflow(((login stackOverflow "pedro" "123" reward)2)300))

;El usuario israel ofrece 50 de recompensa por pregunta con id 3:

;(define stackOverflow(((login stackOverflow "israel" "qwerty" reward)3)50))

;El usuario israel ofrece ahora 100 de recompensa por la pregunta 1:

;(define stackOverflow(((login stackOverflow "israel" "qwerty" reward)1)100))

;Para visualizar de mejor manera las tres recompensas registradas en el stack, aparte de todo lo registrado anteriormente, se recomienda el uso de la funcion stack->string con un display de la misma:

;(display (stack->string stackOverflow))



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

;Ejemplos de la funcion answer:

;El usuario israel responde a la pregunta con id 2 del usuario Pedro

;(define stackOverflow((((login stackOverflow "israel" "qwerty" answer)(date 18 11 2020))2) "Hola, te recomiendo leer el informe y los ejemplos del archivo main.rtk" "duda" "foro"))

;El usuario pedro responde a la pregunta con id 3 de la usuaria maria

;(define stackOverflow((((login stackOverflow "pedro" "123" answer)(date 18 11 2020))3) "Hola, hay que usar recursión a fin de iterar sobre algo en scheme" "recursion" "scheme"))

;La usuaria maria responde a la pregunta con id 1 del usuario israel

;(define stackOverflow((((login stackOverflow "maria" "456" answer)(date 19 11 2020))1) "Puedes hacer uso de la funcion print en python para ello" "duda" "python"))

; Para visualizar de mejor manera la respuesta creada, se puede hacer uso de stack-> string

;(display (stack->string stackOverflow))

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

;Ejemplos de la funcion accept:

;La usuaria maria acepta la respuesta con id 2 de su pregunta con id 3, lo que cierra la pregunta, y le da la recompensa de la pregunta al usuario de la respuesta aceptada, modificando la reputacion

;(define stackOverflow(((login stackOverflow "maria" "456" accept)3)2))

;El usuario israel acepta la respuesta con id 2 de su pregunta con id 3, lo que cierra la pregunta, y le da la recompensa de la pregunta al usuario de la respuesta aceptada, modificando la reputacion

;(define stackOverflow(((login stackOverflow "israel" "qwerty" accept)1)3))

;El usuario pedro acepta la respuesta con id 1 de su pregunta con id 2, lo que cierra la pregunta, y le da la recompensa de la pregunta al usuario de la respuesta aceptada, modificando la reputacion

;(define stackOverflow(((login stackOverflow "pedro" "123" accept)2)1))

;Para visualizar de mejor manera las respuestas aceptadas y los cambios, se puede hacer uso de stack-> string

;(display (stack->string stackOverflow))


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


;Ejemplos de stack->string:

;Todos las funciones incorporadas llevan un ejemplo usando stack->string, se dejara como ejemplo el stack que se ha ido formando con la ejecución de las distintas funciones hasta ahora:
;transforma el stack actual a una string:

;(stack->string stackOverflow)

;para ser mostrada se puede usar display:

;(display (stack->string stackOverflow))

;Ejemplo de stack->string siendo llamada desde el login:
;(login stackOverflow "israel" "qwerty" stack->string)



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


;Ejemplos de vote:

;El usuario Israel vota negativamente a la pregunta con id 2:

;(define stackOverflow((((login stackOverflow "israel" "qwerty" vote)getQuestion)2)false))

;El usuario pedro vota positivamente por la respuesta con id 3 hecha en la pregunta con id 1:

;(define stackOverflow((((login stackOverflow "pedro" "123" vote)(getAnswer 1))3)true))

;La usuaria maria vota negativamente por la respuesta con id 1 hecha en la pregunta con id 2:

;(define stackOverflow ((((login stackOverflow "maria" "456" vote)(getAnswer 2))1)false))

;El usuario pedro vota positivamente por la pregunta con id 3:

;(define stackOverflow((((login stackOverflow "pedro" "123" vote)getQuestion)3)true))

;Para visualizar de mejor manera el cambio en los votos y reputacion, se puede hacer uso de stack-> string

;(display (stack->string stackOverflow))