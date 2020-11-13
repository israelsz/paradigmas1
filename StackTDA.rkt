#lang scheme
(require "UsuariosTDA.rkt")
(require "preguntaTDA.rkt")
(provide (all-defined-out))

;Implementacion del TDA stack
;El tda Stack es la base para este programa

;Representacion
;(list X list X list X list)
;(list preguntas respuestas usuarios nombreUsuarioActivo)



;Constructores

;descripción: Permite Crear un Stack vacio
;dom: ----
;rec: lista
(define (crearStack)
  (list (list) (list) (list) (list))
  )
;Pertenencia

;Selectores


;descripción: Funcion que retorna la lista con preguntas del stack, para ello se solicita el stack
;dom: list
;rec: list
(define stackGetQuestions
  (lambda (stack)
    (list-ref stack 0)
    )
  )
  

;descripción:Funcion que retorna la lista con respuestas del stack, para ello se solicita el stack
;dom: list
;rec: list
(define stackGetAnswers
  (lambda (stack)
    (list-ref stack 1)
    )
  )

;descripción: Funcion que retorna la lista con usuarios del stack, para ello se solicita el stack
;dom: lista
;rec: lista
(define stackGetUsers
  (lambda (stack)
    (list-ref stack 2)
    )
  )

;descripción: Funcion que retorna la lista con Nombres de usuarios activos.
;dom: list
;rec: list
(define stackGetActiveUsers
  (lambda (stack)
    (list-ref stack 3)
    )
  )


;descripción: Funcion que retorna el username del usuario que se encuentra loggeado.
;dom: list
;rec: string
(define stackGetLoggedUser
  (lambda (stack)
    (list-ref (list-ref stack 3)0)
    )
  )



;Otras Operaciones

;descripción: Permite saber si un usuario ya se encuentra dentro del stack de usuarios.
;dom: lista X string
;rec: booleano
;tipo de recursión: natural
(define yaEstaRegistrado
  (lambda (listaUsuarios username)
    (if (null? listaUsuarios);Caso base alcanzado al llegar al final de la lista o si la lista se encuentra vacia
        #f ;No esta registrado
        (if (equal? (car(car listaUsuarios)) username) ;Segundo caso base, compara el nombre del usuario de la posicion del stack con username
            #t ; Ya se encuentra registrado
            (yaEstaRegistrado (cdr listaUsuarios) username) ;Descomposicion recursiva, se pasara al siguiente usuario
            )
        )
    )
  )

;descripción: Permite comparar si la password ingresada es igual a la del usuario
;dom: lista X string
;rec: booleano
;tipo de recursión: natural
(define verificarUserPassword
  (lambda (listaUsuarios username password)
    (if (null? listaUsuarios);Caso base alcanzado al llegar al final de la lista o si la lista se encuentra vacia
        #f ;El usuario no existe
        (if (equal? (car(car listaUsuarios)) username) ;Segundo caso base, compara la password del usuario de la posicion del stack con username
            (if (equal? (car(cdr(car listaUsuarios))) password); Si se encuentra el username, se verificara si coincide su password
                #t ;El usuario existe y su password es correcta
                #f ;El usuario existe, pero la password no es correcta
            )
            (verificarUserPassword (cdr listaUsuarios) username password) ;Descomposicion recursiva, se pasara a revisar al siguiente usuario
            )
        )
    )
  )
            
;descripción: Permite saber si un usuario se encuentra conectado o no
;dom: lista
;rec: booleano
(define isUserLoggedIn?
  (lambda (stackUsuariosActivos)
    (if (null? stackUsuariosActivos)
        #f ;Si el stack de usuarios activos esta vacío, el usuario no esta loggeado
        #t ;Si tiene un elemento, se encuentra loggeado
        )
    )
  )

;descripción: Permite saber si un usuario puede dar una determinada recompensa o no
;dom: lista X string X entero
;rec: booleano
(define puedeDarReputacion?
  (lambda (stackUsuarios username recompensa)
    (if (equal? (userGetUsername(car stackUsuarios)) username) ;Si encontramos al usuario que quiere dar reputacion
        (if (>=(userGetReputation (car stackUsuarios)) recompensa) ;Se verifica si la reputacion del usuario es mayor a la que se quiere ofrecer
            #t ;si tiene  mas reputacion, puede ofrecer la recompensa
            #f ;no tiene mas reputacion que la recompensa que quiere ofrecer
            )
        (puedeDarReputacion? (cdr stackUsuarios) username recompensa) ;Descomposicion recursiva, se verificara al siguiente usuario
        )
    )
  )

;descripción: Funcion que agrega un id de pregunta a un usuario, retorna una lista actualizada
;dom: lista x lista x stack
;rec: lista
(define addQuestionToUser
  (lambda (stackUsuarios username stack)
    (map (lambda (usuario)
       (if (equal? (car usuario) username) ;Se verifica si es el usuario al que se le quiere agregar el id de pregunta
           (list (userGetUsername usuario) (userGetPassword usuario) (append (userGetQuestions usuario) (list (crearIdPregunta (stackGetQuestions stack)))) (userGetReputation usuario)) ;Se le agrega el id de pregunta
           usuario)) ;Si no lo es se mantiene el stack y se verifica la siguiente posicion
     stackUsuarios)
    )
  )


;descripción: Funcion que descuenta una recompensa a un usuario, retorna el stack de usuarios actualizado
;dom: lista X lista X string X entero X entero
;rec: lista
(define descontarRecompensaUsuario
  (lambda (stackUsuarios username recompensa)
    (map (lambda (usuario)
       (if (equal? (userGetUsername usuario) username) ;Se verifica si es el usuario al que se le quiere descontar la recompensa
           (list (userGetUsername usuario) (userGetPassword usuario) (userGetQuestions usuario) (- (userGetReputation usuario) recompensa)) ;Se le descuenta la recompensa
           usuario)) ;Si no lo es se mantiene el stack y se verifica la siguiente posicion
     stackUsuarios)
    )
  )

;descripción: Funcion que agrega una recompensa a la pregunta del id señalado retorna el stack de preguntas actualizado
;dom: lista X lista X string X entero X entero
;rec: lista
(define agregarRecompensaPregunta
  (lambda (stackPreguntas idPregunta recompensa)
    (map (lambda (pregunta)
       (if (equal? (QuestionGetId pregunta) idPregunta) ;Se verifica si es la pregunta a la que se le quiere agregar la recompensa
           (list (QuestionGetPregunta pregunta) (QuestionGetId pregunta)(QuestionGetUser pregunta) (QuestionGetDate pregunta) (QuestionGetLabels pregunta) (QuestionGetStatus pregunta) (+(QuestionGetRecompensa pregunta) recompensa) (QuestionGetVotes pregunta)) ;Se le descuenta la recompensa
           pregunta)) ;Si no lo es se mantiene el stack y se verifica la siguiente posicion
     stackPreguntas)
    )
  )

;descripción: Funcion que trapasa la determinada recompensa de un usuario a una pregunta, retorna un stack actualizado
;dom: lista X lista X string X entero X entero
;rec: stack
(define traspasarRecompensa
  (lambda (stackUsuarios stackPreguntas username idPregunta recompensa stack)
    (list (agregarRecompensaPregunta stackPreguntas idPregunta recompensa) (stackGetAnswers stack) (descontarRecompensaUsuario stackUsuarios username recompensa) (list))
    )
  )