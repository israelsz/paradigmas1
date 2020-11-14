#lang scheme
(require "UsuariosTDA.rkt")
(require "preguntaTDA.rkt")
(require "respuestaTDA.rkt")
(require "fechaTDA.rkt")
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
;dom: lista X string X string
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


;descripción: Permite saber si una pregunta se encuentra en una lista o no
;dom: lista X entero
;rec: booleano
;tipo de recursión: natural
(define estaPregunta?
  (lambda (listaPreguntaUsuario idPregunta)
    (if (null? listaPreguntaUsuario) ;Caso base alcanzado al llegar al final de la lista o si esta se encuentra vacia
        #f ;No esta la pregunta
        (if (equal? (car listaPreguntaUsuario) idPregunta)
            #t ;la pregunta se encuentra en la lista de preguntas del usuario
            (estaPregunta? (cdr listaPreguntaUsuario) idPregunta) ;Descomposicion recursiva, se pasa a revisar la siguiente pregunta
            )
        )
    )
  )


;descripción: Permite saber si una pregunta es del usuario ingresado o no
;dom: lista X string X entero
;rec: booleano
;tipo de recursión: natural
(define isPreguntaOfUser?
  (lambda (listaUsuarios username idPregunta)
    (if (null? listaUsuarios);Caso base alcanzado al llegar al final de la lista o si la lista se encuentra vacia
        #f ;El usuario no existe
        (if (equal? (userGetUsername (car listaUsuarios)) username) ;Se encuentra el user ?
            (if (equal? (estaPregunta? (userGetQuestions (car listaUsuarios)) idPregunta) #t) ;En caso de haber encontrado al user se buscara si la pregunta se encuentra hecha por el
                #t ;La pregunta fue hecha por el usuario ingresado
                #f ;La pregunta no fue hecha por el usuario ingresado
            )
            (isPreguntaOfUser? (cdr listaUsuarios) username idPregunta) ;Descomposicion recursiva, se pasara a revisar al siguiente usuario
            )
        )
    )
  )


;descripción: Permite saber si un  id de respuesta apunta al id de pregunta ingresado
;dom: lista X entero X entero
;rec: booleano
;tipo de recursión: natural
(define respuestaApuntaAPregunta?
  (lambda (listaRespuestas idPregunta idRespuesta)
    (if (null? listaRespuestas) ;Caso base alcanzado a llegar al final de la lista o si esta se encuentra vacia
        #f ;No esta la respuesta buscada
        (if (equal? (answerGetId (car listaRespuestas)) idRespuesta) ;Si se encuentra la respuesta buscada
            (if (equal? (answerGetIdPregunta (car listaRespuestas)) idPregunta);Si una vez encontrada la respuesta buscada, se revisa el id de pregunta al que responde
                #t ;si, el idRespuesta ingresado responde al idPregunta ingresado
                #f ;La respuesta no responde al idPregunta ingresado
                )
            (respuestaApuntaAPregunta? (cdr listaRespuestas) idPregunta idRespuesta) ;Descomposicion recursiva, se pasara a revisar la siguiente respuesta
            )
        )
    )
  )
    
;descripción: Funcion que cambia el estado a cerrado y fija en cero la recompensa de la pregunta
;dom: lista X entero
;rec: lista
(define cerrarPreguntaYEliminarRecompensa
  (lambda (stackPreguntas idPregunta)
    (map (lambda (pregunta)
       (if (equal? (QuestionGetId pregunta) idPregunta) ;Se verifica si es la pregunta a la que se le quiere agregar la recompensa
           (list (QuestionGetPregunta pregunta) (QuestionGetId pregunta)(QuestionGetUser pregunta) (QuestionGetDate pregunta) (QuestionGetLabels pregunta) "cerrada" 0 (QuestionGetVotes pregunta)) ;Se cierra y descuenta la recompensa
           pregunta)) ;Si no lo es se mantiene el stack y se verifica la siguiente posicion
     stackPreguntas)
    )
  )

;descripción: Funcion que cambia el estado de una respuesta a aceptada y fija en cero la recompensa
;dom: lista X entero
;rec: lista
(define marcarRespuestaAceptada
  (lambda (stackRespuestas idRespuesta)
    (map (lambda (respuesta)
       (if (equal? (answerGetId respuesta) idRespuesta) ;Se verifica si es la respuesta a la que se le quiere agregar la recompensa
           (list (answerGetRespuesta respuesta) (answerGetIdPregunta respuesta) (answerGetId respuesta) (answerGetUser respuesta) (answerGetDate respuesta) (answerGetLabels respuesta) "aceptada" (answerGetVotes respuesta)) ;Se cierra la respuesta
           respuesta)) ;Si no lo es se mantiene el stack y se verifica la siguiente posicion
     stackRespuestas)
    )
  )

;descripción: Funcion que le agrega 2 puntos de reputacion al usuario que acepta la pregunta
;dom: lista X string
;rec: lista
(define agregarReputacionUsuarioAceptante
  (lambda (stackUsuarios username)
    (map (lambda (usuario)
       (if (equal? (userGetUsername usuario) username) ;Se verifica si es el usuario al que se le quiere agregar la reputacion
           (list (userGetUsername usuario) (userGetPassword usuario) (userGetQuestions usuario) (+ (userGetReputation usuario) 2)) ;Se le agrega la reputacion
           usuario)) ;Si no lo es se mantiene el stack y se verifica la siguiente posicion
     stackUsuarios)
    )
  )

;descripción: Funcion que retorna el usuario que hizo cierta respuesta
;dom: lista X string
;rec: string
(define buscarUserIdRespuesta
  (lambda (stackRespuestas idRespuesta)
    (if (null? stackRespuestas) ;Caso base alcanzado a llegar al final de la lista o si esta se encuentra vacia
        #f ;No esta la respuesta buscada
        (if (equal? (answerGetId (car stackRespuestas)) idRespuesta) ;Si se encuentra la respuesta buscada
            (answerGetUser(car stackRespuestas));Si se encuentra la respuesta buscada retorna el username que hizo la respuesta
            (buscarUserIdRespuesta (cdr stackRespuestas) idRespuesta) ;Descomposicion recursiva, se pasara a revisar la siguiente respuesta
            )
        )
    )
  )

;descripción: Funcion que retorna la recompensa del id de pregunta ingresado
;dom: lista X string X entero
;rec: entero
(define buscarRecompensaPregunta
  (lambda (stackPreguntas idPregunta)
    (if (null? stackPreguntas) ;Caso base alcanzado a llegar al final de la lista o si esta se encuentra vacia
        #f ;No esta la pregunta buscada
        (if (equal? (QuestionGetId (car stackPreguntas)) idPregunta) ;Si se encuentra la respuesta buscada
            (QuestionGetRecompensa (car stackPreguntas));Si se encuentra la pregunta buscada retorna su recompensa
            (buscarRecompensaPregunta (cdr stackPreguntas) idPregunta) ;Descomposicion recursiva, se pasara a revisar la siguiente pregunta
            )
        )
    )
  )

;descripción: Funcion que le agrega la recompensa por una pregunta a un usuario por responder bien una pregunta
;dom: lista X string X entero
;rec: lista
(define otorgarRecompensaUsuarioAceptado
  (lambda (stackUsuarios username recompensa)
    (map (lambda (usuario)
           (if (equal? (userGetUsername usuario) username) ;Se verifica si es el usuario al que se le quiere agregar la reputacion
               (list (userGetUsername usuario) (userGetPassword usuario) (userGetQuestions usuario) (+ (userGetReputation usuario) recompensa)) ;Se le agrega la reputacion
               usuario)) ;Si no lo es se mantiene el stack y se verifica la siguiente posicion
         stackUsuarios)
    )
  )
  

;descripción: Funcion que retorna un Stack actualizado, distribuyendo reputacion segun corresponde y cerrando la pregunta aceptada
;dom: lista X lista X lista X string X entero X entero
;rec: lista
(define distribuirReputacionYCerrarPregunta
  (lambda (stackUsuarios stackPreguntas stackRespuestas username idPregunta idRespuesta)
    (list (cerrarPreguntaYEliminarRecompensa stackPreguntas idPregunta) (marcarRespuestaAceptada stackRespuestas idRespuesta) (otorgarRecompensaUsuarioAceptado (agregarReputacionUsuarioAceptante stackUsuarios username) (buscarUserIdRespuesta stackRespuestas idRespuesta) (buscarRecompensaPregunta stackPreguntas idPregunta)) (list))
    )
  )


;descripción: Funcion que transforma una lista de respuesta a una lista de strings
;dom: lista
;rec: lista
(define crearStringRespuestaAPregunta
  (lambda (respuestasAPregunta)
    (map (lambda (respuesta)
           (string-append "\n        Respuesta:" "  Id: " (number->string (answerGetId respuesta))
                          "  Fecha de respuesta: " (number->string(getDay(answerGetDate respuesta))) "/" (number->string(getMonth(answerGetDate respuesta))) "/" (number->string(getYear(answerGetDate respuesta))) 
                          "  Votos: " (number->string(answerGetVotes respuesta))
                          "  Estado: " (answerGetStatus respuesta) "\n"
                          "         " (answerGetRespuesta respuesta) "\n"
                          "         Etiquetas: " (string-join (answerGetLabels respuesta)" ") "   Autor: " (answerGetUser respuesta) "\n" 
                          "\n"))
     respuestasAPregunta)
    )
  )


;descripción: Funcion que genera una lista compuesta de strings la cual contiene todas las preguntas con sus respectivas respuestas, sera usada por stack->string.
;dom: lista X stack
;rec: lista
(define preguntas->string
  (lambda (stackPreguntas stack)
    (map (lambda (pregunta)
           (string-append "Pregunta:" "  Id: " (number->string (QuestionGetId pregunta))
                          "   Fecha de publicacion: " (number->string(getDay(QuestionGetDate pregunta))) "/" (number->string(getMonth(QuestionGetDate pregunta))) "/" (number->string(getYear(QuestionGetDate pregunta)))
                          "   Recompensa: " (number->string(QuestionGetRecompensa pregunta))
                          "   Votos: " (number->string(QuestionGetVotes pregunta))
                          "   Estado: " (QuestionGetStatus pregunta) "\n"
                          "    " (QuestionGetPregunta pregunta) "\n"
                          "    Etiquetas: " (string-join (QuestionGetLabels pregunta)" ") "   Autor: " (QuestionGetUser pregunta) "\n" 
                          (string-join (crearStringRespuestaAPregunta (getAnswersToAQuestion stack (QuestionGetId pregunta))) " ") ;Transforma las respuesta a un string y los agrega
                          "\n"))
                          
     stackPreguntas)
    )
  )

;descripción: Funcion que transforma una lista de enteros a una lista de strings, sera usada para transformar a string las preguntas hechas por un usuario
;dom: lista
;rec: lista
(define listaPreguntas->string
  (lambda (listaPreguntasUsuario)
    (map (lambda (idPregunta)
           (number->string idPregunta))
         listaPreguntasUsuario)
    )
  )
           
;descripción: Funcion que transforma una lista de tda usuario a una lista de strings que contiene su informacion
;dom: lista
;rec: lista
(define usuarios->string
  (lambda (stackUsuarios)
    (map (lambda (usuario)
           (string-append "    Usuario: \n"
                          "        Username: " (userGetUsername usuario) "  Reputación: " (number->string(userGetReputation usuario)) " Preguntas realizadas(Id): " (string-join (listaPreguntas->string (userGetQuestions usuario)) " ") 
                          "\n"))
     stackUsuarios)
    )
  )

;descripción: Funcion que retorna la lista de todas las respuestas a una determinada pregunta
;dom: stack x entero
;rec: lista
(define getAnswersToAQuestion
  (lambda (stack idPregunta)
    (filter (lambda (e)
                (equal? (answerGetIdPregunta e) idPregunta))
              (stackGetAnswers stack))
    )
  )
        
;descripción: Funcion que retorna la pregunta a la que le corresponde su id, sera implementada para ser usada con la funcion vote
;dom: stack X entero
;rec: lista
(define getQuestion
  (lambda (stack)
    (lambda (idPregunta)
      (filter (lambda (e)
                (equal? (QuestionGetId e) idPregunta))
              (stackGetQuestions stack))
      )
    )
  )

;descripción: Funcion que retorna la respuesta a la que le corresponde su id, sera implementada para ser usada con la funcion vote
;dom: enterp X entero X stack
;rec: lista
(define getAnswer
  (lambda (idPregunta)
    (lambda (stack)
      (lambda (idRespuesta)
        (filter (lambda (e)
                (and (equal? (answerGetIdPregunta e) idPregunta) (equal? (answerGetId e) idRespuesta)))
              (stackGetAnswers stack))
        )
      )
    )
  )




