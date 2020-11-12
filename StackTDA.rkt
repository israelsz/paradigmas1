#lang scheme
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;descripción: Funcion que retorna la lista con Nombres de usuarios registrados.
;dom: list
;rec: list
(define(selecListaUsuariosRegistrados stack)
  (list-ref(list-ref stack 2)0)
  )

;descripción: Funcion que retorna la lista con Contraseñas de los usuarios registrados.
;dom: list
;rec: list
(define(selecListaPasswords stack)
  (list-ref(list-ref stack 2)1)
  )


;Modificadores

;descripción: Agrega una pregunta al stack
;dom: list X elemento
;rec: list
;(define (addQuestion stack x)
;  (list(modificarPos0 stack x)(selecListaRespuestas stack)(selecListaUsuarios stack)(selecListaUsuariosActivos stack)
;       )
;  )

;descripción: Agrega una respuesta al stack
;dom: list X elemento
;rec: list
;(define (addAnswer stack x)
;  (list(selecListaPreguntas stack)(modificarPos1 stack x)(selecListaUsuarios stack)(selecListaUsuariosActivos stack)
;       )
;  )

;descripción: Agrega un usuario y su contraseña al stack
;dom: list X string x string
;rec: list
;(define(addUserPassword stack nombre password)
;  (list(selecListaPreguntas stack)(selecListaRespuestas stack)(list(modificarPos2_0 stack nombre)(modificarPos2_1 stack password))(selecListaUsuariosActivos stack)
;       )
;  )

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
            (verificarUserPassword (cdr listaUsuarios) username) ;Descomposicion recursiva, se pasara a revisar al siguiente usuario
            )
        )
    )
  )
            




;LAS SIGUIENTES FUNCIONES SON COMPLEMENTARIAS/AUXILIARES AL TDA. NO FORMAN PARTE DEL TDA STACK, PERO
;EL TDA FECHA LAS EMPLEA PARA PODER REALIZAR SU TRABAJO. ESTAS FUNCIONES DE IGUAL FORMA PUEDEN
;SER EMPLEADAS INDEPENDIENTEMENTE DE LA EXISTENCIA DEL TDA STACK, POR LO QUE NO EXISTE ACOPLAMIENTO CON EL TDA.
;POR OTRO LADO, EL TDA SEGUN LA IMPLEMENTACION REALIZADA A CONTINUACION, ESTA ACOPLADO A ESTAS FUNCIONES.


;descripción: Agrega un elemento a la posicion de Preguntas del stack
;dom: list X elemento
;rec: list
;(define (modificarPos0 stack x)
;  (append(selecListaPreguntas stack)(list x)
;         )
;  )

;descripción: Agrega un elemento a la posicion de Respuestas del stack
;dom: list X elemento
;rec: list
;(define (modificarPos1 stack x)
;  (append(selecListaRespuestas stack)(list x)
;         )
;  )

;descripción: Agrega un elemento a la posicion de usuarios del stack
;dom: list X elemento
;rec: list
;(define(modificarPos2_0 stack nombre)
;  (append(selecListaUsuariosRegistrados stack)(list nombre)
;         )
;  )

;descripción: Agrega un elemento a la posicion de contraseñas del stack
;dom: list X elemento
;rec: list
;(define(modificarPos2_1 stack password)
;  (append(selecListaPasswords stack)(list password)
;         )
;  )