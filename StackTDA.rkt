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
            (verificarUserPassword (cdr listaUsuarios) username) ;Descomposicion recursiva, se pasara a revisar al siguiente usuario
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


