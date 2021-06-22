(require errortrace) ;; Ofrece más información al producirse un error
(require racket/trace) ;; Permite seguir la ejecución de una función usando antes (trace nombre_de_función)

(define fibo
    (lambda (x)
    (if (= x 0)
        0
        (if (= x 1)
            1
            (when (> x 1)
                  (+ (fibo (- x 1)) (fibo (- x 2))))))))
    
(define expo
    (lambda (x y)
        (if (= y 0)
            1
            (when (>= y 1)
                (* x (expo x (- y 1)))))))

(define (minimo lista)
    (if (= (length lista) 1)
        (car lista)
        (if (< (car lista) (minimo (cdr lista)))
            (car lista)
            (minimo (cdr lista)))))

(define inserta
    (lambda (que lista)
        (if (null? lista)
            (list que)
            (if (> (car lista) que)
                (cons que lista)
                (cons (car lista) (inserta que (cdr lista)))))))
    
(define concatena
    (lambda (lista1 lista2)
        (if (null? lista1)
            lista2
            (if (not (null? (cdr lista1)))
                (cons (car lista1) (concatena (cdr lista1) lista2))
                (cons (car lista1) lista2)))))
    
(define invierte
    (lambda (lista)
        (if (null? lista)
            lista
            (concatena (invierte (cdr lista)) (list (car lista))))))

(define elimina
    (lambda (que lista)
        (if (null? lista)
            '()
            (if (equal? que (car lista))
                (elimina que (cdr lista))
                (cons (car lista) (elimina que (cdr lista)))))))
        
(define repetidos
    (lambda (lista)
        (if (null? lista)
            lista
            (if (eqv? (invierte (elimina (car (invierte lista)) (cdr (invierte lista)))) lista)
                lista
                (concatena (repetidos (invierte (elimina (car (invierte lista)) (cdr (invierte lista))))) (list (car (invierte lista))))))))
