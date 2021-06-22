# Scheme

1.- Implemente la función recursiva fibo (Fibonacci) que se define como se muestra a continuación. La función no está definida para valores negativos de n. NO pruebe la función para valores grandes ya que crece muy deprisa.

F( 0 ) = 0
F( 1 ) = 1
F( n ) = F(n-1) + F(n-2) si n > 1

Ejemplo:

> (fibo 7)
13

2.- Escriba una función llamada expo para calcular el exponencial (bn) utilizando la definición recursiva que aparece a continuación. La función no está definida para valores negativos de n. No pruebe la función para valores grandes ya que los valores resultantes pueden crecer muy deprisa.

b0 = 1
bn = b * bn-1 (n >= 1)

Ejemplo:

> (expo 2 3)
8

Uso de listas:
3.- Escriba una función denominada minimo que, dada una lista de números enteros, devuelva el número menor de la lista.

Ejemplo:

> (minimo '(7 6 3 2 9 1))
1

4.- Escriba una función denominada inserta, la cual debe insertar un número dado en una lista numérica simple (ordenada de forma ascendente), dada como segundo argumento. La lista debe seguir ordenada tras la inserción. Ejemplo:

Ejemplo:

> (inserta 55 '(5 6 30 60 90))
(list 5 6 30 55 60 90)

5.- Defina una función concatena que dadas dos listas (que pueden contener a su vez otras listas) devuelva otra lista resultado de la concatenación de las dos pasadas como parámetros. No puede utilizarse la función de Scheme append.

Ejemplo:

> (concatena '(a b c) '(a (b c) d e))
(list 'a 'b 'c 'a (list 'b 'c) 'd 'e)

6.- Utilizando la función anterior, defina una función invierte que dada una lista (que puede contener a su vez otras listas) devuelva dicha lista con sus elementos en orden inverso. NO se puede utilizar la función de Scheme reverse.

Ejemplo:

> (invierte '(a (b c) d (e f)))
(list (list 'e 'f) 'd (list 'b 'c) 'a)

7.- Utilizando la función de Scheme equal?, defina una función elimina que dado un elemento (átomo o lista) y una lista, devuelva dicha lista en la cual se han eliminado todas las apariciones del elemento dado.

Ejemplos:

> (elimina 'a '(f e d a h a d a))
(list 'f 'e 'd 'h 'd)

> (elimina '(a b) '(a b (a b) c d (c d)))
(list 'a 'b 'c 'd (list 'c 'd))

8.- Escriba una función repetidos que recibe como único argumento una lista y retorna otra lista en que se han eliminado los elementos repetidos de la lista de entrada (dejando únicamente la última aparición). Puede utilizar para ello una función auxiliar.

Ejemplo:

> (repetidos '(a b c d a f g d c c h))
(list 'b 'a 'f 'g 'd 'c 'h)

