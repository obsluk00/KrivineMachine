**Seminar:** Struktur und Interpretation von Computerprogrammen - Grundlegende Programmierkonzepte anhand der Sprache LISP, Frühjahrsemester 2021  
**Dozenten:** Prof. Dr. Christian Tschudin, Dr. Marcel Lüthi  
**Datum:** 5. September 2021  

# Eine Krivine-Maschine in Scheme  

Luka Obser <luka.obser@unibas.ch>  
Reto Krummenacher <reto.krummenacher@unibas.ch>

## Jupyter-Notebook
Die gesamte Arbeit ist als Jupyter-Notebook verfügbar: [KrivineMaschine.ipynb](KrivineMaschine.ipynb).
Darin enthalten sind die Erklärungen zur Theorie sowie die Codes. 
Um Diese in Jupyter auszuführen, wird der Racket Kernel für Jupyter *IRacket* benötigt.
Eine Installationsanleitung findet sich unter [https://docs.racket-lang.org/iracket/index.html](https://docs.racket-lang.org/iracket/index.html).

## DrRacket
Aufgrund der Tatsache, dass der Kernel für Racket nicht standardmässig in Jupyter-Notebooks vorhanden ist, wird
der Code ebenfalls im ".rkt" Format zur verfügung gestellt. Dieser kann mit [DrRacket](https://racket-lang.org/) ausgeführt werden.
Die Routinen finden sich im Ordner ['racket'](racket) zusammen mit einige Tests der einzelnen [Datenstrukturen](racket\DataStructures.rkt).
Die Krivine-Maschine ist in [Execution.rkt](racket\Execution.rkt) implementiert und kann nach dem Run (Ctrl+R) 
mittels Befehle `(krivine-machine (compile (parse '((λx.x x)(λx.x))) 0 '()))` aus der Konsole ausgeführt werden.