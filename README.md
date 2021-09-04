**Seminar:** Struktur und Interpretation von Computerprogrammen - Grundlegende Programmierkonzepte anhand der Sprache LISP, Frühjahrsemester 2021  
**Dozenten:** Prof. Dr. Christian Tschudin, Dr. Marcel Lüthi  
**Betreuer:** Prof. Dr. Christian Tschudin  
**Datum:** 5. September 2021  

# Eine Krivine-Maschine in Scheme  

Luka Obser <luka.obser@unibas.ch>  
Reto Krummenacher <reto.krummenacher@unibas.ch>

## Inhalt
Thema dieser Arbeit war das Implementieren einer Krivine-Maschine in Scheme. Als Endergebnis wurde eine Jupyter-Notebook
gewählt. Damit können Erklärungen zur Theorie sowie der geschriebenen Code kompakt untereinander dargestellt werden.
Zudem bietet Jupyter die Möglichkeit, die Code-Fragemente direkt auszuführen.

## Eingeriechte Materialen

### 1. Jupyter-Notebook
Als Ergebnis unserer Arbeit haben wir eine Jupyter-Notebook erstellt: [KrivineMaschine.ipynb](KrivineMaschine.ipynb).  
Darin enthalten sind die Erklärungen zur Theorie sowie die Codes. 
Um dieses Notebook in Jupyter auszuführen, wird der Racket Kernel *IRacket* benötigt.
Eine Installationsanleitung findet sich unter [https://docs.racket-lang.org/iracket/index.html](https://docs.racket-lang.org/iracket/index.html).

### 2. Code im DrRacket Format
Aufgrund der Tatsache, dass *IRacket* nicht standardmässig in Jupyter-Notebooks vorhanden ist, wird
der Code ebenfalls als ".rkt" Dateien zur verfügung gestellt. Dieser kann mit [DrRacket](https://racket-lang.org/) ausgeführt werden.
Die Routinen finden sich im Ordner ['racket'](racket) zusammen mit den benötigten Datenstrukturen sowie die dazugehörenden Tests.  
Die Krivine-Maschine ist in [Execution.rkt](racket\Execution.rkt) implementiert und kann nach dem Run (Ctrl+R) 
mittels  
   `(krivine-machine (compile (parse '((λx.x x)(λx.x))) 0 '()))`  
aus der DrRacket Konsole ausgeführt werden.