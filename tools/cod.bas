DIM AS STRING tecla
DIM AS INTEGER a,b,file
CLS
SCREEN 13
file = FREEFILE
OPEN "c:\codigos.txt" FOR OUTPUT AS #file
DO
  DO
    a = SLEEP(100)
    print "."
  LOOP WHILE a = 0
  DO
    tecla = INKEY$
    a = ASC(LEFT$(tecla, 1))
    b = ASC(RIGHT$(tecla, 1))
    IF a = b THEN b = 0
    PRINT " asc1= "; a; " asc2= "; b
    PRINT #file, " caracter= "; tecla; "  asc1= ", a, " asc2= ", b
  LOOP UNTIL INKEY$="" OR a = 27
LOOP UNTIL ASC(RIGHT$(tecla, 1)) = 27
CLOSE #file

