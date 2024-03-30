      ******************************************************************
      * Author:GONZALEZ NAZARENA ARACELI; TRONCOSO LEANDRO
      * Date:02/2024
      * Purpose: Juego de pantalla diferente cobol
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JUEGOPAN.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  opcion                  PIC X.

       01  indices.
           05 i                    PIC 99.
           05 j                    PIC 99.

       01  MAT-1 OCCURS 30 TIMES.
           05 MAT-2 OCCURS 80 TIMES.
               10 MAT-IND          PIC X VALUE SPACE.

       01  MOVIMIENTO.
           05 EJEy                 PIC 99.
           05 EJEx                 PIC 99.

       01  ANTIGUO-LUGAR.
           05 VIEJOEJEy            PIC 99.
           05 VIEJOEJEx            PIC 99.

      *DISTANCIA QUE TENDRA EL ENEMIGO DEL PERSONAJE.
       01  WS-DISTANCIA.
           05 WS-DISTANCIA-X       PIC S99.
           05 WS-DISTANCIA-Y       PIC S99.

      *DESPLAZAMIENTO DEL ENEMIGO.
       01  WS-ENEMY-MOVIMIENTO.
           05 ENEMY-EJEy           PIC 99.
           05 ENEMY-EJEx           PIC 99.

       01  WS-ENEMY-OLDLUGAR.
           05 ENEMY-VIEJOEJEy      PIC 99.
           05 ENEMY-VIEJOEJEx      PIC 99.

      *Cabecera del juego.
       01  WS-HEADER.
           05 FILLER               PIC X VALUE '*'.
           05 FILLER               PIC XX VALUE SPACES.
           05 FILLER               PIC X(9) VALUE 'JUEGO PAN'.
           05 FILLER               PIC XX VALUE SPACES.
           05 FILLER               PIC X(20) VALUE ALL '*'.
           05 WS-H-MENSAJE         PIC X(10) VALUE ALL '*'.
           05 FILLER               PIC X(20) VALUE ALL '*'.
           05 FILLER               PIC XX VALUE SPACES.
           05 FILLER               PIC X(8) VALUE 'PUNTAJE:'.
           05 WS-PUNTAJE           PIC 999.
           05 FILLER               PIC XX VALUE SPACES.
           05 FILLER               PIC X VALUE '*'.

      *Mensaje si acaba el juego
       01  WS-GAME-OVER.
           05 FILLER               PIC X(25) VALUE SPACES.
           05 WS-GO-MENSAJE        PIC X(10).
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(9) VALUE "PUNTAJE: ".
           05 WS-GO-PUNTAJE        PIC 999.

       77  WS-ASTERISCOS           PIC X(80) VALUE ALL '*'.

       01  WS-GUIONES.
           05 FILLER               PIC X(20) VALUE SPACES.
           05 WS-GUIONES-FULL      PIC X(40) VALUE ALL '='.
           05 FILLER               PIC X(20) VALUE SPACES.

      *Control visual de direcciones
       01  WS-DIRECCIONES.
           05 FILLER               PIC X VALUE '*'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(14) VALUE 'DIRECCIONES =>'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(9) VALUE 'DERECHA:D'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE 'IZQUIERDA:A'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(8) VALUE 'ARRIBA:W'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'ABAJO:S'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'SALIR:9'.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 FILLER               PIC X VALUE '*'.
      */////////////////////////////////////////////////////////////////
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM 1000-INICIALIZAR-MAPA
           PERFORM 2000-MOSTRAR-MAPA
           PERFORM 3000-RECIBIR-MOVIMIENTO

           PERFORM UNTIL opcion EQUAL "9"
               EVALUATE opcion
               WHEN "D"
                   IF EJEX NOT EQUAL 80
                       COMPUTE EJEx = EJEx + 1
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                       PERFORM 5000-TRASPASAR-PUERTA
                       PERFORM 8000-CAMBIO-POS-ENEMY
                       PERFORM 2000-MOSTRAR-MAPA
                       IF opcion not equal "9"
                           PERFORM 3000-RECIBIR-MOVIMIENTO
                       END-IF
                   ELSE
                       PERFORM 8000-CAMBIO-POS-ENEMY
                       PERFORM 6000-DEJAR-PERSONAJE-QUIETO
                  END-IF
               WHEN "A"
                   IF EJEX NOT EQUAL 1
                       COMPUTE EJEx = EJEx - 1
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                       PERFORM 5000-TRASPASAR-PUERTA
                       PERFORM 8000-CAMBIO-POS-ENEMY
                       PERFORM 2000-MOSTRAR-MAPA
                       IF opcion not equal "9"
                           PERFORM 3000-RECIBIR-MOVIMIENTO
                       END-IF
                   ELSE
                       PERFORM 8000-CAMBIO-POS-ENEMY
                       PERFORM 6000-DEJAR-PERSONAJE-QUIETO
                   END-IF
               WHEN "W"
                   IF EJEy NOT EQUAL 2
                       COMPUTE EJEy = EJEy -  1
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                       PERFORM 5000-TRASPASAR-PUERTA
                       PERFORM 8000-CAMBIO-POS-ENEMY
                       PERFORM 2000-MOSTRAR-MAPA
                       IF opcion not equal "9"
                           PERFORM 3000-RECIBIR-MOVIMIENTO
                       END-IF
                   ELSE
                       PERFORM 8000-CAMBIO-POS-ENEMY
                       PERFORM 6000-DEJAR-PERSONAJE-QUIETO
                   END-IF
               WHEN "S"
                   IF EJEy NOT EQUAL 29
                       COMPUTE EJEy = EJEy +  1
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                       PERFORM 5000-TRASPASAR-PUERTA
                       PERFORM 8000-CAMBIO-POS-ENEMY
                       PERFORM 2000-MOSTRAR-MAPA
                       IF opcion not equal "9"
                           PERFORM 3000-RECIBIR-MOVIMIENTO
                       END-IF

                   ELSE
                       PERFORM 8000-CAMBIO-POS-ENEMY
                       PERFORM 6000-DEJAR-PERSONAJE-QUIETO
                   END-IF
               WHEN OTHER
                   PERFORM 8000-CAMBIO-POS-ENEMY
                   PERFORM 6000-DEJAR-PERSONAJE-QUIETO
               END-EVALUATE
           END-PERFORM
           PERFORM 11000-GAME-OVER.

      */////////////////////////////////////////////////////////////////

       1000-INICIALIZAR-MAPA.

           MOVE "]" TO MAT-IND(5,14)
           MOVE "[" TO MAT-IND(5,60)

           MOVE "]" TO MAT-IND(15,40)
           MOVE "[" TO MAT-IND(15,70)

           MOVE "]" TO MAT-IND(25,10)
           MOVE "[" TO MAT-IND(25,40)

           MOVE 2 TO EJEy
           MOVE 3 TO EJEx
           MOVE "X" TO MAT-IND(EJEy,EJEx)

           MOVE 2 TO VIEJOEJEy
           MOVE 3 TO VIEJOEJEx

           MOVE 0 TO J
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 80
               MOVE "*" TO MAT-IND(1,j)
           END-PERFORM

           MOVE 0 TO J
           PERFORM VARYING j FROM 1 BY 1 UNTIL J > 80
               MOVE '*' TO MAT-IND(30,j)
           END-PERFORM.

      *-----------------------------------------------------------------

       2000-MOSTRAR-MAPA.
           DISPLAY WS-ASTERISCOS
           DISPLAY WS-HEADER
           MOVE 0 TO J
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 30
                DISPLAY MAT-1(j)
           END-PERFORM.

      *-----------------------------------------------------------------

       3000-RECIBIR-MOVIMIENTO.
           DISPLAY WS-DIRECCIONES
           DISPLAY WS-ASTERISCOS
           ACCEPT opcion
           EVALUATE opcion
               WHEN "a"
                   MOVE "A"             TO opcion
               WHEN "s"
                    MOVE "S"            TO opcion
               WHEN "d"
                    MOVE "D"            TO opcion
               WHEN "w"
                    MOVE "W"            TO opcion
           END-EVALUATE.

      *-----------------------------------------------------------------

       4000-MOVIMIENTO-DE-PERSONAJE.
           ADD 1 TO WS-PUNTAJE
           PERFORM 7000-VERIFICAR-SCORE
           MOVE "X" TO MAT-IND(EJEy,EJEx)
           MOVE SPACE TO MAT-IND(VIEJOEJEy,VIEJOEJEx)
           MOVE MOVIMIENTO TO ANTIGUO-LUGAR.

      *-----------------------------------------------------------------

       5000-TRASPASAR-PUERTA.
           EVALUATE EJEy
           WHEN 5
               EVALUATE EJEx
               WHEN 14
                   EVALUATE opcion
                   WHEN "D"
                       MOVE 61 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "A"
                       MOVE 13 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "W"
                       MOVE 4 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "S"
                       MOVE 6 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   END-EVALUATE
                   MOVE "]" TO MAT-IND(5,14)
               WHEN 60
                   EVALUATE opcion
                   WHEN "D"
                       MOVE 61 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "A"
                       MOVE 13 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "W"
                       MOVE 4 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "S"
                       MOVE 6 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   END-EVALUATE
                   MOVE "[" TO MAT-IND(5,60)
               END-EVALUATE
           WHEN 15
               EVALUATE EJEx
               WHEN 40
                   EVALUATE opcion
                   WHEN "D"
                       MOVE 71 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "A"
                       MOVE 39 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "W"
                       MOVE 14 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "S"
                       MOVE 16 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   END-EVALUATE
                   MOVE "]" TO MAT-IND(15,40)
               WHEN 70
                   EVALUATE opcion
                   WHEN "D"
                       MOVE 71 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "A"
                       MOVE 39 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "W"
                       MOVE 14 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "S"
                       MOVE 16 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   END-EVALUATE
                   MOVE "[" TO MAT-IND(15,70)
               END-EVALUATE
           WHEN 25
               EVALUATE EJEx
               WHEN 10
                   EVALUATE opcion
                   WHEN "D"
                       MOVE 41 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "A"
                       MOVE 9 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "W"
                       MOVE 24 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "S"
                       MOVE 26 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   END-EVALUATE
                   MOVE "]" TO MAT-IND(25,10)
               WHEN 40
                   EVALUATE opcion
                   WHEN "D"
                       MOVE 41 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "A"
                       MOVE 9 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "W"
                       MOVE 24 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   WHEN "S"
                       MOVE 26 TO EJEy
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   END-EVALUATE
                   MOVE "[" TO MAT-IND(25,41)
               END-EVALUATE
           END-EVALUATE.

      *-----------------------------------------------------------------

       6000-DEJAR-PERSONAJE-QUIETO.
           PERFORM 2000-MOSTRAR-MAPA
           IF opcion not equal "9"
               PERFORM 3000-RECIBIR-MOVIMIENTO
           END-IF.

      *-----------------------------------------------------------------
      *CONTROLAREMOS EL PUNTAJE QUE POSEE EL PERSONAJE A MEDIDA DE
      *CADA MOVIMIENTO. CUANDO LLEGUE A SCORE: 10 APARECERA EL ENEMIGO.

       7000-VERIFICAR-SCORE.
           PERFORM 10000-GENERAR-MENSAJE
           IF WS-PUNTAJE = 5 THEN
              MOVE 78 TO ENEMY-EJEx
              MOVE 29 TO ENEMY-EJEy
              MOVE "E" TO MAT-IND(ENEMY-EJEy,ENEMY-EJEx)
           END-IF
           IF WS-PUNTAJE >= 100 THEN
               PERFORM 11000-GAME-OVER
           END-IF.

      *-----------------------------------------------------------------
      *AQUI REALIZAMOS EL MOVIMIENTO VISUAL DEL ENEMIGO.

       8000-CAMBIO-POS-ENEMY.
           IF WS-PUNTAJE > 5 THEN
              MOVE ENEMY-EJEx TO ENEMY-VIEJOEJEx
              MOVE ENEMY-EJEy TO ENEMY-VIEJOEJEy
              PERFORM 9000-MOVIMIENTO-ENEMIGO
              PERFORM 9100-NO-TRANSPASAR-PUERTA
              MOVE SPACE TO MAT-IND(ENEMY-VIEJOEJEy,ENEMY-VIEJOEJEx)
              MOVE "E" TO MAT-IND(ENEMY-EJEy,ENEMY-EJEx)
           END-IF.

      *-----------------------------------------------------------------
      *AQUI INDICAMOS EL MOVIMIENTO QUE DEBE REALIZAR EL ENEMIGO
      *EN BASE A LA DISTANCIA ENTRE AMBOS

       9000-MOVIMIENTO-ENEMIGO.
           MOVE 0 TO WS-DISTANCIA-X
           MOVE 0 TO WS-DISTANCIA-Y
           COMPUTE WS-DISTANCIA-X = EJEx - ENEMY-EJEx
           COMPUTE WS-DISTANCIA-Y = EJEy - ENEMY-EJEy
      *VERIFICAMOS QUE SU POSICION NO COINCIDA HORIZONTAL NI
      *VERTICALMENTE CON LA DEL PERSONAJE
           IF WS-DISTANCIA-X NOT = 0 AND WS-DISTANCIA-Y NOT = 0
      *VERIFICAMOS SI EL ENEMIGO ESTA ABAJO O ARRIBA DEL PERSONAJE
               IF WS-DISTANCIA-Y < 0
                   SUBTRACT 1 FROM ENEMY-EJEy
               ELSE
                   ADD 1 TO ENEMY-EJEy
               END-IF
      *VERIFICAMOS SI EL ENEMIGO ESTA IZQUIERDA O DERECHA
               IF WS-DISTANCIA-X < 0
                   IF WS-DISTANCIA-X = -1 THEN
                       SUBTRACT 1 FROM ENEMY-EJEx
                   ELSE
                       SUBTRACT 2 FROM ENEMY-EJEx
                   END-IF
               ELSE
                   IF WS-DISTANCIA-X = 1 THEN
                       ADD 1 TO ENEMY-EJEx
                   ELSE
                       ADD 2 TO ENEMY-EJEx
                   END-IF
               END-IF
           END-IF
      *SI EL ENEMIGO SE ENCUENTRA A LA ALTURA DEL PERSONAJE
      *SE DESPLAZARA UNICAMENTE DE IZQUIERDA A DERECHA
           IF WS-DISTANCIA-Y = 0 AND WS-DISTANCIA-X NOT = 0
               IF WS-DISTANCIA-X >= 1 THEN
                   IF WS-DISTANCIA-X = 1 THEN
                       ADD 1 TO ENEMY-EJEx
                   ELSE
                       ADD 2 TO ENEMY-EJEx
                   END-IF
               ELSE
                   IF WS-DISTANCIA-X = -1 THEN
                       SUBTRACT 1 FROM ENEMY-EJEx
                   ELSE
                       SUBTRACT 2 FROM ENEMY-EJEx
                   END-IF
               END-IF
           END-IF
      *SI EL ENEMIGO SE ENCUENTRA HORIZONTALMENTE IGUAL QUE EL PERSONAJE
      *EL ENEMIGO SE MOVERA DE MANERA VERTICAL.
           IF WS-DISTANCIA-X = 0 AND WS-DISTANCIA-Y NOT = 0
               IF WS-DISTANCIA-Y >= 1 THEN
                   ADD 1 TO ENEMY-EJEy
               ELSE
                   SUBTRACT 1 FROM ENEMY-EJEy
               END-IF
           END-IF
      *AQUI VERIFICAMOS SI EL ENEMIGO ALCANZO AL JUGADOR, PERDERA
           IF EJEX = ENEMY-EJEx AND EJEy = ENEMY-EJEy THEN
               MOVE "9" TO opcion
           END-IF.

      *-----------------------------------------------------------------

       9100-NO-TRANSPASAR-PUERTA.
           EVALUATE ENEMY-EJEy
           WHEN 5
               EVALUATE ENEMY-EJEx
               WHEN 14
                  COMPUTE ENEMY-EJEx = ENEMY-EJEx + 1
                  MOVE "]" TO MAT-IND(5,14)
               WHEN 60
                  COMPUTE ENEMY-EJEx = ENEMY-EJEx + 1
                  MOVE "[" TO MAT-IND(5,60)
               END-EVALUATE
           WHEN 15
               EVALUATE ENEMY-EJEx
               WHEN 40
                  COMPUTE ENEMY-EJEx = ENEMY-EJEx + 1
                  MOVE "]" TO MAT-IND(15,40)
               WHEN 70
                  COMPUTE ENEMY-EJEx = ENEMY-EJEx + 1
                  MOVE "[" TO MAT-IND(15,70)
               END-EVALUATE
           WHEN 25
               EVALUATE ENEMY-EJEx
               WHEN 10
                  COMPUTE ENEMY-EJEx = ENEMY-EJEx + 1
                  MOVE "]" TO MAT-IND(25,10)
               WHEN 40
                  COMPUTE ENEMY-EJEx = ENEMY-EJEx + 1
                  MOVE "[" TO MAT-IND(25,40)
               END-EVALUATE
           END-EVALUATE.


      *-----------------------------------------------------------------
      *Genera un mensaje con el transcurso de la partida en la cabecera.
       
       10000-GENERAR-MENSAJE.
           EVALUATE TRUE
           WHEN WS-PUNTAJE = 11 or WS-PUNTAJE = 6 or WS-PUNTAJE = 35
               MOVE '  CORRE!  ' TO WS-H-MENSAJE
           WHEN WS-PUNTAJE = 30 or WS-PUNTAJE = 40 or WS-PUNTAJE = 15
               MOVE 'SIGUE ASI!' TO WS-H-MENSAJE
           WHEN WS-PUNTAJE = 60 or WS-PUNTAJE = 55 or WS-PUNTAJE = 80
               MOVE ' YA CASI!' TO WS-H-MENSAJE
           WHEN OTHER
               MOVE ALL '*' TO WS-H-MENSAJE
           END-EVALUATE.

      *-----------------------------------------------------------------

       11000-GAME-OVER.
           IF WS-PUNTAJE >= 100 THEN
               MOVE "Ha ganado" TO WS-GO-MENSAJE
           ELSE
               MOVE "Ha perdido" TO WS-GO-MENSAJE
           END-IF
           MOVE WS-PUNTAJE TO WS-GO-PUNTAJE
           DISPLAY WS-GUIONES
           DISPLAY WS-GAME-OVER
           DISPLAY WS-GUIONES
           STOP RUN.

      *-----------------------------------------------------------------
       END PROGRAM JUEGOPAN.
