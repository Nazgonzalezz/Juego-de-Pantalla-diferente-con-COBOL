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
       77  opcion                 PIC X.

       01  indices.
           05 i                   PIC 99.
           05 j                   PIC 99.

       01  TITULO                 PIC X(55) VALUE
           "DIRECCION   DER:D   IZQ:A   ARRIB:W   ABAJ:S   SALIR:9".


       01  MAT-1 OCCURS 30 TIMES.
           05 MAT-2 OCCURS 80 TIMES.
               10 MAT-IND         PIC X VALUE SPACE.


       01  MOVIMIENTO.
           05 EJEy                PIC 99.
           05 EJEx                PIC 99.

       01  ANTIGUO-LUGAR.
           05 VIEJOEJEy           PIC 99.
           05 VIEJOEJEx           PIC 99.

       PROCEDURE DIVISION.
      */////////////////////////////////////////////////////////////////
       MAIN-PROCEDURE.
           PERFORM 1000-INICIALIZAR-MAPA
           PERFORM 2000-MOSTRAR-MAPA
           PERFORM 3000-RECIBIR-MOVIMIENTO
           
           PERFORM UNTIL opcion EQUAL "9"
               EVALUATE opcion
               WHEN "D"
                   COMPUTE EJEx = EJEx + 1
                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   PERFORM 5000-TRASPASAR-PUERTA
                   PERFORM 3000-RECIBIR-MOVIMIENTO
               WHEN "A"
                   COMPUTE EJEx = EJEx - 1
                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   PERFORM 5000-TRASPASAR-PUERTA
                   PERFORM 3000-RECIBIR-MOVIMIENTO
               WHEN "W"
                   COMPUTE EJEy = EJEy -  1
                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   PERFORM 5000-TRASPASAR-PUERTA
                   PERFORM 3000-RECIBIR-MOVIMIENTO
               WHEN "S"
                   COMPUTE EJEy = EJEy +  1
                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   PERFORM 5000-TRASPASAR-PUERTA
                   PERFORM 3000-RECIBIR-MOVIMIENTO
               WHEN OTHER
                   PERFORM 2000-MOSTRAR-MAPA
                   PERFORM 3000-RECIBIR-MOVIMIENTO
               END-EVALUATE
           END-PERFORM
           STOP RUN.
      */////////////////////////////////////////////////////////////////

       1000-INICIALIZAR-MAPA.
           MOVE "]" TO MAT-IND(5,14)
           MOVE "[" TO MAT-IND(5,60)

           MOVE "]" TO MAT-IND(15,40)
           MOVE "[" TO MAT-IND(15,70)

           MOVE "]" TO MAT-IND(25,10)
           MOVE "[" TO MAT-IND(25,40)

           MOVE 2 TO EJEy
           MOVE 2 TO EJEx
           MOVE "X" TO MAT-IND(EJEy,EJEx)

           MOVE 2 TO VIEJOEJEy
           MOVE 2 TO VIEJOEJEx

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
           MOVE 0 TO J
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 30
                DISPLAY MAT-1(j)
           END-PERFORM.
               
      *-----------------------------------------------------------------         

       3000-RECIBIR-MOVIMIENTO.
           DISPLAY TITULO
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
           MOVE "X" TO MAT-IND(EJEy,EJEx)
           MOVE SPACE TO MAT-IND(VIEJOEJEy,VIEJOEJEx)
           MOVE MOVIMIENTO TO ANTIGUO-LUGAR.

      *-----------------------------------------------------------------

       5000-TRASPASAR-PUERTA.
           EVALUATE EJEy
           WHEN 5
               EVALUATE EJEx
               WHEN 14
                   IF opcion equal "D"
                       MOVE 61 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   ELSE
                      IF opcion equal "A"
                          MOVE 13 TO EJEx
                           PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                        ELSE
                          IF  opcion equal "W"
                               MOVE 4 TO EJEy
                               PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                           ELSE
                               IF OPCION EQUAL "S"
                                   MOVE 6 TO EJEy
                                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   MOVE "]" TO MAT-IND(5,14)
               WHEN 60
                   IF opcion equal "D"
                       MOVE 61 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   ELSE
                      IF opcion equal "A"
                          MOVE 13 TO EJEx
                           PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                        ELSE
                          IF  opcion equal "W"
                               MOVE 4 TO EJEy
                               PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                           ELSE
                               IF OPCION EQUAL "S"
                                   MOVE 6 TO EJEy
                                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   MOVE "[" TO MAT-IND(5,60)
               END-EVALUATE
           WHEN 15
               EVALUATE EJEx
               WHEN 40
                   IF opcion equal "D"
                       MOVE 71 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   ELSE
                      IF opcion equal "A"
                          MOVE 15 TO EJEx
                           PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                        ELSE
                          IF  opcion equal "W"
                               MOVE 14 TO EJEy
                               PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                           ELSE
                               IF OPCION EQUAL "S"
                                   MOVE 16 TO EJEy
                                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   MOVE "]" TO MAT-IND(15,40)
               WHEN 70
                   IF opcion equal "D"
                       MOVE 71 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   ELSE
                      IF opcion equal "A"
                          MOVE 39 TO EJEx
                           PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                        ELSE
                          IF  opcion equal "W"
                               MOVE 14 TO EJEy
                               PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                           ELSE
                               IF OPCION EQUAL "S"
                                   MOVE 16 TO EJEy
                                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   MOVE "[" TO MAT-IND(15,70)
               END-EVALUATE
           WHEN 25
               EVALUATE EJEx
               WHEN 10
                   IF opcion equal "D"
                       MOVE 41 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   ELSE
                      IF opcion equal "A"
                          MOVE 9 TO EJEx
                           PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                        ELSE
                          IF  opcion equal "W"
                               MOVE 24 TO EJEy
                               PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                           ELSE
                               IF OPCION EQUAL "S"
                                   MOVE 26 TO EJEy
                                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   MOVE "]" TO MAT-IND(25,10)
               WHEN 40
                   IF opcion equal "D"
                       MOVE 41 TO EJEx
                       PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                   ELSE
                      IF opcion equal "A"
                          MOVE 9 TO EJEx
                           PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                        ELSE
                          IF  opcion equal "W"
                               MOVE 24 TO EJEy
                               PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                           ELSE
                               IF OPCION EQUAL "S"
                                   MOVE 26 TO EJEy
                                   PERFORM 4000-MOVIMIENTO-DE-PERSONAJE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                   MOVE "[" TO MAT-IND(25,40)
               END-EVALUATE
           END-EVALUATE

           PERFORM 2000-MOSTRAR-MAPA.

      *-----------------------------------------------------------------
       END PROGRAM JUEGOPAN.
