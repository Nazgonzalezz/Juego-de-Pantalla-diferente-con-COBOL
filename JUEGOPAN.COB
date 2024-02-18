      ******************************************************************
      * Author:GONZALEZ NAZARENA ARACELI
      * Date:02/2024
      * Purpose: Juego de pantalla diferente cobol
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JUEGOPAN.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  opcion            PIC 9.

       01  indices.
           05 i              PIC 99.
           05 j              PIC 99.

       01  TITULO            PIC X(55) VALUE
           "DIRECCION   DER:1   IZQ: 2   ARRIB:3   ABAJ:4   SALIR:9".


       01  MAT-1 OCCURS 30 TIMES.
           05 MAT-2 OCCURS 80 TIMES.
               10 MAT-IND    PIC X VALUE SPACE.


       01  MOVIMIENTO.
           05 EJEy           PIC 99.
           05 EJEx           PIC 99.

       01  ANTIGUO-LUGAR.
           05 VIEJOEJEy           PIC 99.
           05 VIEJOEJEx           PIC 99.

       PROCEDURE DIVISION.
      */////////////////////////////////////////////////////////////////
       MAIN-PROCEDURE.
           PERFORM 1000-INICIALIZAR-MATRIZ
           PERFORM 2000-MOSTRAR-MATRIZ
           DISPLAY TITULO
           ACCEPT opcion

           PERFORM UNTIL opcion EQUAL 9
               EVALUATE opcion
               WHEN 1
                   COMPUTE EJEx = EJEx + 1
                   PERFORM 2200-MODIFICAR-MATRIZ
                   PERFORM 2300-VER-SI-LLEGA-AL-PARENT
                   DISPLAY TITULO
                   ACCEPT opcion
               WHEN 2
                   COMPUTE EJEx = EJEx - 1
                   PERFORM 2200-MODIFICAR-MATRIZ
                   PERFORM 2300-VER-SI-LLEGA-AL-PARENT
                   DISPLAY TITULO
                   ACCEPT opcion
               WHEN 3
                   COMPUTE EJEy = EJEy -  1
                   PERFORM 2200-MODIFICAR-MATRIZ
                   PERFORM 2300-VER-SI-LLEGA-AL-PARENT
                   DISPLAY TITULO
                   ACCEPT opcion
               WHEN 4
                   COMPUTE EJEy = EJEy +  1
                   PERFORM 2200-MODIFICAR-MATRIZ
                   PERFORM 2300-VER-SI-LLEGA-AL-PARENT
                   DISPLAY TITULO
                   ACCEPT opcion
               WHEN OTHER
                   DISPLAY "ERROR"
                   MOVE 9 TO opcion
           END-PERFORM

           STOP RUN.
      */////////////////////////////////////////////////////////////////

       1000-INICIALIZAR-MATRIZ.
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

       2000-MOSTRAR-MATRIZ.
           MOVE 0 TO J
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 30
                DISPLAY MAT-1(j)
           END-PERFORM.

      *-----------------------------------------------------------------

       2200-MODIFICAR-MATRIZ.
           MOVE "X" TO MAT-IND(EJEy,EJEx)
           MOVE SPACE TO MAT-IND(VIEJOEJEy,VIEJOEJEx)
           MOVE MOVIMIENTO TO ANTIGUO-LUGAR
           
           MOVE "]" TO MAT-IND(5,14)
           MOVE "[" TO MAT-IND(5,60)

           MOVE "]" TO MAT-IND(15,40)
           MOVE "[" TO MAT-IND(15,70)

           MOVE "]" TO MAT-IND(25,10)
           MOVE "[" TO MAT-IND(25,40).

      *-----------------------------------------------------------------

       2300-VER-SI-LLEGA-AL-PARENT.
           IF EJEy = 5 and EJEx = 14 and opcion = 1 THEN
               MOVE 61 TO EJEx
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF

           IF EJEy = 15 and EJEx = 40 and opcion = 1 THEN
               MOVE 71 TO EJEx
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF

           IF EJEy = 25 and EJEx = 10 and opcion = 1 THEN
           MOVE 41 TO EJEx
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF

            IF EJEy = 5 and EJEx = 60 and opcion = 2 THEN
               MOVE 13 TO EJEx
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF

           IF EJEy = 15 and EJEx = 70 and opcion = 2 THEN
               MOVE 39 TO EJEx
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF

           IF EJEy = 25 and EJEx = 40 and opcion = 2 THEN
               MOVE 9 TO EJEx
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
           
      *-------------------------------     
           IF EJEy = 5 AND EJEx = 14 AND opcion = 4 THEN
               MOVE 6 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
           
           IF EJEy = 5 AND EJEx = 14 AND opcion = 3 THEN
               MOVE 4 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
      *-------------------------------     
           IF EJEy = 5 AND EJEx = 60 AND opcion = 4 THEN
               MOVE 6 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
           
           IF EJEy = 5 AND EJEx = 60 AND opcion = 3 THEN
               MOVE 4 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
      *-------------------------------     
           IF EJEy = 15 AND EJEx = 40 AND opcion = 4 THEN
               MOVE 16 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
           
           IF EJEy = 15 AND EJEx = 40 AND opcion = 3 THEN
               MOVE 14 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
      
      *-------------------------------     
           IF EJEy = 15 AND EJEx = 70 AND opcion = 4 THEN
               MOVE 16 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
           
           IF EJEy = 15 AND EJEx = 70 AND opcion = 3 THEN
               MOVE 14 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
           
      *-------------------------------     
           IF EJEy = 25 AND EJEx = 10 AND opcion = 4 THEN
               MOVE 26 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
           
           IF EJEy = 25 AND EJEx = 10 AND opcion = 3 THEN
               MOVE 24 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
           
      *-------------------------------     
           IF EJEy = 25 AND EJEx = 40 AND opcion = 4 THEN
               MOVE 26 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
           
           IF EJEy = 25 AND EJEx = 40 AND opcion = 3 THEN
               MOVE 24 to EJEy
               PERFORM 2200-MODIFICAR-MATRIZ
           END-IF
      *-------------------------------- 
           PERFORM 2000-MOSTRAR-MATRIZ.

      *-----------------------------------------------------------------
       END PROGRAM JUEGOPAN.
