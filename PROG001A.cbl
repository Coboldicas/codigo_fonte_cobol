      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 20/01/2025
      * OBJETIVO: PRIMEIRO PROGRAMA COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG001A.
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      *     YYYYMMDD
       01  WRK-DATA   PIC X(08) VALUE SPACES.

       01  WRK-DATA2  PIC X(10) VALUE SPACES.
       01  WRK-DATA3  REDEFINES WRK-DATA2.
         05 WRK-DATA3-DIA  PIC X(02).
         05 FILLER         PIC X(01).
         05 WRK-DATA3-MES  PIC X(02).
         05 FILLER         PIC X(01).
         05 WRK-DATA3-ANO  PIC X(04).

      *    TIME   20452199
       01  WRK-TIME  PIC X(12) VALUE SPACES.

       01  WRK-TIME2  PIC X(10) VALUE SPACES.
       01  WRK-TIME3  REDEFINES WRK-TIME2.
         05 WRK-TIME3-HOR  PIC X(02).
         05 FILLER         PIC X(01).
         05 WRK-TIME3-MIN  PIC X(02).
      *  05 FILLER         PIC X(01).
      *  05 WRK-TIME3-SEG  PIC X(02).

       01  WRK-NOME  PIC X(20) VALUE SPACES.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.


       0001-PROCESSAR.
            DISPLAY 'INICIO DO PROGRAMA'
            PERFORM 0002-MENSAGEM
            PERFORM 0003-NOME
            PERFORM 9999-FINALIZAR
            .
       0001-END.
      *----------------------------------------------------------------*
       0002-MENSAGEM.
            DISPLAY "0002-MENSAGEM"
            DISPLAY "BOA NOITE! SEJAM BEM VINDOS! **********************
      -    "*************"

            ACCEPT WRK-DATA FROM DATE YYYYMMDD

            ACCEPT WRK-TIME FROM TIME

      *     DISPLAY 'DATA: ' WRK-DATA
      *     YYYYMMDD
            MOVE WRK-DATA(1:4) TO WRK-DATA3-ANO
            MOVE WRK-DATA(5:2) TO WRK-DATA3-MES
            MOVE WRK-DATA(7:2) TO WRK-DATA3-DIA
      *     DISPLAY 'ANO: ' WRK-DATA3-ANO
      *     DISPLAY 'MES: ' WRK-DATA3-MES
      *     DISPLAY 'DIA: ' WRK-DATA3-DIA
            MOVE '/' TO WRK-DATA3(3:1)
            MOVE '/' TO WRK-DATA3(6:1)
            DISPLAY 'DATA: ' WRK-DATA3

            MOVE WRK-TIME(1:2) TO WRK-TIME3-HOR
            MOVE WRK-TIME(3:2) TO WRK-TIME3-MIN
      *     MOVE WRK-TIME(5:2) TO WRK-TIME3-SEG


      *     DISPLAY 'TIME: ' WRK-TIME

            MOVE ':' TO WRK-TIME3(3:1)
      *     MOVE ':' TO WRK-TIME3(6:1)
            DISPLAY 'HORA: ' WRK-TIME3
            .
       0002-END.
      *----------------------------------------------------------------*
       0003-NOME.
            DISPLAY "0003-NOME"
            MOVE 'JOSE ROBERTO' TO WRK-NOME
            DISPLAY "NOME: " WRK-NOME
            .
       0003-END.
      *----------------------------------------------------------------*
       9999-FINALIZAR.

            DISPLAY 'FIM DO PROGRAMA'
            STOP RUN
            .
       9999-END.
      *----------------------------------------------------------------*
