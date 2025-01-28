      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 27/01/2025
      * OBJETIVO: PROGRAMA COBOL PARA DEMONSTRAR COMANDOS
      *           IF/ELSE, EVALUATE, PERFORM e GO TO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG002A.
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
       01  WRK-NOME2 PIC X(20) VALUE SPACES.

       01  WRK-IND   PIC 9(02) VALUE ZEROS.
       01  WRK-IND1  PIC 9(02) VALUE ZEROS.
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

            IF WRK-TIME3-HOR EQUAL 19
               IF WRK-TIME3-MIN GREATER 18
                  MOVE 'JOSE ROBERTO' TO WRK-NOME
               ELSE
                  MOVE 'PROGRAMA 01 ' TO WRK-NOME
               END-IF
            ELSE
               IF WRK-DATA3-DIA EQUAL 26
                  MOVE 'COBOL DICAS'   TO WRK-NOME
               ELSE
                  MOVE 'PROGRAMA 02 '   TO WRK-NOME
      *           GO TO  9999-FINALIZAR
               END-IF
            END-IF

            DISPLAY "NOME: " WRK-NOME


            MOVE '19' TO WRK-DATA3-MES

            EVALUATE  WRK-DATA3-MES
            WHEN 01
                MOVE 'JANEIRO' TO WRK-NOME2
            WHEN 02
                MOVE 'FEVEREIRO' TO WRK-NOME2
            WHEN 03
                MOVE 'MARCO' TO WRK-NOME2
            WHEN 04
                MOVE 'ABRIL' TO WRK-NOME2
            WHEN 05
                MOVE 'MAIO' TO WRK-NOME2
            WHEN 06
                MOVE 'JUNHO' TO WRK-NOME2
            WHEN 07
                MOVE 'JULHO' TO WRK-NOME2
            WHEN 08
                MOVE 'AGOSTO' TO WRK-NOME2
            WHEN 09
                MOVE 'SETEMBRO' TO WRK-NOME2
            WHEN 10
                MOVE 'OUTUBRO' TO WRK-NOME2
            WHEN 11
                MOVE 'NOVEMBRO' TO WRK-NOME2
            WHEN 12
                MOVE 'DEZEMBRO' TO WRK-NOME2
            WHEN OTHER
                MOVE 'INVALIDO' TO WRK-NOME2
            END-EVALUATE


            DISPLAY "NOME: " WRK-NOME2

            PERFORM 0004-CONDICAO1 UNTIL WRK-IND EQUAL 10


            .
       0003-END.

       0004-CONDICAO1.

            ADD 1 TO WRK-IND
      *     DISPLAY 'UNTIL' WRK-IND

            PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL WRK-IND1
                                                  GREATER 10
               DISPLAY 'UNTIL' WRK-IND ' VARYING'  WRK-IND1

            END-PERFORM
            GO TO 9999-FINALIZAR

           .
       0004-END.
      *----------------------------------------------------------------*
       9999-FINALIZAR.

            DISPLAY 'FIM DO PROGRAMA'
            STOP RUN
            .
       9999-END.
      *----------------------------------------------------------------*
