       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG018A.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       working-storage section.


       PROCEDURE DIVISION.

       100-INICIO.

           PERFORM 300-ETAPA2.
           GO TO 200-ETAPA1.
           ALTER 100-INICIO TO PROCEED TO 300-ETAPA2.
      *    GO TO 100-INICIO.

       200-ETAPA1.
           DISPLAY "Etapa 1".
           PERFORM 300-ETAPA2.

       300-ETAPA2.
           DISPLAY "Etapa 2"
           
           STOP RUN.
