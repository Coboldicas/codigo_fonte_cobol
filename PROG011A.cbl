       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG011A.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       working-storage section.

       01  WRK-DADOS.
        05 WRK-NOME       PIC X(20) VALUE 'JOSE ROBERTO'.
        05 WRK-IDADE      PIC 9(02) VALUE 20.
        05 WRK-SALARIO    PIC 9(05) VALUE 12345.

       procedure division.
       
           DISPLAY "ANTES DO INITIALIZE" WRK-DADOS
           initialize WRK-DADOS
                      REPLACING ALPHANUMERIC BY SPACES
                                     NUMERIC BY ZEROES
           DISPLAY "DEPOIS DO INITIALIZE" WRK-DADOS

           STOP RUN.