       identification division.
       program-id. PROGTEST4.

       data division.
       working-storage section.
       01 WRK-NUMERO        PIC 9(02) VALUE ZEROS.

       procedure division.
       
           accept WRK-NUMERO
           display "the number is " WRK-NUMERO

           stop run.