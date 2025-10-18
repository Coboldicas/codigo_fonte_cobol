       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG012A.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       working-storage section.

       01  WRK-DADOS.
        05 WRK-NOME       PIC X(30) VALUE 'J3241OSE R321OBERTO    '.
        05 WRK-CONTADOR       PIC 9(02) VALUE ZEROS.
        05 WRK-NOME2       PIC X(20) VALUE SPACES.
        05 WRK-CONTADOR2      PIC 9(02) VALUE ZEROS.
        05 WRK-CONTADOR3      PIC 9(02) VALUE ZEROS.

       01  IND-1             PIC 9(3) VALUE ZEROS.
       01  CHAR-ATUAL      PIC X(1).

      * 05 WRK-IDADE      PIC 9(02) VALUE 20.
      * 05 WRK-SALARIO    PIC 9(05) VALUE 12345.

       procedure division.
       

      *    DISPLAY "QUANTIDADE DE CARACTERES: " WRK-CONTADOR

           DISPLAY "NOME: " WRK-NOME 


           PERFORM VARYING IND-1 FROM 1 BY 1 UNTIL IND-1 > LENGTH OF 
              WRK-NOME
               MOVE WRK-NOME(IND-1:1) TO CHAR-ATUAL
               IF CHAR-ATUAL IS NUMERIC
                   ADD 1 TO WRK-CONTADOR
               END-IF
           END-PERFORM.



           DISPLAY "QUANTIDADE DE CARACTERES: " WRK-CONTADOR

      *    inspect WRK-NOME tallying WRK-CONTADOR
      *      for ALL 'BE'


      *    DISPLAY "QUANTIDADE DE CARACTERES: " WRK-CONTADOR


      *    inspect WRK-NOME replacing ALL '1978' BY '1968'.
             
      *    inspect WRK-NOME tallying WRK-CONTADOR
      *            for leading spaces


      *    DISPLAY "QUANTIDADE DE ESPACOS: " WRK-CONTADOR

                              
      *    move WRK-NOME(WRK-CONTADOR:20) to WRK-NOME2
      *    DISPLAY "nome 2: " WRK-NOME2

      *    display "TAMANHO DO CAMPO: " function length 
      *                       (WRK-NOME).


      *    move WRK-CONTADOR2 to function length (WRK-CONTADOR)
      *    add 1 to WRK-CONTADOR

      *    display 'nome correto: ' WRK-NOME(WRK-CONTADOR:20)



           STOP RUN.