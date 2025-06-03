      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 02/06/2025
      * OBJETIVO: PROGRAMA DE LEITURA DE ARQUIVO SEQUENCIAL
      * OBS.: 
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LER0002A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENTRADA      ASSIGN TO "CADASTRO2.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION. 
       FD ARQUIVO-ENTRADA.
       01  FD-REG-ENTRADA.
         05 NOME   PIC X(30).
         05 IDADE  PIC 9(02).
         05 CPF    PIC 9(11).

       WORKING-STORAGE SECTION.
       01  WRK-FIM-ARQUIVO PIC X(01) VALUE 'N'.

       PROCEDURE DIVISION.

      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR SECTION.

            PERFORM 0001-INICIAR
            PERFORM 0002-LER-ARQUIVO UNTIL WRK-FIM-ARQUIVO EQUAL 'S'         
            PERFORM 0003-FINALIZAR.

      *> cobol-lint CL002 0000-end
       0000-END.

       0001-INICIAR SECTION.
            OPEN INPUT ARQUIVO-ENTRADA.
            READ ARQUIVO-ENTRADA INTO FD-REG-ENTRADA
                AT END MOVE 'S' TO WRK-FIM-ARQUIVO
            END-READ.
      *> cobol-lint CL002 0001-end
       0001-END.

      *> cobol-lint CL002 0002-ler-arquivo
       0002-LER-ARQUIVO SECTION.

           DISPLAY 'NOME....: ' NOME
           DISPLAY 'IDADE...: ' IDADE
           DISPLAY 'NUM CPF.: ' CPF

           READ ARQUIVO-ENTRADA INTO FD-REG-ENTRADA
                AT END MOVE 'S' TO WRK-FIM-ARQUIVO
           END-READ.

       *> cobol-lint CL002 0002-end
       0002-END.

      *> cobol-lint CL002 0003-finalizar
       0003-FINALIZAR SECTION.

            CLOSE ARQUIVO-ENTRADA

            STOP RUN.
      *> cobol-lint CL002 0003-end
       0003-END.