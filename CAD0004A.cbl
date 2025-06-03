      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 26/05/2025
      * DESCRICAO..: PROGRAMA COBOL - CADASTRO DE PESSOAS
      * NOME.......: CAD0004A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAD0004A.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-CADASTRO ASSIGN TO "CADASTRO.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.       
       FD ARQUIVO-CADASTRO.
       01  REGISTRO-CADASTRO.
         05 NOME     PIC X(30).
         05 IDADE    PIC 9(03).
         05 CPF      PIC X(14).

       WORKING-STORAGE  SECTION. 
       01  WRK-CONTINUAR PIC X(1) VALUE 'S'.
       01  WRK-NOME   PIC X(30).
       01  WRK-IDADE  PIC 9(03).
       01  WRK-CPF    PIC X(14).

       PROCEDURE DIVISION. 
      *> cobol-lint CL002 0001-processar
       0001-PROCESSAR.
            OPEN OUTPUT ARQUIVO-CADASTRO
            
            PERFORM 0002-CADASTRAR-REG UNTIL WRK-CONTINUAR EQUAL 'N'

            CLOSE ARQUIVO-CADASTRO

            STOP RUN.
      *> cobol-lint CL002 0001-end
       0001-END.
 
       0002-CADASTRAR-REG.
   
           DISPLAY 'DIGITE O NOME: ' 
           ACCEPT WRK-NOME

           DISPLAY 'DIGITE A IDADE: '
           ACCEPT WRK-IDADE

           DISPLAY 'DIGITE O CPF (FORMATO 000.000.000-00): '
           ACCEPT WRK-CPF

           MOVE WRK-NOME  TO NOME
           MOVE WRK-IDADE TO IDADE
           MOVE WRK-CPF   TO CPF

           WRITE REGISTRO-CADASTRO

           DISPLAY 'DESEJA CADASTRAR OUTRA PESSOA? (S/N)'

           ACCEPT WRK-CONTINUAR.

      *> cobol-lint CL002 0002-end
       0002-END.

