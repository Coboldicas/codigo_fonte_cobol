       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG009A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-OUT ASSIGN TO "SEQSAI01.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-OUT.
       01 REGISTRO-OUT.
          05 COD     PIC 9(02).
          05 NOME    PIC X(20).
          05 IDADE   PIC 9(02).

       WORKING-STORAGE SECTION.
       01 OPCAO PIC X VALUE "S".

       PROCEDURE DIVISION.

      *> cobol-lint CL002 0000-principal
       0000-PRINCIPAL SECTION.
            PERFORM 0001-ABRE-ARQUIVO 
            PERFORM 0002-ESCREVE-REGISTROS
            PERFORM 0003-FECHA-ARQUIVO
            . 
      *> cobol-lint CL002 0000-fim
       0000-FIM. EXIT.

       0001-ABRE-ARQUIVO SECTION.
           OPEN OUTPUT ARQUIVO-OUT.
      *> cobol-lint CL002 0001-fim
       0001-FIM. EXIT.

       0002-ESCREVE-REGISTROS SECTION.
           PERFORM UNTIL OPCAO = "N"
               DISPLAY "Digite COD: "
               ACCEPT COD
               DISPLAY "Digite Nome: "
               ACCEPT NOME
               DISPLAY "Digite Idade: "
               ACCEPT IDADE

               WRITE REGISTRO-OUT

               DISPLAY "Deseja continuar? (S/N)"
               ACCEPT OPCAO
           END-PERFORM.
      *> cobol-lint CL002 0002-fim
       0002-FIM. EXIT.

       0003-FECHA-ARQUIVO SECTION.
           CLOSE ARQUIVO-OUT.
           DISPLAY "Arquivo Gravado com Sucesso!"
           STOP RUN.
      *> cobol-lint CL002 0003-fim
       0003-FIM. EXIT.