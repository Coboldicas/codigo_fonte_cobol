      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 30/06/2025
      * DESCRICAO..: ORDENACAO MANUAL - BUBBLE SORT
      * NOME.......: SORT002A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT002A.
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENTRADA ASSIGN TO 'CADASTRO3.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-ENTRADA.
      *    RECORD CONTAINS 40 CHARACTERS
      *    BLOCK CONTAINS 0 RECORDS
      *    DATA RECORD IS REG-ENTRADA.
       01 REG-ENTRADA.
        05 REG-NOME         PIC X(30).
        05 REG-IDADE        PIC 9(02).
        05 REG-SALARIO      PIC 9(05)V99.

       WORKING-STORAGE SECTION.
       01 TABELA-NOMES.
           05 NOME-ALUNO     OCCURS 20 TIMES.
               10 NOME       PIC X(30).
               10 IDADE      PIC 9(02).
               10 SALARIO    PIC 9(05)V99.

       01  INDICE-LEITURA    PIC 9(02) VALUE 1.
       01  WRK-IND1          PIC 9(02).
       01  WRK-IND2          PIC 9(02).
       01  WRK-NOME-AUX      PIC X(40).
       01  WRK-CTPO-CASS     PIC X(01) VALUE 'N'.
       01  WRK-FIM-ARQUIVO   PIC X(01) VALUE 'N'.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-ENTRADA
           PERFORM 0001-LER-ARQUIVO UNTIL WRK-FIM-ARQUIVO EQUAL 'S'
           CLOSE ARQUIVO-ENTRADA

           DISPLAY "TABELA ANTES DA ORDENACAO:".
           PERFORM 0003-MOSTRAR-TABELA

           DISPLAY 'DEFINA MODO DE CLASSIFICACAO'
           DISPLAY '[N] NOME [I] IDADE [S] SALARIO'
           ACCEPT WRK-CTPO-CASS

           EVALUATE WRK-CTPO-CASS 
            WHEN 'N'
                 PERFORM 0002-CLASS-REG-NOME
            WHEN 'I'
                 PERFORM 0004-CLASS-REG-IDADE
            WHEN 'S'
                 PERFORM 0005-CLASS-REG-SALARIO 
            WHEN OTHER
                 DISPLAY 'OPCAO INCORRETA'
                 PERFORM 9999-FINALIZAR
           END-EVALUATE

           DISPLAY "TABELA DEPOIS DA ORDENACAO:".
           PERFORM 0003-MOSTRAR-TABELA

           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA ARQUIVO ENTRADA
      *----------------------------------------------------------------*
       0001-LER-ARQUIVO                SECTION.
      *----------------------------------------------------------------*

           READ ARQUIVO-ENTRADA INTO REG-ENTRADA
              AT END 
                 MOVE 'S' TO WRK-FIM-ARQUIVO
              NOT AT END  
                 MOVE REG-ENTRADA TO NOME-ALUNO (INDICE-LEITURA)
                 ADD 1            TO INDICE-LEITURA
           END-READ 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CLASSIFICAR REGISTROS POR NOME
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-class-reg-nome
       0002-CLASS-REG-NOME             SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL WRK-IND1 > 19
               PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL 
                                           WRK-IND2 > 20 - WRK-IND1
                   IF NOME (WRK-IND2) > NOME (WRK-IND2 + 1)
                       MOVE NOME-ALUNO  (WRK-IND2)
                                       TO WRK-NOME-AUX
                       MOVE NOME-ALUNO  (WRK-IND2 + 1)
                                       TO NOME-ALUNO  (WRK-IND2)
                       MOVE WRK-NOME-AUX 
                                       TO NOME-ALUNO  (WRK-IND2 + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MOSTRAR TABELA INTERNA
      *----------------------------------------------------------------*
       0003-MOSTRAR-TABELA             SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL WRK-IND1 > 20
               DISPLAY NOME-ALUNO (WRK-IND1)
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CLASSIFICAR REGISTROS POR IDADE
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-class-reg-idade
       0004-CLASS-REG-IDADE            SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL WRK-IND1 > 19
               PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL 
                                           WRK-IND2 > 20 - WRK-IND1
                   IF IDADE  (WRK-IND2) > IDADE  (WRK-IND2 + 1)
                       MOVE NOME-ALUNO  (WRK-IND2)
                                       TO WRK-NOME-AUX
                       MOVE NOME-ALUNO  (WRK-IND2 + 1)
                                       TO NOME-ALUNO  (WRK-IND2)
                       MOVE WRK-NOME-AUX 
                                       TO NOME-ALUNO  (WRK-IND2 + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CLASSIFICAR REGISTROS POR SALARIO
      *----------------------------------------------------------------*
       0005-CLASS-REG-SALARIO           SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL WRK-IND1 > 19
               PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL 
                                           WRK-IND2 > 20 - WRK-IND1
                   IF SALARIO  (WRK-IND2) > SALARIO  (WRK-IND2 + 1)
                       MOVE NOME-ALUNO  (WRK-IND2)
                                       TO WRK-NOME-AUX
                       MOVE NOME-ALUNO  (WRK-IND2 + 1)
                                       TO NOME-ALUNO  (WRK-IND2)
                       MOVE WRK-NOME-AUX 
                                       TO NOME-ALUNO  (WRK-IND2 + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           DISPLAY 'FIM DE PROGRAMA'
           STOP RUN 
            .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*