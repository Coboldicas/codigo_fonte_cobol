      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 07/07/2025
      * DESCRICAO..: BUSCA LINEAR EM ARQUIVO
      * NOME.......: BUSCA01A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUSCA01A.
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENTRADA ASSIGN TO 'CADASTRO2.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-ENTRADA.
       01 REGISTRO-ENTRADA.
          05 NOME                      PIC X(30) VALUE SPACES.
          05 IDADE                     PIC 9(02) VALUE ZEROS.
          05 CPF                       PIC 9(11) VALUE ZEROS.

       WORKING-STORAGE SECTION.
       01 WRK-FIM-ARQUIVO              PIC X(01) VALUE 'N'.
       01 WRK-CPF-PROCURADO            PIC 9(11) VALUE ZEROS.
       01 WRK-CPF-LISTADO              PIC 9(11) VALUE ZEROS.
       01 WRK-CPF-ENCONTRADO           PIC X(01) VALUE 'N'.
       01 WRK-IND1                     PIC 9(02) VALUE ZEROS.

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

           PERFORM 0001-LER-ARQUIVO 
          
           DISPLAY 'DIGITE O CPF A SER PROCURADO: '
           ACCEPT WRK-CPF-PROCURADO

           PERFORM 0002-BUSCAR-CPF UNTIL WRK-CPF-ENCONTRADO EQUAL 'S'
                                      OR WRK-FIM-ARQUIVO EQUAL 'S'
           CLOSE ARQUIVO-ENTRADA

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

           READ ARQUIVO-ENTRADA INTO REGISTRO-ENTRADA
              AT END 
                 MOVE 'S'         TO WRK-FIM-ARQUIVO
                 DISPLAY 'CPF NÃO ENCONTRADO!'
              NOT AT END  
                 MOVE CPF         TO WRK-CPF-LISTADO
                 ADD 1            TO WRK-IND1
           END-READ 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PESQUISAR CPF PROCURADO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-BUSCAR-CPF 
       0002-BUSCAR-CPF                 SECTION.
      *----------------------------------------------------------------*

           IF WRK-CPF-PROCURADO     EQUAL WRK-CPF-LISTADO
              DISPLAY 'CPF ENCONTRADO!'
              MOVE 'S'                 TO WRK-CPF-ENCONTRADO 
           ELSE
              PERFORM 0001-LER-ARQUIVO
           END-IF 
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           DISPLAY 'FIM DE PROGRAMA'
           DISPLAY 'QUANTIDADE DE REGISTROS LIDOS: ' WRK-IND1
           STOP RUN 
            .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*