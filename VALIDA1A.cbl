      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 04/08/2025
      * DESCRICAO..: VALIDACAO CPF/CNPJ (NOVO FORMATO)
      * NOME.......: VALIDA1A
      ******************************************************************
      *     CPF: 000.000.000-00
      *     ANTES - CNPJ: 00.000.000/0000-00
      *     DEPOIS - CNPJ: XX.XXX.XXX/XXXX-00
      *
      *     CPF 
      *     00000000000 = 11 BYTES
      *     CNPJ
      *     XXXXXXXXXXXX00 = 14 BYTES (12 ALFA + 2 NUM)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDA1A.
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WRK-CPF-CNPJ-ENTRADA         PIC X(14) VALUE SPACES.

       01 WRK-MASCARA-CPF.
         05 WRK-MASC-CPF-PARTE1        PIC 9(03) VALUE ZEROS.
         05 FILLER                     PIC X(01) VALUE '.'.
         05 WRK-MASC-CPF-PARTE2        PIC 9(03) VALUE ZEROS.
         05 FILLER                     PIC X(01) VALUE '.'.
         05 WRK-MASC-CPF-PARTE3        PIC 9(03) VALUE ZEROS.       
         05 FILLER                     PIC X(01) VALUE '-'.
         05 WRK-MASC-CPF-DV            PIC 9(02) VALUE ZEROS.


       01 WRK-MASCARA-CNPJ.
         05 WRK-MASC-CNPJ-PARTE1       PIC X(02) VALUE SPACES.
         05 FILLER                     PIC X(01) VALUE '.'.
         05 WRK-MASC-CNPJ-PARTE2       PIC X(03) VALUE SPACES.
         05 FILLER                     PIC X(01) VALUE '.'.
         05 WRK-MASC-CNPJ-PARTE3       PIC X(03) VALUE SPACES.       
         05 FILLER                     PIC X(01) VALUE '/'.
         05 WRK-MASC-CNPJ-FILIAL       PIC X(04) VALUE SPACES.
         05 FILLER                     PIC X(01) VALUE '-'.
         05 WRK-MASC-CNPJ-DV           PIC 9(02) VALUE ZEROS.
      
       01 WRK-TAMANHO                  PIC 9(02) VALUE ZEROS.
       01 WRK-TAMANHO-ESP              PIC 9(02) VALUE ZEROS.
       01 WRK-VALIDA-DIGITO            PIC X(02) VALUE SPACES.

      *     CPF: 000.000.000-00
       01 WRK-CPF-IDENTIFICADO         PIC X(11) VALUE SPACES.

      *     DEPOIS - CNPJ: XX.XXX.XXX/XXXX-00
       01 WRK-CNPJ-IDENTIFICADO        PIC X(14) VALUE SPACES.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0001-INSERIR-CPF
           PERFORM 0002-VERIFICAR-TAMANHO
           PERFORM 9999-FINALIZAR 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE NUMERO DO CPF ATRAVES DA TELA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-inserir-cpf
       0001-INSERIR-CPF                SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE O NUMERO DO CPF OU CNPJ (NOVO FORMATO)'  
           ACCEPT WRK-CPF-CNPJ-ENTRADA 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO TAMANHO STRING
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-VERIFICAR-TAMANHO
       0002-VERIFICAR-TAMANHO          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-CPF-CNPJ-ENTRADA 
                                       TO WRK-TAMANHO.

           INSPECT WRK-CPF-CNPJ-ENTRADA
                                       TALLYING WRK-TAMANHO-ESP
                                       FOR ALL " "
           
           SUBTRACT WRK-TAMANHO-ESP    FROM WRK-TAMANHO

           IF WRK-TAMANHO              EQUAL 11
              MOVE WRK-CPF-CNPJ-ENTRADA
                                       TO WRK-CPF-IDENTIFICADO
              MOVE WRK-CPF-IDENTIFICADO(1:3)
                                       TO WRK-MASC-CPF-PARTE1
              MOVE WRK-CPF-IDENTIFICADO(4:3)
                                       TO WRK-MASC-CPF-PARTE2
              MOVE WRK-CPF-IDENTIFICADO(7:3)
                                       TO WRK-MASC-CPF-PARTE3
              MOVE WRK-CPF-IDENTIFICADO(10:2)
                                       TO WRK-MASC-CPF-DV
                                          WRK-VALIDA-DIGITO
              PERFORM 0004-VALIDAR-NUMERAL
              PERFORM 0003-VALIDAR-DIGITO
              DISPLAY 'CPF COM FORMATO VALIDO'
              DISPLAY 'CPF: 'WRK-MASCARA-CPF
           ELSE
              IF WRK-TAMANHO           EQUAL 14
                 MOVE WRK-CPF-CNPJ-ENTRADA
                                       TO WRK-CNPJ-IDENTIFICADO
                 MOVE WRK-CNPJ-IDENTIFICADO(1:2)  
                                       TO WRK-MASC-CNPJ-PARTE1
                 MOVE WRK-CNPJ-IDENTIFICADO(3:3) 
                                       TO WRK-MASC-CNPJ-PARTE2
                 MOVE WRK-CNPJ-IDENTIFICADO(6:3)
                                       TO WRK-MASC-CNPJ-PARTE3
                 MOVE WRK-CNPJ-IDENTIFICADO(9:4)
                                       TO WRK-MASC-CNPJ-FILIAL
                 MOVE WRK-CNPJ-IDENTIFICADO(13:2) 
                                       TO WRK-MASC-CNPJ-DV
                                          WRK-VALIDA-DIGITO                 
                 PERFORM 0005-VALIDAR-FILIAL
                 PERFORM 0003-VALIDAR-DIGITO  
                 DISPLAY 'CNPJ COM FORMATO VALIDO'
                 DISPLAY 'CNPJ: ' WRK-MASCARA-CNPJ
              ELSE 
                 DISPLAY 'CPF OU CNPJ INVALIDO!'
                 DISPLAY 'VERIFIQUE O NUMERO DIGITADO!'                  
                 PERFORM 9999-FINALIZAR
            END-IF
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DIGITO VERIFICADOR DO CPF/CNPJ
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-VALIDAR-DIGITO
       0003-VALIDAR-DIGITO             SECTION.
      *----------------------------------------------------------------*
       
           IF WRK-VALIDA-DIGITO        IS NUMERIC 
              CONTINUE 
           ELSE
              DISPLAY 'CPF OU CNPJ INVALIDO!'
              DISPLAY 'VERIFIQUE O NUMERO DO DIGITO!'                  
              PERFORM 9999-FINALIZAR
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO SE TODAS AS PARTES SAO NUMERICAS
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-validar-numeral
       0004-VALIDAR-NUMERAL            SECTION.
      *----------------------------------------------------------------*

           IF WRK-CPF-CNPJ-ENTRADA(1:9) IS NUMERIC
              CONTINUE
           ELSE
              DISPLAY 'CPF COM FORMATO INVALIDO'
              DISPLAY 'CONTEM CARACTERES NAO NUMERICOS'
              PERFORM 9999-FINALIZAR
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO NUMERO DA FILIAL - NAO PODE SER ZERADO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-validar-filial
       0005-VALIDAR-FILIAL             SECTION.
      *----------------------------------------------------------------*

           IF WRK-CNPJ-IDENTIFICADO(9:4)
                                       NOT EQUAL ZEROS
              CONTINUE
           ELSE
              DISPLAY 'FILIAL COM FORMATO INVALIDO'
              DISPLAY 'NUMERO DA FILIAL NAO PODE SER IGUAL A ZEROS'
              PERFORM 9999-FINALIZAR
           END-IF 
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
