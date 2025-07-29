      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 28/07/2025
      * DESCRICAO..: VALIDACAO SIMPLES DE CPF - FORMATO
      * NOME.......: VALCPF01
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALCPF01.
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WRK-CPF-ENTRADA              PIC X(14) VALUE SPACES.

       01 WRK-CPF-PARTE1               PIC 9(03) VALUE ZEROS.
       01 WRK-CPF-PARTE2               PIC 9(03) VALUE ZEROS.
       01 WRK-CPF-PARTE3               PIC 9(03) VALUE ZEROS.       
       01 WRK-CPF-DV                   PIC 9(02) VALUE ZEROS.

       01 WRK-PONTOS                   PIC 9(01) VALUE ZEROS.
       01 WRK-TRACOS                   PIC 9(01) VALUE ZEROS.

       01 WRK-VALIDA                   PIC X(01) VALUE SPACES.

       01 WRK-AUX                      PIC X(07) VALUE SPACES.

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
           PERFORM 0002-VERIFICAR-PONTO 
           PERFORM 0003-QUEBRAR-CPF
           PERFORM 0004-VALIDAR-NUMERAL
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

           DISPLAY 'DIGITE O NUMERO DO CPF NO FORMATO 000.000.000-00'  
           ACCEPT WRK-CPF-ENTRADA 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VERIFICACAO DE PONTOS E TRACOS
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-VERIFICAR-PONTO
       0002-VERIFICAR-PONTO            SECTION.
      *----------------------------------------------------------------*

           INSPECT WRK-CPF-ENTRADA TALLYING WRK-PONTOS FOR ALL '.'
           INSPECT WRK-CPF-ENTRADA TALLYING WRK-TRACOS FOR ALL '-'           

           IF WRK-PONTOS                EQUAL 2 AND 
              WRK-TRACOS                EQUAL 1
              CONTINUE
           ELSE 
              DISPLAY 'FORMATO INVALIDO: PONTOS E TRACOS FORA DO PADRAO'
              PERFORM 9999-FINALIZAR
           END-IF 
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    QUEBRAR O CPF NAS PARTES COM UNSTRING
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-quebrar-cpf
       0003-QUEBRAR-CPF                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-TRACOS 

           UNSTRING WRK-CPF-ENTRADA
           DELIMITED BY '.'
            INTO WRK-CPF-PARTE1
                 WRK-CPF-PARTE2
                 WRK-AUX

           UNSTRING WRK-AUX
           DELIMITED BY '-'
            INTO WRK-CPF-PARTE3
                 WRK-CPF-DV

           INSPECT WRK-AUX TALLYING WRK-TRACOS FOR ALL '-'           

           IF WRK-TRACOS                EQUAL 1
              CONTINUE
           ELSE 
              DISPLAY 'FORMATO INVALIDO: PONTOS E TRACOS FORA DO PADRAO'
              PERFORM 9999-FINALIZAR
           END-IF 

           IF WRK-CPF-ENTRADA(4:1)   EQUAL '.'
              IF WRK-CPF-ENTRADA(8:1) EQUAL '.'
                 CONTINUE
              ELSE 
                 DISPLAY 'FORMATO INVALIDO: PONTOS FORA DO PADRAO'
                 PERFORM 9999-FINALIZAR
              END-IF
           ELSE 
              DISPLAY 'FORMATO INVALIDO: PONTOS FORA DO PADRAO'
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

           IF WRK-CPF-PARTE1 IS NUMERIC AND 
              WRK-CPF-PARTE2 IS NUMERIC AND 
              WRK-CPF-PARTE3 IS NUMERIC AND 
              WRK-CPF-DV     IS NUMERIC 
              DISPLAY 'CPF COM FORMATO VALIDO'
           ELSE
              DISPLAY 'FORMATO INVALIDO:CONTEM CARACTERES NAO NUMERICOS'
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0021-end
       0021-END.                       EXIT.
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
