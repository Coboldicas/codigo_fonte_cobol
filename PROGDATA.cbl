      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 30/01/2025
      * OBJETIVO: OBTER DATA DO SISTEMA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGDATA.
      *================================================================*
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
 
        01  WRK-DIAS-ANO-YYYYDDD. 
         05 WRK-DIAS-ANO-AAAA          PIC 9(004) VALUE zeros.
         05 WRK-DIAS-ANO-DDD           PIC 9(003) VALUE ZEROS.

       LINKAGE SECTION.
      *01   LNK-AREA-COD001A.
       COPY COD001A.

      *================================================================*
       PROCEDURE DIVISION USING COD001A-REGISTRO.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *     DISPLAY 'INICIO DO PROGRAMA PROGDATA'
            PERFORM 0001-OBTER-DATA
            PERFORM 0002-OBTER-DESC-MES
            PERFORM 0003-OBTER-DESC-SEM
            PERFORM 0004-OBTER-DIAS-ANO
            PERFORM 9999-FINALIZAR
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *    OBTER DATA DO SISTEMA
      *----------------------------------------------------------------*
       0001-OBTER-DATA                 SECTION.
      *----------------------------------------------------------------*

            ACCEPT COD001A-DATA        FROM DATE YYYYMMDD
            ACCEPT COD001A-DIA-SEMANA  FROM DAY-OF-WEEK
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DESCRICAO DO MES
      *----------------------------------------------------------------*
       0002-OBTER-DESC-MES             SECTION.
      *----------------------------------------------------------------*
        
            EVALUATE  COD001A-DATA-MES
            WHEN 01
                MOVE 'JANEIRO'         TO COD001A-DESC-MES
            WHEN 02
                MOVE 'FEVEREIRO'       TO COD001A-DESC-MES
            WHEN 03      
                MOVE 'MARCO'           TO COD001A-DESC-MES
            WHEN 04      
                MOVE 'ABRIL'           TO COD001A-DESC-MES
            WHEN 05      
                MOVE 'MAIO'            TO COD001A-DESC-MES
            WHEN 06      
                MOVE 'JUNHO'           TO COD001A-DESC-MES
            WHEN 07      
                MOVE 'JULHO'           TO COD001A-DESC-MES
            WHEN 08      
                MOVE 'AGOSTO'          TO COD001A-DESC-MES
            WHEN 09      
                MOVE 'SETEMBRO'        TO COD001A-DESC-MES
            WHEN 10      
                MOVE 'OUTUBRO'         TO COD001A-DESC-MES
            WHEN 11      
                MOVE 'NOVEMBRO'        TO COD001A-DESC-MES
            WHEN 12      
                MOVE 'DEZEMBRO'        TO COD001A-DESC-MES
            WHEN OTHER      
                MOVE 'INVALIDO'        TO COD001A-DESC-MES
            END-EVALUATE      
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DESCRICAO DA SEMANA
      *----------------------------------------------------------------*
       0003-OBTER-DESC-SEM             SECTION.
      *----------------------------------------------------------------*
      
            EVALUATE  COD001A-DIA-SEMANA
            WHEN 01
                MOVE 'SEGUNDA-FEIRA'   TO COD001A-DESC-SEMANA
            WHEN 02
                MOVE 'TERCA-FEIRA'     TO COD001A-DESC-SEMANA
            WHEN 03
                MOVE 'QUARTA-FEIRA'    TO COD001A-DESC-SEMANA
            WHEN 04
                MOVE 'QUINTA-FEIRA'    TO COD001A-DESC-SEMANA
            WHEN 05
                MOVE 'SEXTA-FEIRA'     TO COD001A-DESC-SEMANA
            WHEN 06
                MOVE 'SABADO'          TO COD001A-DESC-SEMANA
            WHEN 07
                MOVE 'DOMINGO'         TO COD001A-DESC-SEMANA
            WHEN OTHER
                MOVE 'INVALIDO'        TO COD001A-DESC-SEMANA
            END-EVALUATE
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DIAS DO ANO
      *----------------------------------------------------------------*
       0004-OBTER-DIAS-ANO             SECTION.
      *----------------------------------------------------------------*

            ACCEPT WRK-DIAS-ANO-YYYYDDD 
                                       FROM DAY YYYYDDD

            MOVE WRK-DIAS-ANO-DDD      TO COD001A-DIAS-ANO 
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

            GOBACK 
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
