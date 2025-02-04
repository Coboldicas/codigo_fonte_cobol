      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 03/02/2025
      * OBJETIVO: PROGRAMA COBOL - OPERACOES ARITIMETICAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG003A.
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

        01 WRK-VALOR-SIMP           PIC 9(03) VALUE ZEROS.
        01 WRK-VALOR-SIMP2          PIC 9(03) VALUE ZEROS.

        01 WRK-VALOR-SIMP3          PIC 9(03) VALUE ZEROS.

        01 WRK-VALOR-SIMP4          PIC 9(03) VALUE ZEROS.

        01 WRK-VALOR-SIMP5          PIC 9(03) VALUE ZEROS.
        01 WRK-VALOR-SIMP6          PIC 9(03) VALUE ZEROS.

        01 WRK-VALOR-SIMP7          PIC 9(03) VALUE ZEROS.

        01 WRK-VALOR-DEC            PIC 9(07)V99 VALUE ZEROS.

        01 WRK-VALOR-SINAL          PIC S9(07)V99 VALUE ZEROS.

        01 WRK-VALOR-TAXA           PIC S9(05)V9999 VALUE ZEROS.

        01 WRK-VALOR-CORR1.
         05 WRK-VALOR-SAL           PIC 9(07)V99 VALUE ZEROS.
         05 WRK-VALOR-BENEF         PIC 9(07)V99 VALUE ZEROS.

        01 WRK-VALOR-CORR2.
         05 WRK-VALOR-SAL           PIC 9(07)V99 VALUE ZEROS.
         05 WRK-VALOR-BENEF         PIC 9(07)V99 VALUE ZEROS.


       PROCEDURE DIVISION.
            
            MOVE 35 TO WRK-VALOR-SIMP

            DISPLAY 'VALOR SIMPLES: ' WRK-VALOR-SIMP 
            
            ADD WRK-VALOR-SIMP TO WRK-VALOR-SIMP2

            DISPLAY 'VALOR ADCIONADO SIMPLES: ' WRK-VALOR-SIMP2

            ADD 60 TO WRK-VALOR-SIMP2

            DISPLAY 'VALOR 60 ADD SIMPLES 2: ' WRK-VALOR-SIMP2

            SUBTRACT WRK-VALOR-SIMP FROM WRK-VALOR-SIMP2

            DISPLAY 'VALOR SUBTRAIDO SIMPLES 2: ' WRK-VALOR-SIMP2

            SUBTRACT 10 FROM WRK-VALOR-SIMP2 
            
            DISPLAY 'VALOR FINAL SIMPLES 2: ' WRK-VALOR-SIMP2


            DISPLAY ' COMANDO MULTUPLY ************************'
            DISPLAY 'VALOR SIMP3: ' WRK-VALOR-SIMP3 


             MOVE 5 TO WRK-VALOR-SIMP3
             MOVE 3 TO WRK-VALOR-SIMP4

             DISPLAY 'VALOR SIMP3: ' WRK-VALOR-SIMP3 
             DISPLAY 'VALOR SIMP4: ' WRK-VALOR-SIMP4

             MULTIPLY WRK-VALOR-SIMP4
                   BY WRK-VALOR-SIMP3.
             DISPLAY 'VALOR CALCULADO SIMP3: ' WRK-VALOR-SIMP3


             MULTIPLY WRK-VALOR-SIMP2  by WRK-VALOR-SIMP3
             DISPLAY 'VALOR CALCULADO SIMP3: ' WRK-VALOR-SIMP3
             
             DISPLAY 'VALOR SIMP4: ' WRK-VALOR-SIMP4
            
             DIVIDE WRK-VALOR-SIMP4 INTO WRK-VALOR-SIMP3
             
             DISPLAY 'VALOR DIVIDIDO SIMP3: ' WRK-VALOR-SIMP3
 
             DIVIDE WRK-VALOR-SIMP4  INTO WRK-VALOR-SIMP3 
                                             GIVING WRK-VALOR-SIMP5
                                          REMAINDER WRK-VALOR-SIMP6
             DISPLAY 'VALOR FINAL SIMP5: ' WRK-VALOR-SIMP5
             DISPLAY 'VALOR SOBRADO    : ' WRK-VALOR-SIMP6

             COMPUTE WRK-VALOR-SIMP7 = (WRK-VALOR-SIMP5 + 
                                        WRK-VALOR-SIMP4)

             DISPLAY 'VALOR COMPUTE SIMP7: ' WRK-VALOR-SIMP7

             COMPUTE WRK-VALOR-SIMP7 = ((WRK-VALOR-SIMP5 / 3) + 10)
                                        
             DISPLAY 'VALOR COMPUTE 2 SIMP7: ' WRK-VALOR-SIMP7

             . 
            STOP RUN.
