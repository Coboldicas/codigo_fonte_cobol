******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 03/09/2025
      * DESCRICAO..: JOGO DA COBRINHA NO GNUCOBOL
      * NOME.......: GAMESNAK
      ******************************************************************
       IDENTIFICATION DIVISION.      *> Inicialização ncurses
       PROGRAM-ID. GAMESNAK.
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURSOR IS CRT-AT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ALTURA               PIC 99 VALUE 20.
       77  LARGURA              PIC 99 VALUE 60.
       77  MAXLEN               PIC 999 VALUE 400.

       77  DIR                  PIC X   VALUE "R".  *> U/D/L/R
       77 TECLA                 PIC X   VALUE SPACE.
       77  SCORE                PIC 9(5) VALUE 0.
       77  GAME-OVER            PIC X   VALUE "N".

       77  NEW-LINE             PIC 99.
       77  NEW-COL              PIC 99.
       77  HEAD-LINE            PIC 99.
       77  HEAD-COL             PIC 99.
       77  FOOD-LINE            PIC 99.
       77  FOOD-COL             PIC 99.
       77  SNAKE-LEN            PIC 999 VALUE 3.
       77  FOUND-COLLISION      PIC X VALUE "N".

       01 CRT-AT.
         05 LINHA   PIC 99.
         05 COLUNA  PIC 99.
       01  SNAKE-LINES          OCCURS 400 TIMES PIC 99.
       01  SNAKE-COLS           OCCURS 400 TIMES PIC 99.
       01 WRK-IND1               PIC 9(02) VALUE ZEROS.
       01 WRK-IND2               PIC 9(02) VALUE ZEROS.
       01 WRK-IND3               PIC 9(02) VALUE ZEROS.
       01 WRK-IND4               PIC 9(02) VALUE ZEROS.
       01 WRK-IND5               PIC 9(02) VALUE ZEROS.

       01 WRK-IND6               PIC 9(02) VALUE ZEROS.
       01 WRK-IND7               PIC 9(02) VALUE ZEROS.
       01 WRK-IND8               PIC 9(02) VALUE ZEROS.
       01 WRK-IND9               PIC 9(02) VALUE ZEROS.
       01 WRK-IND10              PIC 9(02) VALUE ZEROS.
       01 WRK-IND11              PIC 9(02) VALUE ZEROS.
       01 WRK-IND12              PIC 9(02) VALUE ZEROS.
       01 WRK-IND13              PIC 9(02) VALUE ZEROS.
       01 WRK-IND14              PIC 9(02) VALUE ZEROS.
       01  I                     PIC 9(03) VALUE ZEROS.
       77  RND                  PIC 9V9(6).
       77  TENTAR                  PIC 999.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
           PERFORM 0001-INICIAR-JOGO
           PERFORM UNTIL GAME-OVER = "Y"
              PERFORM 0003-DESENHAR-TELA
              PERFORM 0004-LER-ENTRADA
              PERFORM 0005-MOVER-COBRA
              PERFORM 0006-CHECAR-COLISAO
           END-PERFORM
      *    PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL WRK-IND1  > 30
      *      DISPLAY ' ' 
      *    END-PERFORM
           PERFORM 0003-DESENHAR-TELA
           DISPLAY '   GAME OVER! Pontos: ' SCORE
                       AT LINE  24 COLUMN 35
           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    IINICIAR O JOGO
      *----------------------------------------------------------------*
       0001-INICIAR-JOGO               SECTION.
      *----------------------------------------------------------------*

           MOVE 10 TO SNAKE-LINES (1)
                      SNAKE-LINES (2) 
                      SNAKE-LINES (3)
           MOVE  8 TO SNAKE-COLS  (1)
           MOVE  9 TO SNAKE-COLS  (2)
           MOVE 10 TO SNAKE-COLS  (3)
           MOVE 10 TO FOOD-LINE
           MOVE 20 TO FOOD-COL
           PERFORM 0002-LUGAR-COMIDA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LUGAR DE ALIMENTOS
      *----------------------------------------------------------------*
       0002-LUGAR-COMIDA               SECTION.
      *----------------------------------------------------------------*
           
           PERFORM VARYING TENTAR FROM 1 BY 1 UNTIL TENTAR > 200
              COMPUTE RND = FUNCTION RANDOM
              COMPUTE FOOD-LINE = 
                              1 + FUNCTION INTEGER(RND * (ALTURA - 2))
              COMPUTE RND = FUNCTION RANDOM
              COMPUTE FOOD-COL  = 
                            1 + FUNCTION INTEGER(RND * (LARGURA  - 2))
              IF FOUND-COLLISION = "N"
                 EXIT PERFORM   *> significa que achamos posição livre
              END-IF
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    DESENHA A TELA
      *----------------------------------------------------------------*
       0003-DESENHAR-TELA              SECTION.
      *----------------------------------------------------------------*
           *> Cabeçalho
           DISPLAY 'W/A/S/D para mover, Q para sair'
                   AT LINE 1 COLUMN 18
           DISPLAY 'SNAKE (COBOL)  Pontos:' AT LINE 2 COLUMN 20
           DISPLAY SCORE AT LINE 2 COLUMN 43
 
           *> Moldura
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > LARGURA
              *>parte de cima
              COMPUTE WRK-IND2 = I + 1
              DISPLAY '#' AT LINE 3 COLUMN WRK-IND2
              *>parte de baixo
              COMPUTE WRK-IND3 = ALTURA + 3
              DISPLAY '#' AT LINE WRK-IND3 COLUMN WRK-IND2
           END-PERFORM

           PERFORM VARYING I FROM 0 BY 1 UNTIL I > ALTURA
              *>lado esquerdo
              COMPUTE WRK-IND4 = I + 3
              DISPLAY '#' AT LINE WRK-IND4 COLUMN 1
              *>lado direito
              COMPUTE WRK-IND5 = LARGURA + 1
              DISPLAY '*' AT LINE WRK-IND4 COLUMN WRK-IND5
           END-PERFORM

           *> Cobra
                     DISPLAY  SNAKE-LEN
                      AT LINE 26 COLUMN 8
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SNAKE-LEN
              IF I = SNAKE-LEN
                 COMPUTE WRK-IND8 = SNAKE-LINES (I) + 3
                 COMPUTE WRK-IND9 = SNAKE-COLS  (I) + 1
                      DISPLAY  WRK-IND9
                      AT LINE 27 COLUMN 4
                 DISPLAY 'O' AT LINE WRK-IND8 COLUMN WRK-IND9
              ELSE
                 IF I > 1
      *          COMPUTE WRK-IND10= SNAKE-LEN + 1
      *          PERFORM VARYING WRK-IND10 FROM 1 BY 1
      *                               UNTIL WRK-IND10 > SNAKE-LEN
                 COMPUTE WRK-IND8 = SNAKE-LINES (I) + 3
                 COMPUTE WRK-IND9 = SNAKE-COLS  (I) + 1
                      DISPLAY  WRK-IND9
                      AT LINE 28 COLUMN 7
                      DISPLAY 'o' AT LINE WRK-IND8 COLUMN WRK-IND9
      *          END-PERFORM  
                 ELSE 
                   IF I = 1
      *          SUBTRACT 1 FROM SNAKE-LEN 
      *          PERFORM VARYING WRK-IND11 FROM 1 BY 1
      *                               UNTIL WRK-IND11 > SNAKE-LEN
                 COMPUTE WRK-IND8 = SNAKE-LINES (I) + 3
                 DISPLAY WRK-IND11
                 AT LINE 29 COLUMN 10
                 COMPUTE WRK-IND11 = SNAKE-COLS  (I) 
                     DISPLAY ' ' AT LINE WRK-IND8 COLUMN WRK-IND11
      *          END-PERFORM  
                   END-IF
                 END-IF
              END-IF
           END-PERFORM

           *> Comida
           COMPUTE WRK-IND6 = FOOD-LINE + 3
           COMPUTE WRK-IND7 = FOOD-COL  + 1
           DISPLAY '*' AT LINE WRK-IND6 COLUMN WRK-IND7

           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LER ENTRADA DO TECLADO
      *----------------------------------------------------------------*
       0004-LER-ENTRADA               SECTION.
      *----------------------------------------------------------------*

           DISPLAY '<= Movimento - ' AT LINE 1 COLUMN 3
           ACCEPT TECLA
           IF TECLA = 'w' 
           OR TECLA = 'W'
              MOVE 'U' TO DIR
           ELSE 
              IF TECLA = 's' 
              OR TECLA = 'S' 
                 MOVE 'D' TO DIR
              ELSE
                 IF TECLA = 'a' 
                 OR TECLA = 'A'
                    MOVE 'L' TO DIR
                 ELSE
                    IF TECLA = 'd'
                    OR TECLA = 'D'
                       MOVE 'R' TO DIR
                    ELSE
                       IF TECLA = 'q'
                       OR TECLA = 'Q'
                          MOVE 'Y' TO GAME-OVER
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MOVIMENTAR COBRA
      *----------------------------------------------------------------*
       0005-MOVER-COBRA                SECTION.
      *----------------------------------------------------------------*

           *> Calcula nova cabeça
           MOVE SNAKE-LINES (SNAKE-LEN) TO HEAD-LINE
           MOVE SNAKE-COLS  (SNAKE-LEN) TO HEAD-COL
           EVALUATE DIR
             WHEN "U" SUBTRACT 1 FROM HEAD-LINE
             WHEN "D" ADD      1 TO HEAD-LINE
             WHEN "L" SUBTRACT 1 FROM HEAD-COL
             WHEN "R" ADD      1 TO HEAD-COL
           END-EVALUATE


           *> Comeu comida?
           IF HEAD-LINE = FOOD-LINE AND HEAD-COL = FOOD-COL
              ADD 1 TO SNAKE-LEN
              IF SNAKE-LEN > MAXLEN MOVE "Y" TO GAME-OVER
              ELSE
                 *> Empurra o corpo para frente (cresce)
                 PERFORM VARYING I FROM SNAKE-LEN BY -1 UNTIL I = 2
                    MOVE SNAKE-LINES (I - 1) TO SNAKE-LINES (I)
                    MOVE SNAKE-COLS  (I - 1) TO SNAKE-COLS  (I)
                 END-PERFORM
                 MOVE HEAD-LINE TO SNAKE-LINES (SNAKE-LEN)
                 MOVE HEAD-COL  TO SNAKE-COLS  (SNAKE-LEN)
                 ADD 10 TO SCORE
                 PERFORM 0002-LUGAR-COMIDA 
              END-IF
           ELSE
              *> Move normal: desloca corpo e não cresce
              PERFORM VARYING I FROM 1 BY 1 UNTIL I = SNAKE-LEN
                 MOVE SNAKE-LINES (I + 1) TO SNAKE-LINES (I)
                 MOVE SNAKE-COLS  (I + 1) TO SNAKE-COLS  (I)
              END-PERFORM
              MOVE HEAD-LINE TO SNAKE-LINES (SNAKE-LEN)
              MOVE HEAD-COL  TO SNAKE-COLS  (SNAKE-LEN)
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CHECAR COLISAO
      *----------------------------------------------------------------*
       0006-CHECAR-COLISAO             SECTION.
      *----------------------------------------------------------------*

           *> Paredes
           IF HEAD-LINE < 1 OR HEAD-LINE > ALTURA
              MOVE "Y" TO GAME-OVER
           END-IF
           IF HEAD-COL  < 1 OR HEAD-COL  > LARGURA
              MOVE "Y" TO GAME-OVER
           END-IF

           *> Corpo
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = SNAKE-LEN
              IF SNAKE-LINES (I) = HEAD-LINE
                 AND SNAKE-COLS (I) = HEAD-COL
                 MOVE "Y" TO GAME-OVER
              END-IF
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           *> AQUI <===============
      *    DISPLAY '<= AQUI ==>' SNAKE-LEN AT LINE 28 COLUMN 7
           DISPLAY '<= AQUI 2=>' SNAKE-LEN AT LINE 29 COLUMN 7


           DISPLAY 'FIM DE PROGRAMA'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
