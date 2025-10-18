      *>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGVSAM.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO "clientes.dat"
      *       ORGANIZATION IS INDEXED
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS CUST-ID
              ALTERNATE RECORD KEY IS ALT-EMAIL WITH DUPLICATES
              FILE STATUS IS FS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CLIENTES.
       01  REG-CLIENTE.
           05 CUST-ID     PIC 9(6).
           05 NOME        PIC X(40).
           05 EMAIL       PIC X(50).
           05 ALT-EMAIL   REDEFINES EMAIL PIC X(50).
       
       WORKING-STORAGE SECTION.
       01  FS             PIC XX.
       01  I              PIC 9(02).
       
       01  TAB-SEED.
           05 TS OCCURS 6 TIMES.
              10 TS-ID    PIC 9(6).
              10 TS-NOME  PIC X(40).
              10 TS-EMAIL PIC X(50).
       
       PROCEDURE DIVISION.
      *> cobol-lint CL002 main
       MAIN.
           PERFORM CARREGAR-SEED.
           PERFORM ABRIR.
           PERFORM GRAVAR-SEED.
           CLOSE CLIENTES.
           DISPLAY "Seed finalizado com FS=" FS.
           STOP RUN.
       
       CARREGAR-SEED.
           MOVE 000001                     TO TS-ID(1)    
           MOVE 000002                     TO TS-ID(2)    
           MOVE 000003                     TO TS-ID(3)    
           MOVE 000004                     TO TS-ID(4)    
           MOVE 000005                     TO TS-ID(5)    
           MOVE 000006                     TO TS-ID(6)    

           MOVE "ALICE SILVA"              TO TS-NOME(1)
           MOVE "BRUNO SOUZA"              TO TS-NOME(2)
           MOVE "CARLA MENDES"             TO TS-NOME(3)
           MOVE "DANIELA PEREIRA"          TO TS-NOME(4)
           MOVE "EDUARDO GOMES"            TO TS-NOME(5)
           MOVE "FERNANDA ALMEIDA"         TO TS-NOME(6)

           MOVE "alice@exemplo.com"        TO TS-EMAIL(1)
           MOVE "bruno@exemplo.com"        TO TS-EMAIL(2)
           MOVE "carla@exemplo.com"        TO TS-EMAIL(3)
           MOVE "dani.p@exemplo.com"       TO TS-EMAIL(4)
           MOVE "ed.gomes@exemplo.com"     TO TS-EMAIL(5)
           MOVE "fernanda@exemplo.com"     TO TS-EMAIL(6).  

       ABRIR.
           OPEN I-O CLIENTES
           IF FS = "35"
              DISPLAY "Arquivo nao existe. Criando..."
              OPEN OUTPUT CLIENTES
              CLOSE CLIENTES
              OPEN I-O CLIENTES
           END-IF
           IF FS NOT = "00"
              DISPLAY "Falha ao abrir CLIENTES. FS=" FS
              STOP RUN
           END-IF.
       
       GRAVAR-SEED.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
              MOVE TS-ID(I)    TO CUST-ID
              MOVE TS-NOME(I)  TO NOME
              MOVE TS-EMAIL(I) TO EMAIL
              WRITE REG-CLIENTE INVALID KEY
                   DISPLAY "Ja existe ID " CUST-ID " (FS=" FS ")"
              NOT INVALID KEY
                   DISPLAY "Incluido ID " CUST-ID
              END-WRITE
           END-PERFORM.
