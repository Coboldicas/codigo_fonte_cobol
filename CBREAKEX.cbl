 
      IDENTIFICATION DIVISION.
      PROGRAM-ID. "CBREAKEX".
      AUTHOR. ALFRED SIMPSON.
      ENVIRONMENT DIVISION.
      
      INPUT-OUTPUT SECTION.
      FILE-CONTROL.
      SELECT SALES-FILE ASSIGN TO INFILE
      ORGANIZATION IS LINE SEQUENTIAL.
      
      SELECT REPORT-FILE ASSIGN TO PRNTFILE
      ORGANIZATION IS LINE SEQUENTIAL.
      DATA DIVISION.
      FILE SECTION.
      
      FD SALES-FILE
      RECORD CONTAINS 60 CHARACTERS
      DATA RECORD IS NEXT-RECORD.
      01  NEXT-RECORD.
      
          05 NEXT-STATE                  PIC X(2).
          05 NEXT-CITY                   PIC X(20).
          05 FILLER                      PIC X(30).
          05 FILLER                      PIC S9(6)V9(2).
      
      FD REPORT-FILE
      DATA RECORD IS REPORT-RECORD.
      01  REPORT-RECORD                  PIC X(80).
 
      
      WORKING-STORAGE SECTION.
      01  CURRENT-RECORD.
          05  CURRENT-STATE              PIC X(2).
          05  CURRENT-CITY               PIC X(20).
      
          05  CURRENT-CUSTOMER           PIC X(30).
          05  CURRENT-AMOUNT             PIC S9(6)V9(2).
 
      01  LEVEL-CONTROL                  PIC 9999.
      
 
      01  WORKING-NUMERICS.
          05  AMOUNT-BY-FEDERAL          PIC S9(8)V9(2).
          05  AMOUNT-BY-STATE            PIC S9(8)V9(2).
      
          05  AMOUNT-BY-CITY             PIC S9(8)V9(2).
          05  CUSTOMER-COUNT-BY-FEDERAL  PIC 9(5).
          05  CUSTOMER-COUNT-BY-STATE    PIC 9(5).
          05  CUSTOMER-COUNT-BY-CITY     PIC 9(5).
      
          05  CITY-COUNT-BY-FEDERAL      PIC 9(5).
          05  CITY-COUNT-BY-STATE        PIC 9(5).
          05  STATE-COUNT-BY-FEDERAL     PIC 9(5).
 
      
      01  STATE-HEADER.
          05  FILLER                     PIC X(7) VALUE " STATE ".
          05  LN01-STATE                 PIC X(2).
 
      
      01  CITY-HEADER.
          05  FILLER                     PIC X(9) VALUE " CITY ".
          05  LN02-CITY                  PIC X(20).
 
      
      01  CUSTOMER-DETAIL.
          05  FILLER                     PIC X(5) VALUE SPACES.
          05  LN03-CUSTOMER              PIC X(30).
          05  FILLER                     PIC X(5) VALUE SPACES.
      
          05  LN03-AMOUNT                PIC ----,--9.99.
 
      01  CITY-TRAILER.
          05  FILLER                     PIC X(6) VALUE " CITY ".
      
          05  LN04-CITY                  PIC X(20).
          05  FILLER                     PIC X(5) VALUE SPACES.
          05  LN04-AMOUNT                PIC ---,---,--9.99.
          05  FILLER                     PIC X(5) VALUE SPACES.
      
          05  LN04-CUSTOMER-COUNT        PIC ZZZ,ZZ9.
          05  FILLER                     PIC X(12) VALUE " CUSTOMERS ".
 
      01  STATE-TRAILER.
      
          05  FILLER                     PIC X(10) VALUE " STATE ".
          05  LN05-STATE                 PIC X(2).
          05  FILLER                     PIC X(5) VALUE SPACES.
          05  LN05-AMOUNT                PIC ---,---,--9.99.
      
          05  FILLER                     PIC X(5) VALUE SPACES.
          05  LN05-CITY-COUNT            PIC ZZZ,ZZ9.
          05  FILLER                     PIC X(9) VALUE " CITIES ".
          05  LN05-CUSTOMER-COUNT        PIC ZZZ,ZZ9.
      
          05  FILLER                     PIC X(12) VALUE " CUSTOMERS ".
 
      01  FEDERAL-TRAILER.
          05  FILLER                     PIC X(9) VALUE " FEDERAL ".
      
          05  LN06-AMOUNT                PIC ---,---,--9.99.
          05  FILLER                     PIC X(5) VALUE SPACES.
          05  LN06-CUSTOMER-COUNT        PIC ZZZ,ZZ9.
          05  FILLER                     PIC X(12) VALUE " CUSTOMERS ".
      
          05  LN06-CITY-COUNT            PIC ZZZ,ZZ9.
          05  FILLER                     PIC X(9) VALUE " CITIES ".
          05  LN06-STATE-COUNT           PIC ZZZ,ZZ9.
          05  FILLER                     PIC X(9) VALUE " STATES ".
      
 
      PROCEDURE DIVISION.
 
      PROCEDURE-MAIN.
      
          PERFORM PROGRAM-INITIAL.
          PERFORM MAIN-LINE           UNTIL LEVEL-CONTROL = 100.
          PERFORM PROGRAM-SUMMARY.
          STOP RUN.
      
 
      PROGRAM-INITIAL.
          MOVE ZERO                      TO AMOUNT-BY-FEDERAL.
          MOVE ZERO                      TO STATE-COUNT-BY-FEDERAL.
      
          MOVE ZERO                      TO CITY-COUNT-BY-FEDERAL.
          MOVE ZERO                      TO CUSTOMER-COUNT-BY-FEDERAL.
          OPEN INPUT SALES-FILE.
          OPEN OUTPUT REPORT-FILE.
      
          READ SALES-FILE AT END MOVE 100 TO LEVEL-CONTROL
                      NOT AT END MOVE 99  TO LEVEL-CONTROL
          END-READ.
 
      
      PROGRAM-SUMMARY.
          MOVE AMOUNT-BY-FEDERAL         TO LN06-AMOUNT.
          MOVE CUSTOMER-COUNT-BY-FEDERAL TO LN06-CUSTOMER-COUNT.
          MOVE CITY-COUNT-BY-FEDERAL     TO LN06-CITY-COUNT.
      
          MOVE STATE-COUNT-BY-FEDERAL    TO LN06-STATE-COUNT.
          WRITE REPORT-RECORD          FROM FEDERAL-TRAILER.
          CLOSE SALES-FILE REPORT-FILE.
 
      
      MAIN-LINE.
          MOVE NEXT-RECORD TO CURRENT-RECORD.
          IF LEVEL-CONTROL >= 20    PERFORM STATE-INITIAL.
          IF LEVEL-CONTROL >= 10    PERFORM CITY-INITIAL.
      
          PERFORM CUSTOMER-NORMAL-PROCESS.
          PERFORM GET-NEXT-RECORD.
          IF LEVEL-CONTROL >= 10    PERFORM CITY-SUMMARY.
          IF LEVEL-CONTROL >= 20    PERFORM STATE-SUMMARY.
      
 
      STATE-INITIAL.
          MOVE ZERO                      TO AMOUNT-BY-STATE.
          MOVE ZERO                      TO CITY-COUNT-BY-STATE.
      
          MOVE ZERO                      TO CUSTOMER-COUNT-BY-STATE.
          MOVE CURRENT-STATE             TO LN01-STATE.
          WRITE REPORT-RECORD          FROM STATE-HEADER.
 
      
      CITY-INITIAL.
          MOVE ZERO                      TO AMOUNT-BY-CITY.
          MOVE ZERO                      TO CUSTOMER-COUNT-BY-CITY.
          MOVE CURRENT-CITY              TO LN02-CITY.
      
          WRITE REPORT-RECORD          FROM CITY-HEADER.
 
      CUSTOMER-NORMAL-PROCESS.
          MOVE CURRENT-CUSTOMER          TO LN03-CUSTOMER.
      
          MOVE CURRENT-AMOUNT            TO LN03-AMOUNT.
          WRITE REPORT-RECORD          FROM CUSTOMER-DETAIL.
          ADD CURRENT-AMOUNT             TO AMOUNT-BY-CITY.
          ADD 1                          TO CUSTOMER-COUNT-BY-CITY.
      
 
      GET-NEXT-RECORD.
          READ SALES-FILE AT END MOVE 100 TO LEVEL-CONTROL
                      NOT AT END PERFORM SET-LEVEL-CONTROL
      
          END-READ.
 
      SET-LEVEL-CONTROL.
          IF NEXT-STATE NOT = CURRENT-STATE
      
          THEN MOVE 20                   TO LEVEL-CONTROL
          ELSE IF NEXT-CITY NOT = CURRENT-CITY
               THEN MOVE 10              TO LEVEL-CONTROL
               ELSE MOVE 0               TO LEVEL-CONTROL.
      
 
      CITY-SUMMARY.
          MOVE CURRENT-CITY              TO LN04-CITY.
          MOVE AMOUNT-BY-CITY            TO LN04-AMOUNT.
      
          MOVE CUSTOMER-COUNT-BY-CITY    TO LN04-CUSTOMER-COUNT.
          WRITE REPORT-RECORD          FROM CITY-TRAILER.
          ADD AMOUNT-BY-CITY             TO AMOUNT-BY-STATE.
          ADD CUSTOMER-COUNT-BY-CITY     TO CUSTOMER-COUNT-BY-STATE.
      
          ADD 1                          TO CITY-COUNT-BY-STATE.
 
      STATE-SUMMARY.
          MOVE CURRENT-STATE             TO LN05-STATE.
      
          MOVE AMOUNT-BY-STATE           TO LN05-AMOUNT.
          MOVE CITY-COUNT-BY-STATE       TO LN05-CITY-COUNT.
          MOVE CUSTOMER-COUNT-BY-STATE   TO LN05-CUSTOMER-COUNT.
          WRITE REPORT-RECORD          FROM STATE-TRAILER.
      
          ADD AMOUNT-BY-STATE            TO AMOUNT-BY-FEDERAL.
          ADD CUSTOMER-COUNT-BY-STATE    TO CUSTOMER-COUNT-BY-FEDERAL.
          ADD CITY-COUNT-BY-STATE        TO CITY-COUNT-BY-FEDERAL.
          ADD 1                          TO STATE-COUNT-BY-FEDERAL.