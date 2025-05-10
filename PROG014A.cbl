       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG014A.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       working-storage section.

       01  wrk-campo     pic x(50).
       01  wrk-tamanho   pic 9(03).


       procedure division.
       
           move 'klsjdh fsjkd' to wrk-campo
           display "campo: " wrk-campo

           inspect function reverse(wrk-campo) tallying wrk-tamanho for
           leading spaces

           subtract length of wrk-campo from wrk-tamanho



      *    move wrk-campo(1:LENGTH OF wrk-campo) to wrk-tamanho
           
           display "TAMANHO DO CAMPO: " wrk-tamanho

           stop run.