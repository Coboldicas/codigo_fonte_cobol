       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG007A.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NOME PIC X(20).
       01 WS-IDADE PIC 99.
       
       PROCEDURE DIVISION.
           DISPLAY '***************************************'
           DISPLAY '*        Simulação de Interface BMS     *'
           DISPLAY '***************************************'
           DISPLAY 'Digite seu nome: '
           ACCEPT WS-NOME
           DISPLAY 'Digite sua idade: '
           ACCEPT WS-IDADE
           DISPLAY '***************************************'
           DISPLAY '*  Dados Recebidos                     *'
           DISPLAY '***************************************'
           DISPLAY 'Nome: ' WS-NOME
           DISPLAY 'Idade: ' WS-IDADE
           STOP RUN.