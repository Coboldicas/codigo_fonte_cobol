      *================================================================*
      * DESCRICAO..: BOOK DE INTERFACE DE CADASTRO DE USUARIO
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 06/02/2025
      * TAMANHO....: 02125
      *----------------------------------------------------------------*
      * COPY002A-COD            = CODIGO DO USUARIO
      * COPY002A-NOME           = NOME DO USUARIO
      * COPY002A-IDADE          = IDADE DO USUARIO
      * COPY002A-DATA-NASC      = DATA DE NASCIMENTO
      * COPY002A-CARGO          = CARGO DO USUARIO    
      * COPY002A-EMAIL          = EMAIL DO USUARIO
      * COPY002A-TELEFONE       = TELEFONE (FIXO OU CELULAR)
      * COPY002A-RUA            = RUA
      * COPY002A-CIDADE         = CIDADE
      * COPY002A-ESTADO         = ESTADO
      * COPY002A-CEP            = CEP
      *================================================================*
       01  COPY002A-HEADER.
        05  COPY002A-COD-BOOK           PIC X(08) VALUE 'COPY002A'.       
        05  COPY002A-TAM-BOOK           PIC 9(05) VALUE 02125.
       01  COPY002A-REGISTRO.
        05  COPY002A-QUANT-REG          PIC 9(02).
        05  COPY002A-CADUSUAR OCCURS 50 TIMES.
         10 COPY002A-COD                PIC 9(02).
         10 COPY002A-NOME               PIC X(30).
         10 COPY002A-IDADE              PIC 9(02).
         10 COPY002A-DATA-NASC          PIC 9(08).
         10 COPY002A-CARGO              PIC X(20).    
         10 COPY002A-EMAIL              PIC X(50).
         10 COPY002A-TELEFONE           PIC 9(09).
         10 COPY002A-ENDERECO.
          15 COPY002A-RUA               PIC X(50).
          15 COPY002A-CIDADE            PIC X(30).
          15 COPY002A-ESTADO            PIC X(02).
          15 COPY002A-CEP               PIC 9(08).