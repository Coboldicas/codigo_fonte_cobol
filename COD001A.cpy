      *================================================================*
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 28/01/2025
      * BOOK DE DATA / HORA
      * FORMATO: YYYYMMDD - HMMSSSS
      *----------------------------------------------------------------*
      * COD001A-DATA-ANO    = ANO
      * COD001A-DATA-MES    = MES
      * COD001A-DATA-DIA    = DIA
      * COD001A-DIA-SEMANA  = DIA DA SEMANA
      * COD001A-DESC-MES    = DESCRICAO DO MES
      * COD001A-DESC-SEMANA = DESCRICAO DA SEMANA
      * COD001A-DIAS-ANO    = QUANTIDADE DE DIAS DO ANO
      *----------------------------------------------------------------*
      * COD001A-HORA        = HORA
      * COD001A-MINUTO      = MINUTO
      * COD001A-SEGUNDO     = SEGUNDO
      * COD001A-MILESIMO    = MILISEGUNDO
      * COD001A-PERIODO     = PERIODO DO DIA      
      *================================================================*
       01  COD001A-REGISTRO.
      *     YYYYMMDD
        05  COD001A-DATA.
         10 COD001A-DATA-ANO            PIC 9(004).
         10 COD001A-DATA-MES            PIC 9(002).
         10 COD001A-DATA-DIA            PIC 9(002).
        05  COD001A-DIA-SEMANA          PIC 9(002).
        05  COD001A-DESC-MES            PIC X(020).
        05  COD001A-DESC-SEMANA         PIC X(020).
        05  COD001A-DIAS-ANO            PIC 9(003).
      *     HHMMSSSS
        05  COD001A-TIME.
         10 COD001A-HORA                PIC 9(002).
         10 COD001A-MINUTO              PIC 9(002).
         10 COD001A-SEGUNDO             PIC 9(002).
         10 COD001A-MILESIMO            PIC 9(002).
        05  COD001A-PERIODO             PIC X(020).