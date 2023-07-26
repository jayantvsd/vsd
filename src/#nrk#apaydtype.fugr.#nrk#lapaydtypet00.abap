*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /NRK/APAYDTYPE..................................*
DATA:  BEGIN OF STATUS_/NRK/APAYDTYPE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/NRK/APAYDTYPE                .
CONTROLS: TCTRL_/NRK/APAYDTYPE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */NRK/APAYDTYPE                .
TABLES: /NRK/APAYDTYPE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
