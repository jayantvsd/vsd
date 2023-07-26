*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /NRK/APAYVCODE..................................*
DATA:  BEGIN OF STATUS_/NRK/APAYVCODE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/NRK/APAYVCODE                .
CONTROLS: TCTRL_/NRK/APAYVCODE
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: */NRK/APAYVCODE                .
TABLES: /NRK/APAYVCODE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
