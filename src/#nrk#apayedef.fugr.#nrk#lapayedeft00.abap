*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /NRK/APAYEDEF...................................*
DATA:  BEGIN OF STATUS_/NRK/APAYEDEF                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/NRK/APAYEDEF                 .
CONTROLS: TCTRL_/NRK/APAYEDEF
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */NRK/APAYEDEF                 .
TABLES: /NRK/APAYEDEF                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
