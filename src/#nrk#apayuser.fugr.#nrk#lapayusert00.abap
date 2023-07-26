*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /NRK/APAYUSER...................................*
DATA:  BEGIN OF STATUS_/NRK/APAYUSER                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/NRK/APAYUSER                 .
CONTROLS: TCTRL_/NRK/APAYUSER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */NRK/APAYUSER                 .
TABLES: /NRK/APAYUSER                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
