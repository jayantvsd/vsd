*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /NRK/APAYSDEF...................................*
DATA:  BEGIN OF STATUS_/NRK/APAYSDEF                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/NRK/APAYSDEF                 .
CONTROLS: TCTRL_/NRK/APAYSDEF
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */NRK/APAYSDEF                 .
TABLES: /NRK/APAYSDEF                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
