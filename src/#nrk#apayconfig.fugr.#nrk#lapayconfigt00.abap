*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /NRK/APAYCONFIG.................................*
DATA:  BEGIN OF STATUS_/NRK/APAYCONFIG               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/NRK/APAYCONFIG               .
CONTROLS: TCTRL_/NRK/APAYCONFIG
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: */NRK/APAYCONFIG               .
TABLES: /NRK/APAYCONFIG                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
