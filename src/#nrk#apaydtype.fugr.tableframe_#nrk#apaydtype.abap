*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/NRK/APAYDTYPE
*   generation date: 10/20/2021 at 17:39:38 by user TORSTEN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/NRK/APAYDTYPE     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
