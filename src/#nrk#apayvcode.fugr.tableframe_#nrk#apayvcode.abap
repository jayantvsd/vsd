*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/NRK/APAYVCODE
*   generation date: 09/19/2018 at 17:06:31 by user TORSTEN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/NRK/APAYVCODE     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
