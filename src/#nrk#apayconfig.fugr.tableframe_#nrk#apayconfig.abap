*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/NRK/APAYCONFIG
*   generation date: 06/03/2015 at 15:19:07 by user TORSTEN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/NRK/APAYCONFIG    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
