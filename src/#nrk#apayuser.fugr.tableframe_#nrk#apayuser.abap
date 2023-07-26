*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/NRK/APAYUSER
*   generation date: 10/26/2021 at 16:56:44 by user TORSTEN
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/NRK/APAYUSER      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
