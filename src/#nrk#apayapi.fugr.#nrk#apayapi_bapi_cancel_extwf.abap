FUNCTION /nrk/apayapi_bapi_cancel_extwf.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXCEPTIONS
*"      NO_SHAREPOINT_WORKFLOW
*"      CANCELLATION_FAILED
*"----------------------------------------------------------------------

  TYPE-POOLS: abap.

  DATA: func TYPE string,
        ptab TYPE abap_func_parmbind_tab,
        ptab_line TYPE abap_func_parmbind,
        etab TYPE abap_func_excpbind_tab,
        etab_line TYPE abap_func_excpbind.

* Check, if sharepoint workflows exist
  CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
    EXPORTING
      function_module       = '/NRK/APAYEXTWFCANCEL'
* IMPORTING
*   TFDIR_INFO            =
    EXCEPTIONS
      not_existent          = 1
      OTHERS                = 2.

  IF sy-subrc = 0.

    ptab_line-name = 'APAYNO'.
    ptab_line-kind = abap_func_exporting.
    GET REFERENCE OF apayno INTO ptab_line-value.
    INSERT ptab_line INTO TABLE ptab.

    MOVE '/NRK/APAYEXTWFCANCEL' TO func.

    CALL FUNCTION func
      PARAMETER-TABLE
        ptab
      EXCEPTION-TABLE
        etab.

    IF sy-subrc NE 0.
      RAISE cancellation_failed.
    ENDIF.

  ENDIF.


ENDFUNCTION.
