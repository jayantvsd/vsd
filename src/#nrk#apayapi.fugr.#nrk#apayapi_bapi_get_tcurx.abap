function /nrk/apayapi_bapi_get_tcurx.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      TCURX STRUCTURE  TCURX
*"  EXCEPTIONS
*"      NO_ENTRIES_IN_TABLE
*"----------------------------------------------------------------------

* Get data
  select * from tcurx into corresponding fields of table tcurx.

* Error handling
  if sy-subrc ne 0.
    raise no_entries_in_table.
  endif.

endfunction.
