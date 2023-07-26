FUNCTION /nrk/apay_display_comment.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_APAYNO) TYPE  /NRK/APAYNO
*"  EXCEPTIONS
*"      ENTRY_NOT_FOUND
*"      DB_INSERT_FAILED
*"----------------------------------------------------------------------

* Local data declaration
  DATA: lt_apaycmnt TYPE /nrk/apaycmnt OCCURS 0,
        ls_apaycmnt TYPE /nrk/apaycmnt.
  DATA: lv_end_col    TYPE c LENGTH 3,
        ls_alv_layout TYPE slis_layout_alv.

* Get comments for record
  SELECT * FROM /nrk/apaycmnt INTO CORRESPONDING FIELDS OF TABLE lt_apaycmnt WHERE apayno = iv_apayno.
  IF sy-subrc <> 0.
    MESSAGE e499(sy) WITH text-104 iv_apayno.
    EXIT.
  ENDIF.

* Sort output by date and time
  SORT lt_apaycmnt BY crea_date DESCENDING crea_time DESCENDING.
  REFRESH: gt_alv_fieldcat_cmnt.

* Set fields
  PERFORM build_lst_table_cmnt  USING 10   text-106 'CREA_DATE' space.
  PERFORM build_lst_table_cmnt  USING 8    text-107 'CREA_TIME' space.
  PERFORM build_lst_table_cmnt  USING 15   text-109 'USER_NAME' space.
* perform build_lst_table_cmnt  using 100  text-108 'PROCESS_COMMENT' space.
  PERFORM build_lst_table_cmnt  USING 255  text-108 'PROCESS_COMMENT' space.
* perform build_lst_table_cmnt  using 15   text-109 'USER_NAME' space.
  PERFORM build_lst_table_cmnt  USING 10   text-110 'USER_ID' space.

* Set layout
  ls_alv_layout-zebra           = 'X'.
  CONCATENATE text-108 '-' text-105 iv_apayno INTO ls_alv_layout-window_titlebar SEPARATED BY space.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*      i_callback_program     = sy-repid
*      i_callback_top_of_page = 'TOP_OF_PAGE'
      is_layout              = ls_alv_layout
      it_fieldcat            = gt_alv_fieldcat_cmnt
*      i_save                 = 'A'
      i_screen_start_column  = '8'
      i_screen_start_line    = '5'
      i_screen_end_column    = '155'
      i_screen_end_line      = '20'
    TABLES
      t_outtab               = lt_apaycmnt
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFUNCTION.
