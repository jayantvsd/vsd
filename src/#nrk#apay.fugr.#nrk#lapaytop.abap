FUNCTION-POOL /nrk/apay.                    "MESSAGE-ID ..

TYPE-POOLS vrm.

TABLES: swwwihead,
        esdus.

DATA: s_ctu_params     LIKE ctu_params.

DATA: BEGIN OF bdcdata OCCURS 10.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: BEGIN OF bdcmsgcoll OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF bdcmsgcoll.

DEFINE bdc_screen.
  clear bdcdata.
  bdcdata-program = &1.
  bdcdata-dynpro = &2.
  bdcdata-dynbegin = 'X'.
  append bdcdata.
END-OF-DEFINITION.

DEFINE bdc_dynpro.
  clear bdcdata.
  bdcdata-program = &1.
  bdcdata-dynpro = &2.
  bdcdata-dynbegin = 'X'.
  append bdcdata.
END-OF-DEFINITION.

DEFINE bdc_field.
  clear bdcdata.
  bdcdata-fnam = &1.
  bdcdata-fval = &2.
  append bdcdata.
END-OF-DEFINITION.

* ALV display
TYPE-POOLS : slis.
DATA: gv_field_pos         TYPE i,
      gt_alv_fieldcat_cmnt TYPE slis_t_fieldcat_alv,
      gt_header            TYPE slis_t_listheader.

DATA: t_history      LIKE /nrk/apayhis_dis OCCURS 0 WITH HEADER LINE,
      ok_code        LIKE sy-ucomm,
      apayno         LIKE /nrk/apayhd-apayno,
      except(40)     TYPE c,
      extype_list    TYPE vrm_values,
      extype_value   LIKE LINE OF extype_list,
      comment        LIKE /nrk/apaycmnt-process_comment,
      ebeln          LIKE ekko-ebeln,
      cancel         LIKE boole-boole,
      create         LIKE boole-boole,
      lifnr          LIKE lfa1-lifnr,
      wi_id          LIKE swwwihead-wi_id,
      wi_counter     LIKE sy-dbcnt.

DATA: BEGIN OF g_swwwihead OCCURS 0.
        INCLUDE STRUCTURE swwwihead.
DATA: END OF g_swwwihead.
