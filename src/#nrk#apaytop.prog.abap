*&---------------------------------------------------------------------*
*&  Include           /NRK/APAYTOP

*&---------------------------------------------------------------------*
* Class for event receiver definition
CLASS cl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
    handle_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,
    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,
    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.                    "cl_event_receiver DEFINITION

TABLES: /nrk/apayhd, /nrk/apaycmnt, bkpf, bseg, ekko.

DATA: t_apayhd_dis     TYPE TABLE OF /nrk/apayhd_dis,
      wa_dis           TYPE /nrk/apayhd_dis,
      wt_dis_seltab    TYPE TABLE OF /nrk/apayhd_dis,
      t_apay_his       TYPE TABLE OF /nrk/apay_his,
      wa_his           TYPE /nrk/apay_his,
      t_apayitem       TYPE TABLE OF /nrk/apayitems,
      wa_item          TYPE /nrk/apayitems,
      t_apaycmnt       TYPE TABLE OF /nrk/apaycmnt,
      wa_cmnt          TYPE /nrk/apaycmnt,
      ddtext           LIKE dd07v-ddtext,
      domvalue         LIKE dd07v-domvalue_l,
      all_docs         LIKE boole-boole,
      limit_docs       LIKE boole-boole,
      tab_user_bukrs   LIKE /nrk/apayuser OCCURS 0,
      wa_user_bukrs    LIKE /nrk/apayuser,
      cancel           LIKE boole-boole.

DATA: g_view(1)        TYPE n,
      g_count          TYPE i.

DATA: alv_container    TYPE REF TO cl_gui_custom_container,
      alv_doccontainer TYPE REF TO cl_gui_docking_container,
      alv_grid         TYPE REF TO cl_gui_alv_grid,
      alv_cc_name      TYPE scrfname VALUE 'alv_grid',
      alv_top          TYPE REF TO cl_dd_document,
      alv_bottom       TYPE REF TO cl_dd_document,
      event_receiver   TYPE REF TO cl_event_receiver,
      variant          TYPE disvariant,
      layout           TYPE lvc_s_layo,
      ok_code          TYPE sy-ucomm,
      t_fieldcat       TYPE TABLE OF lvc_s_fcat.

DATA: t_status         TYPE TABLE OF /nrk/apaysdef,
      wa_status        TYPE /nrk/apaysdef.

DATA: minutes TYPE i,
      hours   TYPE i,
      days    TYPE i.

DATA: basedate LIKE sy-datum,
      basetime LIKE sy-uzeit.

* ALV status history
DATA: alvhis_container    TYPE REF TO cl_gui_custom_container,
      alvhis_doccontainer TYPE REF TO cl_gui_docking_container,
      alvhis_grid         TYPE REF TO cl_gui_alv_grid,
      alvhis_cc_name      TYPE scrfname VALUE 'alvhis_grid',
      alvhis_top          TYPE REF TO cl_dd_document,
      alvhis_bottom       TYPE REF TO cl_dd_document,
      varianthis          TYPE disvariant,
      layouthis           TYPE lvc_s_layo.

* ALV line items
DATA: alvitem_container    TYPE REF TO cl_gui_custom_container,
      alvitem_doccontainer TYPE REF TO cl_gui_docking_container,
      alvitem_grid         TYPE REF TO cl_gui_alv_grid,
      alvitem_cc_name      TYPE scrfname VALUE 'alvitem_grid',
      alvitem_top          TYPE REF TO cl_dd_document,
      alvitem_bottom       TYPE REF TO cl_dd_document,
      variantitem          TYPE disvariant,
      layoutitem           TYPE lvc_s_layo.

* ALV class implementation
DATA: go_event_processor   TYPE REF TO cl_event_receiver.
