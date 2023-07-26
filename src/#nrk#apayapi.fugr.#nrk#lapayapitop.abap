FUNCTION-POOL /nrk/apayapi.                 "MESSAGE-ID ..

* Types
TYPES: BEGIN OF t_select,
         sign(1)   TYPE c,
         option(2) TYPE c,
         low(40)   TYPE c,
         high(40)  TYPE c.
TYPES: END OF t_select.

TABLES: vbsegs.

* Data
DATA: gs_apayno TYPE t_select,
      gt_apayno TYPE t_select OCCURS 0,
      gs_lifname TYPE t_select,
      gt_lifname TYPE t_select OCCURS 0,
      gs_lifnr   TYPE t_select,
      gt_lifnr   TYPE t_select OCCURS 0,
      gs_xblnr   TYPE t_select,
      gt_xblnr   TYPE t_select OCCURS 0,
      gs_bldat   TYPE t_select,
      gt_bldat   TYPE t_select OCCURS 0,
      gs_wrbtr   TYPE t_select,
      gt_wrbtr   TYPE t_select OCCURS 0,
      gs_waers   TYPE t_select,
      gt_waers   TYPE t_select OCCURS 0,
      gs_duedate TYPE t_select,
      gt_duedate TYPE t_select OCCURS 0,
      gs_budat   TYPE t_select,
      gt_budat   TYPE t_select OCCURS 0,
      gs_belnr   TYPE t_select,
      gt_belnr   TYPE t_select OCCURS 0,
      gs_bukrs   TYPE t_select,
      gt_bukrs   TYPE t_select OCCURS 0,
      gs_gjahr   TYPE t_select,
      gt_gjahr   TYPE t_select OCCURS 0,
      gs_status  TYPE t_select,
      gt_status  TYPE t_select OCCURS 0,
      gs_ebeln  TYPE t_select,
      gt_ebeln  TYPE t_select OCCURS 0.

DATA: message_test LIKE /nrk/apayapi_messages.

* Constants
CONSTANTS: c_i(1)     TYPE c VALUE 'I',
           c_bt(2)    TYPE c VALUE 'BT',
           c_eq(2)    TYPE c VALUE 'EQ',
           c_cp(2)    TYPE c VALUE 'CP'.
