*&---------------------------------------------------------------------*
*&  Include           /NRK/APAYSELECT
*&---------------------------------------------------------------------*

* selection screen
SELECTION-SCREEN BEGIN OF BLOCK basic WITH FRAME TITLE  text-002.

*parameters : p_ledg radiobutton group rd1 default 'X',
*             p_bat radiobutton group rd1,
*             p_wfs radiobutton group rd1,
*             p_docs radiobutton group rd1.


SELECTION-SCREEN END OF BLOCK basic.

SELECTION-SCREEN BEGIN OF BLOCK header WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  s_apayno   FOR /nrk/apayhd-apayno,
  s_arobj    FOR /nrk/apayhd-ar_object,
  s_cstat    FOR /nrk/apayhd-status,
  s_cdate    FOR bkpf-budat,
  s_hstat    FOR /nrk/apayhd-status,
  s_extu     FOR /nrk/apayhd-ext_approver.

SELECTION-SCREEN END OF BLOCK header.

SELECTION-SCREEN BEGIN OF BLOCK document WITH FRAME TITLE text-003.
SELECT-OPTIONS:
    s_bukrs   FOR bkpf-bukrs,
    s_belnr   FOR bkpf-belnr,
    s_livbl   FOR /nrk/apayhd-liv_belnr,
    s_gjahr   FOR bkpf-gjahr.
SELECTION-SCREEN ULINE.
SELECT-OPTIONS:
    s_bldat   FOR bkpf-bldat,
    s_xblnr   FOR /nrk/apayhd-xblnr,
    s_wrbtr   FOR bseg-wrbtr,
    s_waers   FOR bkpf-waers,
    s_dueda   FOR /nrk/apayhd-duedate,
    s_budat   FOR bkpf-budat.
SELECTION-SCREEN ULINE.
SELECT-OPTIONS:
    s_lname   FOR /nrk/apayhd-lifname,
    s_lifnr   FOR /nrk/apayhd-lifnr MATCHCODE OBJECT kred,
    s_ebeln   FOR ekko-ebeln,
    s_ekgrp   FOR ekko-ekgrp,
    s_ernam   FOR ekko-ernam.
SELECTION-SCREEN END OF BLOCK document.

* PARAMETERS: p_kpi AS CHECKBOX DEFAULT ' ' .

PARAMETERS: p_max TYPE i DEFAULT 500,
            p_vari     TYPE slis_vari.
