*&---------------------------------------------------------------------*
*& APay Center 2.0 - APay Center Goods Receipt update program
*&             2.1 - 07/08/2014
*& (c) by Norikkon, LLC 2014
*&---------------------------------------------------------------------*
report  /nrk/apaypaygrupdate message-id 00.

tables: /nrk/apayhd, /nrk/apayhis, /nrk/apaysdef, mseg.

types: begin of t_output,
         apayno  like /nrk/apayhd-apayno,
         bukrs   type bukrs,
         belnr   type belnr_d,
         gjahr   type gjahr,
         msg(50) type c,
       end of t_output.

data: lt_hd    like /nrk/apayhd occurs 1,
      ls_hd    like /nrk/apayhd,
      lt_his   like /nrk/apayhis occurs 1,
      ls_his   like /nrk/apayhis,
      lt_out   type t_output occurs 500,
      ls_out   type t_output,
      lv_subrc like sy-subrc.

data: lv_lines    type i,
      lt_thist    like /nrk/apayhis occurs 1,
      ls_addr3    type bapiaddr3,
      lt_ret2     type bapiret2 occurs 0,
      l_objectkey like  swr_struct-object_key.

type-pools: slis.

data: lt_fieldcatalog type slis_t_fieldcat_alv with header line,
      ls_layout       type slis_layout_alv,
      lv_repid        like sy-repid.

*CONSTANTS: c_bsc21    LIKE bseg-bschl     VALUE '21',
*           c_bsc31    LIKE bseg-bschl     VALUE '31'.

* -----------------------------------------------------------
* SELECTION SCREEN DEFINITION
* -----------------------------------------------------------
selection-screen begin of block procs with frame title text-001.

select-options sapayno for /nrk/apayhd-apayno.        "APay record number
select-options sbukrs  for /nrk/apayhd-bukrs.         "Company code
select-options slifnr  for /nrk/apayhd-lifnr.         "Vendor number
select-options sebeln  for /nrk/apayhd-ebeln.         "Purchase Order

selection-screen end of block procs.

selection-screen begin of block statusto with frame title text-002.

parameters: pswait like /nrk/apaysdef-status obligatory default '3110'.       "Status posted
selection-screen uline.
parameters: pspart like /nrk/apaysdef-status obligatory default '3040'.       "Status paid

selection-screen end of block statusto.

selection-screen begin of block workflow with frame title text-003.

parameter: workflow as checkbox.

selection-screen end of block workflow.

parameters: phits type i default 500.

* -----------------------------------------------------------
* AT SELECTION-SCREEN
* -----------------------------------------------------------
at selection-screen on pswait.
* Check input status
  select single * from /nrk/apaysdef where status eq pswait.
  if sy-subrc ne 0.
    message e398 with text-200 pswait text-300 space.
  endif.

at selection-screen on pspart.
* Check input status
  select single * from /nrk/apaysdef where status eq pspart.
  if sy-subrc ne 0.
    message e398 with text-200 pspart text-300 space.
  endif.

* -----------------------------------------------------------
* START OF SELECTION
* -----------------------------------------------------------
start-of-selection.
* Check all posted invoices in APay
  select * into table lt_hd from /nrk/apayhd up to phits rows
    where apayno in sapayno
      and ebeln in sebeln
      and bukrs in sbukrs
      and lifnr in slifnr
      and status eq pswait.

  loop at lt_hd into ls_hd.

* check if document has purchase order.
    if not ls_hd-ebeln is initial.

* Check if PO received GR
      perform check_gr_for_po.

    endif.

* Clear return value
    clear lv_subrc.

  endloop.

* Update tables
  read table lt_hd into ls_hd index 1.
  if sy-subrc eq 0.
    update /nrk/apayhd from table lt_hd.
    modify /nrk/apayhis from table lt_his.
    commit work.
  else.
    message e398 with text-305 space space space.
  endif.

* Display output
  perform build_fieldcatalog.
  perform build_layout.
  perform display_output.

  clear:   ls_hd, lt_hd, ls_his, lt_his, lt_out, ls_out.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_output .

  lv_repid = sy-repid.
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program = lv_repid
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcatalog[]
      i_save             = 'X'
    tables
      t_outtab           = lt_out
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
    message e398(00) with text-305 space space space.
  endif.

endform.                    " DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_fieldcatalog .

  clear: lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'APAYNO'.
  lt_fieldcatalog-seltext_m   = 'Apay record'.
  lt_fieldcatalog-col_pos     = 0.
  lt_fieldcatalog-outputlen   = 30.
  lt_fieldcatalog-emphasize   = 'X'.
  lt_fieldcatalog-key         = 'X'.
  append lt_fieldcatalog to lt_fieldcatalog.
  clear lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'BUKRS'.
  lt_fieldcatalog-seltext_m   = 'Company code'.
  lt_fieldcatalog-col_pos     = 1.
  append lt_fieldcatalog to lt_fieldcatalog.
  clear lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'BELNR'.
  lt_fieldcatalog-seltext_m   = 'Document number'.
  lt_fieldcatalog-col_pos     = 2.
  append lt_fieldcatalog to lt_fieldcatalog.
  clear lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'GJAHR'.
  lt_fieldcatalog-seltext_m   = 'Fiscal year'.
  lt_fieldcatalog-col_pos     = 3.
  append lt_fieldcatalog to lt_fieldcatalog.
  clear lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'MSG'.
  lt_fieldcatalog-seltext_m   = 'Message'.
  lt_fieldcatalog-col_pos     = 4.
  append lt_fieldcatalog to lt_fieldcatalog.
  clear lt_fieldcatalog.

endform.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_layout .

  ls_layout-no_input          = 'X'.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-totals_text       = 'Totals'.

endform.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_GR_FOR_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_gr_for_po .

* Check if goods receipt exists for purchase order
* 0 - Open GR exists
* 1 - No open GR exists

  data: lv_grexist    type syst-input,
        ls_ekbe       type ekbe,
        ls_ekko       type ekko,
        ls_ekpo       type ekpo,
        lv_menge      type menge_d,
        lv_menge_we   type menge_d,
        lv_menge_re   type menge_d,
        lv_shkzg      type shkzg.

* Clean up
  clear: lv_grexist, ls_ekbe, ls_ekko, ls_ekpo, lv_menge, lv_menge_we, lv_menge_re, lv_shkzg.

* Check if purchase order number is valid
  select single * from ekko into ls_ekko where ebeln eq ls_hd-ebeln.
  if sy-subrc eq 0.

* Check document type
    case ls_ekko-bstyp.

      when 'L'.
* Scheduling agreement / Lieferplan, check if GR quantity is less than RE quantity
        clear: lv_menge, lv_menge_we, lv_menge_re.
* Get GR quantity
        select shkzg menge into (lv_shkzg, lv_menge) from ekbe where ebeln eq ls_hd-ebeln and vgabe eq '1'.
          if lv_shkzg eq 'H'.
* Credit with negative amount
            lv_menge = lv_menge * ( -1 ).
          endif.
          lv_menge_we = lv_menge_we + lv_menge.
        endselect.
        clear: lv_menge, lv_shkzg.
* Get RE quantity
        select shkzg menge into (lv_shkzg, lv_menge)
          from ekbe where ebeln eq ls_hd-ebeln and vgabe eq '2'.
          if lv_shkzg eq 'H'.
* Credit with negative amount
            lv_menge = lv_menge * ( -1 ).
          endif.
          lv_menge_re = lv_menge_re + lv_menge.
        endselect.
        clear: lv_menge, lv_shkzg.
* Compare
        if lv_menge_we > lv_menge_re.
* Quantity delivered is larger than quantity invoiced, open GR exists
          lv_grexist = '0'.
        else.
* Quantity delivered is less or equal than quantity invoiced, no open GR exists
          lv_grexist = '1'.
        endif.

* Purchase order / Normalbestellung, check only if GR exists
      when others.
* Get purchase order history
        select single * from ekbe into ls_ekbe where ebeln eq ls_hd-ebeln
                                                 and vgabe eq '1'.
        if sy-subrc eq 0.
* Open goods receipt exists
          lv_grexist = '0'.
        else.
* No goods receipt exists
          lv_grexist = '1'.
        endif.

    endcase.
  endif.

* Open goods receipt exists
  if lv_grexist eq 0.
* Update APay Center Header
    move pspart to ls_hd-status.
    modify lt_hd from ls_hd.

* Update APay Center History
    select * from /nrk/apayhis into table lt_thist where apayno eq ls_hd-apayno.
* Process item number
    describe table lt_thist lines lv_lines.
* Set values
    clear ls_his.
    ls_his-apayno = ls_hd-apayno.
    ls_his-item   = lv_lines + 1.
    ls_his-sdate  = sy-datum.
    ls_his-stime  = sy-uzeit.
    ls_his-suser  = sy-uname.
    ls_his-status = pspart.
* Get full user name
    call function 'BAPI_USER_GET_DETAIL'
      exporting
        username = sy-uname
      importing
        address  = ls_addr3
      tables
        return   = lt_ret2.

    concatenate ls_addr3-firstname ls_addr3-lastname into ls_his-sname separated by space.
* Append history table
    append ls_his to lt_his.

* Trigger Workflow event
    if workflow eq 'X'.

      move ls_hd-apayno to l_objectkey.
      call function 'SAP_WAPI_CREATE_EVENT'
        exporting
          object_type             = '/NRK/APAY'
          object_key              = l_objectkey
          event                   = 'GoodsReceived'
          commit_work             = 'X'
          event_language          = sy-langu
*           LANGUAGE                = SY-LANGU
          user                    = sy-uname.

    endif.

  endif.

endform.                    " CHECK_GR_FOR_PO
