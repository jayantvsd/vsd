*----------------------------------------------------------------------*
***INCLUDE /NRK/LAPAYAPIFRM .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_SELECTION_PARS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_selection_pars USING
                      p_apaynofr
                      p_apaynoto
                      p_lifnrfr
                      p_lifnrto
                      p_lifnamefr
                      p_lifnameto
                      p_xblnrfr
                      p_xblnrto
                      p_bldatfr
                      p_bldatto
                      p_wrbtrfr
                      p_wrbtrto
                      p_waersfr
                      p_waersto
                      p_duedatefr
                      p_duedateto
                      p_budatfr
                      p_budatto
                      p_belnrfr
                      p_belnrto
                      p_bukrsfr
                      p_bukrsto
                      p_gjahrfr
                      p_gjahrto
                      p_statusfr
                      p_statusto
                      p_ebelnfr
                      p_ebelnto.

* APay Center record number
  IF p_apaynoto NE space.                   "Set between search
    gs_apayno-sign   = c_i.
    gs_apayno-option = c_bt.
    gs_apayno-low    = p_apaynofr.
    gs_apayno-high   = p_apaynoto.
    APPEND gs_apayno TO gt_apayno.
  ELSEIF p_apaynofr NE space.               "Set low search
    gs_apayno-sign   = c_i.
    gs_apayno-option = c_eq.
    gs_apayno-low    = p_apaynofr.
    APPEND gs_apayno TO gt_apayno.
  ENDIF.

** Vendor number
  IF p_lifnrto NE space.                   "Set between search
    gs_lifnr-sign   = c_i.
    gs_lifnr-option = c_bt.
    gs_lifnr-low    = p_lifnrfr.
    gs_lifnr-high   = p_lifnrto.
    APPEND gs_lifnr TO gt_lifnr.
  ELSEIF p_lifnrfr NE space.               "Set low search
    gs_lifnr-sign   = c_i.
    gs_lifnr-option = c_eq.
    gs_lifnr-low    = p_lifnrfr.
    APPEND gs_lifnr TO gt_lifnr.
  ENDIF.

** Vendor name
  IF p_lifnameto NE space.                   "Set between search
    CONCATENATE p_lifnameto '*' INTO p_lifnameto.
    CONCATENATE p_lifnamefr '*' INTO p_lifnamefr.
    TRANSLATE p_lifnameto TO UPPER CASE.
    TRANSLATE p_lifnamefr TO UPPER CASE.

    gs_lifname-sign   = c_i.
    gs_lifname-option = c_bt.
    gs_lifname-low    = p_lifnamefr.
    gs_lifname-high   = p_lifnameto.
    APPEND gs_lifname TO gt_lifname.
  ELSEIF p_lifnamefr NE space.               "Set low search
    TRANSLATE p_lifnamefr TO UPPER CASE.
    CONCATENATE p_lifnamefr '*' INTO p_lifnamefr.
    gs_lifname-sign   = c_i.
    gs_lifname-option = c_cp.
    gs_lifname-low    = p_lifnamefr.
    APPEND gs_lifname TO gt_lifname.
  ENDIF.

** External invoice number
  IF p_xblnrto NE space.                   "Set between search
    gs_xblnr-sign   = c_i.
    gs_xblnr-option = c_bt.
    gs_xblnr-low    = p_xblnrfr.
    gs_xblnr-high   = p_xblnrto.
    APPEND gs_xblnr TO gt_xblnr.
  ELSEIF p_xblnrfr NE space.               "Set low search
    gs_xblnr-sign   = c_i.
    gs_xblnr-option = c_eq.
    gs_xblnr-low    = p_xblnrfr.
    APPEND gs_xblnr TO gt_xblnr.
  ENDIF.

** Document date
  IF NOT p_bldatto IS INITIAL.                   "Set between search
    gs_bldat-sign   = c_i.
    gs_bldat-option = c_bt.
    gs_bldat-low    = p_bldatfr.
    gs_bldat-high   = p_bldatto.
    APPEND gs_bldat TO gt_bldat.
  ELSEIF NOT p_bldatfr IS INITIAL.               "Set low search
    gs_bldat-sign   = c_i.
    gs_bldat-option = c_eq.
    gs_bldat-low    = p_bldatfr.
    APPEND gs_bldat TO gt_bldat.
  ENDIF.

** Amount
  IF p_wrbtrto NE space
    OR p_wrbtrto NE 0.           "Set between search
    gs_wrbtr-sign   = c_i.
    gs_wrbtr-option = c_bt.
    gs_wrbtr-low    = p_wrbtrfr.
    gs_wrbtr-high   = p_wrbtrto.
    APPEND gs_wrbtr TO gt_wrbtr.
  ELSEIF p_wrbtrfr NE space
    OR p_wrbtrfr NE 0.            "Set low search
    gs_wrbtr-sign   = c_i.
    gs_wrbtr-option = c_eq.
    gs_wrbtr-low    = p_wrbtrfr.
    APPEND gs_wrbtr TO gt_wrbtr.
  ENDIF.

** Currency
  IF p_waersto NE space.                   "Set between search
    gs_waers-sign   = c_i.
    gs_waers-option = c_bt.
    gs_waers-low    = p_waersfr.
    gs_waers-high   = p_waersto.
    APPEND gs_waers TO gt_waers.
  ELSEIF p_waersfr NE space.               "Set low search
    gs_waers-sign   = c_i.
    gs_waers-option = c_eq.
    gs_waers-low    = p_waersfr.
    APPEND gs_waers TO gt_waers.
  ENDIF.

** Due date
  IF NOT p_duedateto IS INITIAL.                   "Set between search
    gs_duedate-sign   = c_i.
    gs_duedate-option = c_bt.
    gs_duedate-low    = p_duedatefr.
    gs_duedate-high   = p_duedateto.
    APPEND gs_duedate TO gt_duedate.
  ELSEIF NOT p_duedatefr IS INITIAL.               "Set low search
    gs_duedate-sign   = c_i.
    gs_duedate-option = c_eq.
    gs_duedate-low    = p_apaynofr.
    APPEND gs_duedate TO gt_duedate.
  ENDIF.

*** Posting date
  IF NOT p_budatto IS INITIAL.
    gs_budat-sign   = c_i.
    gs_budat-option = c_bt.
    gs_budat-low    = p_budatfr.
    gs_budat-high   = p_budatto.
    APPEND gs_budat TO gt_budat.
  ELSEIF NOT p_budatfr IS INITIAL.
    gs_budat-sign   = c_i.
    gs_budat-option = c_eq.
    gs_budat-low    = p_budatfr.
    APPEND gs_budat TO gt_budat.
  ENDIF.

** SAP document number
  IF p_belnrto NE space.                   "Set between search
    gs_belnr-sign   = c_i.
    gs_belnr-option = c_bt.
    gs_belnr-low    = p_belnrfr.
    gs_belnr-high   = p_belnrto.
    APPEND gs_belnr TO gt_belnr.
  ELSEIF p_belnrfr NE space.               "Set low search
    gs_belnr-sign   = c_i.
    gs_belnr-option = c_eq.
    gs_belnr-low    = p_belnrfr.
    APPEND gs_belnr TO gt_belnr.
  ENDIF.

** Company Code
  IF p_bukrsto NE space.                   "Set between search
    gs_bukrs-sign   = c_i.
    gs_bukrs-option = c_bt.
    gs_bukrs-low    = p_bukrsfr.
    gs_bukrs-high   = p_bukrsto.
    APPEND gs_bukrs TO gt_bukrs.
  ELSEIF p_bukrsfr NE space.               "Set low search
    gs_bukrs-sign   = c_i.
    gs_bukrs-option = c_eq.
    gs_bukrs-low    = p_bukrsfr.
    APPEND gs_bukrs TO gt_bukrs.
  ENDIF.

** Fiscal Year
  IF p_gjahrto NE space.                   "Set between search
    gs_gjahr-sign   = c_i.
    gs_gjahr-option = c_bt.
    gs_gjahr-low    = p_gjahrfr.
    gs_gjahr-high   = p_gjahrto.
    APPEND gs_gjahr TO gt_gjahr.
  ELSEIF p_gjahrfr NE space.               "Set low search
    gs_gjahr-sign   = c_i.
    gs_gjahr-option = c_eq.
    gs_gjahr-low    = p_gjahrfr.
    APPEND gs_gjahr TO gt_gjahr.
  ENDIF.

** Status
  IF p_statusto NE space.                   "Set between search
    gs_status-sign   = c_i.
    gs_status-option = c_bt.
    gs_status-low    = p_statusfr.
    gs_status-high   = p_statusto.
    APPEND gs_status TO gt_status.
  ELSEIF p_statusfr NE space.               "Set low search
    gs_status-sign   = c_i.
    gs_status-option = c_eq.
    gs_status-low    = p_statusfr.
    APPEND gs_status TO gt_status.
  ENDIF.

** Purchase order
  IF p_ebelnto NE space.                   "Set between search
    gs_ebeln-sign   = c_i.
    gs_ebeln-option = c_bt.
    gs_ebeln-low    = p_ebelnfr.
    gs_ebeln-high   = p_ebelnto.
    APPEND gs_ebeln TO gt_ebeln.
  ELSEIF p_ebelnfr NE space.               "Set low search
    gs_ebeln-sign   = c_i.
    gs_ebeln-option = c_eq.
    gs_ebeln-low    = p_ebelnfr.
    APPEND gs_ebeln TO gt_ebeln.
  ENDIF.

ENDFORM.                    " CREATE_SELECTION_PARS
