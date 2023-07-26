FUNCTION /NRK/APAYPARKBUS2081.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYHD) TYPE  /NRK/APAYHD OPTIONAL
*"  EXPORTING
*"     VALUE(PARKED) TYPE  BOOLE-BOOLE
*"     VALUE(LIV_BELNR) TYPE  RE_BELNR
*"     VALUE(LIV_GJAHR) TYPE  GJAHR
*"  TABLES
*"      ITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"      TAB_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      PARKING_FAILED
*"      RECORD_NOT_FOUND
*"--------------------------------------------------------------------

*  DATA: l_amount(16),
*        l_tax(16),
*        l_item_amount(16),
*        l_bldat(10),
*        l_zfbdt(10),
*        l_vendor LIKE invfo-accnt,
*        l_inv_type(1) TYPE c,
*        l_date(10)       TYPE c,
*        l_tax_amount(10) TYPE c,
*        l_field(10)      TYPE c,
*        l_pid_bukrs      LIKE bkpf-bukrs,
*        l_bukrs          LIKE bkpf-bukrs,
*        l_belnr          LIKE bkpf-belnr,
*        l_gjahr          LIKE bkpf-gjahr,
*        taxcode     LIKE t169v-vstki.


  DATA: item                LIKE bapi_incinv_create_item OCCURS 0,
        wa_item             LIKE bapi_incinv_create_item,
        tab_accountingdata  LIKE bapi_incinv_create_account OCCURS 0,
        tab_glaccountdata   LIKE bapi_incinv_create_gl_account OCCURS 0,
        tab_materialdata    LIKE bapi_incinv_create_material OCCURS 0,
        tab_taxdata         LIKE bapi_incinv_create_tax OCCURS 0,
        tab_withtaxdata     LIKE bapi_incinv_create_withtax OCCURS 0,
        tab_vendorsplitdata LIKE bapi_incinv_create_vendorsplit OCCURS 0,
*       tab_return          LIKE bapiret2 OCCURS 0,
*       s_addressdata       LIKE bapi_incinv_create_addressdata,
        header              LIKE bapi_incinv_create_header,
        wa_hd               LIKE /nrk/apayhd,
        t_lineitem              LIKE /nrk/apayitems OCCURS 0,
        wa_lineitem             LIKE /nrk/apayitems.

  CLEAR: liv_belnr,
         liv_gjahr,
         wa_lineitem,
         t_lineitem[].

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayhd-apayno.

  IF sy-subrc NE 0.
    RAISE record_not_found.
  ENDIF.

* Header Data
  header-doc_date = wa_hd-bldat.
  header-pstng_date = sy-datum.
  header-comp_code = wa_hd-bukrs.
  header-gross_amount = wa_hd-wrbtr.
  header-currency = wa_hd-waers.

  IF wa_hd-shkzg = 'S'.
    header-invoice_ind = 'X'.
  ELSE.
    header-invoice_ind = ' '.
  ENDIF.

  IF wa_hd-wmwst IS INITIAL.
    header-calc_tax_ind = 'X'.
  ELSE.
    header-calc_tax_ind = 'X'.
  ENDIF.

* Item Data
  SELECT * FROM /nrk/apayitems INTO TABLE t_lineitem
    WHERE apayno EQ wa_hd-apayno.

  IF sy-subrc EQ 0.

    LOOP AT t_lineitem INTO wa_lineitem.

      wa_item-invoice_doc_item = wa_lineitem-buzei.
      wa_item-po_number = wa_hd-ebeln.
      wa_item-po_item = wa_lineitem-ebelp.
      wa_item-tax_code = wa_lineitem-mwskz.
      wa_item-item_amount = wa_lineitem-wrbtr.

      IF NOT wa_lineitem-menge IS INITIAL.
        wa_item-quantity = wa_lineitem-menge.
      ENDIF.

      wa_item-po_unit = 'ST'.
*     wa_item-gross_amount = wa_lineitem-wrbtr.

*      IF NOT wa_lineitem-meins IS INITIAL.
*        wa_item-po_unit = wa_lineitem-meins.
*      ENDIF.

      APPEND wa_item TO item.

    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
    EXPORTING
      headerdata                = header
*   ADDRESSDATA               =
    IMPORTING
      invoicedocnumber          = liv_belnr
      fiscalyear                = liv_gjahr
    TABLES
      itemdata                  = item
*   ACCOUNTINGDATA            =
*   GLACCOUNTDATA             =
*   MATERIALDATA              =
*   TAXDATA                   =
*   WITHTAXDATA               =
*   VENDORITEMSPLITDATA       =
      return                    = tab_return.
*   EXTENSIONIN               =



ENDFUNCTION.
