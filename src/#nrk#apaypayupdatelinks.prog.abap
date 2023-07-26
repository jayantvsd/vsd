*&---------------------------------------------------------------------*
*& Report  /NRK/APAYPAYUPDATELINKS
*&
*&---------------------------------------------------------------------*
*& (c)2014 norikkon, llc
*& Program verifies and re-creates links from APay images with
*& SAP accounting document
*&---------------------------------------------------------------------*
report  /nrk/apaypayupdatelinks.

tables: /nrk/apayhd, toaom.

data: t_hd           like /nrk/apayhd occurs 0,
      wa_hd          like /nrk/apayhd,
      t_connections  like toav0 occurs 0,
      t_connections_bkpf  like toav0 occurs 0,
      t_links        like toav0 occurs 0,
      wa_connections like toav0,
      wa_connections_bkpf like toav0,
      wa_links       like toav0,
      object_id_bkpf      like toav0-object_id,
      object_id      like toav0-object_id.

* -----------------------------------------------------------
* SELECTION SCREEN DEFINITION
* -----------------------------------------------------------
selection-screen begin of block apay with frame title text-001.

select-options sapayno for /nrk/apayhd-apayno.        "APay record number

selection-screen end of block apay.

selection-screen begin of block bkpf with frame title text-002.

select-options sbukrs  for /nrk/apayhd-bukrs.         "Company code
select-options sbelnr  for /nrk/apayhd-belnr.         "Document number
select-options sgjahr  for /nrk/apayhd-gjahr.         "Fiscal year

selection-screen end of block bkpf.

parameters: phits type i default 500.

* -----------------------------------------------------------
* START OF SELECTION
* -----------------------------------------------------------
* Clean up
clear: t_hd, wa_hd, t_connections, t_connections_bkpf, t_links, wa_connections, wa_connections_bkpf, wa_links, object_id_bkpf, object_id.

* Get records
select * into table t_hd from /nrk/apayhd up to phits rows
  where apayno in sapayno
    and bukrs  in sbukrs
    and belnr  in sbelnr
    and gjahr  in sgjahr.

* Process records
loop at t_hd into wa_hd.

* Check for blank records first
  if not wa_hd-belnr is initial and not wa_hd-bukrs is initial and not wa_hd-gjahr is initial.
    clear: object_id_bkpf.
    concatenate wa_hd-bukrs wa_hd-belnr wa_hd-gjahr into object_id_bkpf.

* Get current links
    call function 'ARCHIV_GET_CONNECTIONS'
      exporting
        objecttype    = 'BKPF'
        object_id     = object_id_bkpf
        client        = sy-mandt
        documenttype  = wa_hd-ar_object
      tables
        connections   = t_connections_bkpf
      exceptions
        nothing_found = 1
        others        = 2.

* Check current links
    read table t_connections_bkpf into wa_connections_bkpf with key arc_doc_id = space.
    if sy-subrc ne 0.
* No empty records found
      write:/ 'No empty links found for', object_id_bkpf.
    endif.

* Empty links exist, correct
*    concatenate wa_hd-bukrs wa_hd-belnr wa_hd-gjahr into object_id.
    object_id = wa_hd-apayno.
    call function 'ARCHIV_GET_CONNECTIONS'
      exporting
        objecttype    = '/NRK/APAY'
        object_id     = object_id
        client        = sy-mandt
        documenttype  = wa_hd-ar_object
      tables
        connections   = t_connections
      exceptions
        nothing_found = 1
        others        = 2.

    if sy-subrc <> 0.
      write:/ 'No images found for /NRK/APAY', object_id.
      continue.
    else.
* Compare links and correct
      loop at t_connections into wa_connections.
        read table t_connections_bkpf into wa_connections_bkpf with key archiv_id = wa_connections-archiv_id arc_doc_id = wa_connections-arc_doc_id.
        if sy-subrc ne 0.
* Link is missing in BKPF, correct
          move-corresponding wa_connections to wa_links.
          wa_links-sap_object = 'BKPF'.
          wa_links-object_id  = object_id_bkpf.
          append wa_links to t_links.
* Write output
          write:/ 'Updating ', wa_links-object_id, 'with', wa_links-archiv_id, wa_links-arc_doc_id.
        endif.
      endloop.
    endif.
  endif.

endloop.

* Clean up
clear: wa_links.

* Search complete, now write to DB
call function 'ARCHIV_CONNECTIONTABLE_INSERT'
  tables
    entrys_connectiontable = t_links
  exceptions
    no_entry_possible      = 1
    others                 = 2.

if sy-subrc <> 0.
  message s398(00) with 'Error writing to database'.
else.
* Remove duplicate keys
  delete adjacent duplicates from t_links comparing object_id.
* Delete empty records
  loop at t_links into wa_links.
    select single * from toaom where sap_object eq 'BKPF' and ar_object eq wa_links-ar_object and ar_status eq 'X'.
    if sy-subrc eq 0.
* Delete from connection table
      delete from (toaom-connection) where sap_object eq 'BKPF' and ar_object eq wa_links-ar_object and object_id eq wa_links-object_id and arc_doc_id eq space.
      if sy-subrc eq 0.
        write:/ 'Deleted empty record for', wa_links-object_id, 'from', toaom-connection.
      endif.
    endif.
  endloop.
endif.

* All done
commit work.
