*&---------------------------------------------------------------------*
*& Report  /NRK/APAYPAYUSERUPLOAD
*&
*&---------------------------------------------------------------------*
*& APay Center 2.1 - APay Center user upload program
*& (c) by Norikkon, LLC 2014
*&---------------------------------------------------------------------*
REPORT  /nrk/apaypayuserupload MESSAGE-ID 00.

DATA: i_tab   LIKE alsmex_tabline OCCURS 0,
      wa_tab  LIKE alsmex_tabline,
      t_user  LIKE /nrk/apayuser OCCURS 0,
      wa_user LIKE /nrk/apayuser,
      lines   TYPE i.

CLEAR: t_user[],
       wa_user,
       i_tab[],
       wa_tab.


SELECTION-SCREEN BEGIN OF BLOCK file WITH FRAME TITLE text-001.

PARAMETERS: filename LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\TEMP\APAY_USER_UPLOAD.XLS'.

SELECTION-SCREEN END OF BLOCK file.

CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  EXPORTING
    filename                = filename
    i_begin_col             = '1'
    i_begin_row             = '1'
    i_end_col               = '18'
    i_end_row               = '1000'
  TABLES
    intern                  = i_tab
  EXCEPTIONS
    inconsistent_parameters = 1
    upload_ole              = 2
    OTHERS                  = 3.

IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

LOOP AT i_tab INTO wa_tab.

  CASE wa_tab-col.
    WHEN '0001'. "Company code
      MOVE wa_tab-value TO wa_user-bukrs.
    WHEN '0002'. "Object type
      MOVE wa_tab-value TO wa_user-otype.
    WHEN '0003'. "Agent ID
      MOVE wa_tab-value TO wa_user-objid.
    WHEN '0004'. "Full name
      MOVE wa_tab-value TO wa_user-fullname.
    WHEN '0005'. " External user ID
      MOVE wa_tab-value TO wa_user-extuser.
    WHEN '0006'. " Email address
      MOVE wa_tab-value TO wa_user-smtp_addr.
    WHEN '0007'. " Object type of manager
      MOVE wa_tab-value TO wa_user-motype.
    WHEN '0008'. " Agent ID of manager
      MOVE wa_tab-value TO wa_user-mobjid.
    WHEN '0009'. " External user ID of manager
      MOVE wa_tab-value TO wa_user-mextuser.
    WHEN '0010'. " Administrator role
      MOVE wa_tab-value TO wa_user-admin.
    WHEN '0011'. " AP processor role
      MOVE wa_tab-value TO wa_user-processor.
    WHEN '0012'. " AP manager role
      MOVE wa_tab-value TO wa_user-manager.
    WHEN '0013'. " Approver role
      MOVE wa_tab-value TO wa_user-approver.
    WHEN '0014'. " Coder role
      MOVE wa_tab-value TO wa_user-coder.
    WHEN '0015'. " Buyer role
      MOVE wa_tab-value TO wa_user-buyer.
    WHEN '0016'. " Receiver role
      MOVE wa_tab-value TO wa_user-receiver.
    WHEN '0017'. " Amount threshold
      MOVE wa_tab-value TO wa_user-wrbtr.
    WHEN '0018'. " Cost center
      MOVE wa_tab-value TO wa_user-kostl.
  ENDCASE.

  AT END OF row.

    APPEND wa_user TO t_user.
    CLEAR: wa_user.

* At end of row TBU_UPLOAD is updated with the desired value i.e
* When Row Column in table one converted  from 1 to 2 , the first column in Table 2 is inserted.
  ENDAT.
*       APPEND wa_user TO t_user.

ENDLOOP.

DESCRIBE TABLE t_user LINES lines.

IF lines GT 0.
* User from xls uploaded

* delete current users
  DELETE FROM /nrk/apayuser.

* Upload new users to table
  INSERT /nrk/apayuser FROM TABLE t_user.
ELSE.
* No user from xls uploaded
ENDIF.
