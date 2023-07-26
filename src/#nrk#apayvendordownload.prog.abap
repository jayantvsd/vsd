*&---------------------------------------------------------------------*
*& Report  /NRK/APAYVENDORDOWNLOAD
*&---------------------------------------------------------------------*
*& PROGRAM   APay Center
*& VERSION   2.x - 2017
*& AUTHOR    (c) Norikkon, LLC. 2017
*&---------------------------------------------------------------------*
*& Licensee acknowledges that the Software and its sequence, structure
*& and organization are proprietary to Norikkon LLC and that Norikkon LLC
*& retains exclusive ownership of the Products.
*&
*& Licensee agrees that Norikkon LLC owns all intellectual property
*& and proprietary rights, including but not limited to patents, copyrights,
*& trade secrets, trademarks, and any other proprietary rights, in and to
*& the Products and any corrections, bug fixes, enhancements, updates or other
*& modifications, including custom modifications, to Products, whether made
*& by Norikkon, Licensor or a third party. Licensee has the right to use the
*& Products solely as expressly permitted under the signed agreement.
*& Except as expressly allowed under the signed Agreement, Licensee agrees
*& not to reproduce, copy, modify, or translate the Software.
*&
*& Any reproductions, copies, modifications, or translations of the software
*& made at the request of Licensee during the implementation of the software
*& need to be documented and agreed to in writing by both parties. Licensee
*& further agrees not to use the Software in any manner for purposes of
*& designing or developing a competing software product across applications.
*& Licensee shall not permit any parent, subsidiary, affiliated entity or
*& third party to access the Products without the prior written authorization
*& of Licensor.
*&---------------------------------------------------------------------*
REPORT  /nrk/apayvendordownload
        LINE-SIZE 250
        MESSAGE-ID 00
        LINE-COUNT 58(2)
        NO STANDARD PAGE HEADING.

* Class
CLASS cl_abap_char_utilities DEFINITION LOAD.

* TABLES
TABLES: lfa1, lfb1.

* STRUCTURES

* output structure
DATA: itab(300)   TYPE c
      OCCURS 10000
      WITH HEADER LINE.

* vendor master
DATA: itab_lfa1   LIKE lfa1
      OCCURS 20000
      WITH HEADER LINE.

* vendor master (company code)
DATA: BEGIN OF wa_lfb1,
      lifnr       TYPE lifnr,
      bukrs       TYPE bukrs,
      END OF wa_lfb1.

DATA: itab_lfb1   LIKE wa_lfb1
      OCCURS 20000
      WITH HEADER LINE.

*----------------------------------------------------------------------*
*** TEMPORARY VARIABLES                      Description
*----------------------------------------------------------------------*
DATA: t_cnt             TYPE i,        " counter
      t_upd_cnt         TYPE i,        " update counter
      t_skp_cnt         TYPE i,        " skipped counter
      t_err_cnt         TYPE i,        " error counter
      t_str(50)         TYPE c,        " temp string work area
      t_slen            TYPE i,        " string length
      t_date            LIKE sy-datum, " Todays date ccyymmdd
      t_time            LIKE sy-uzeit, " Todays time
      t_date_mmddyy(8)  TYPE c,        " Todays date mmddyy
      t_date_mmyy(4)    TYPE c,        " Todays date mmyy
      t_date_mmyyyy(7)  TYPE c,        " Todays date mm/yyyy
      t_tab_char        TYPE c  VALUE cl_abap_char_utilities=>horizontal_tab.

CONSTANTS:
      c_false           TYPE c VALUE ' ',
      c_true            TYPE c VALUE 'X'.

CONSTANTS:
      c_tab_hex         TYPE x VALUE 9.

*----------------------------------------------------------------------*
*** SELECTION SCREEN OPTIONS
*----------------------------------------------------------------------*
*** Block B1 - Basic Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK b1
  WITH FRAME TITLE text-s01.

SELECT-OPTIONS:
 s_lifnr  FOR lfa1-lifnr,
 s_bukrs  FOR lfb1-bukrs,
 s_land1  FOR lfa1-land1,
 s_name1  FOR lfa1-name1,
 s_name2  FOR lfa1-name2,
 s_ort01  FOR lfa1-ort01,
 s_ort02  FOR lfa1-ort02,
 s_pstl2  FOR lfa1-pstl2,
 s_pstlz  FOR lfa1-pstlz,
 s_regio  FOR lfa1-regio,
 s_erdat  FOR lfa1-erdat,
 s_ernam  FOR lfa1-ernam,
 s_mcod1  FOR lfa1-mcod1,
 s_mcod2  FOR lfa1-mcod2.

PARAMETERS:
 p_nodel LIKE lfa1-nodel,
 p_bflag AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2
  WITH FRAME TITLE text-s02.

PARAMETERS:
     p_sep     TYPE c
               LOWER CASE
               DEFAULT '^'
               OBLIGATORY,

     p_down    AS CHECKBOX
               DEFAULT space,

     p_syst    AS CHECKBOX
               DEFAULT space,

     p_fname   LIKE rlgrap-filename
               LOWER CASE
               DEFAULT 'c:\temp\vendor.txt'
               OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3
  WITH FRAME TITLE text-s03.

PARAMETERS:
     p_prvw    AS  CHECKBOX            " run preview mode (don't do
                   DEFAULT 'X'.        " anything)

SELECTION-SCREEN END OF BLOCK b3.


*----------------------------------------------------------------------*
*** MAIN
*----------------------------------------------------------------------*

PERFORM startup.                       " Initialize variables
*                                      "   and edit parameters

PERFORM select_data.

PERFORM create_delimited_table.

IF p_down EQ c_true.

  PERFORM download_to_desktop.

ELSE.

  PERFORM download_to_server.

ENDIF.

PERFORM display_report.

PERFORM report_results.

* Done!

TOP-OF-PAGE.

  PERFORM top_heading
           USING text-s04
                 text-s05
                 ' '.

*-----------------------------------------------------------------------
*** INITIALIZATION - Init default values in parameters
*-----------------------------------------------------------------------
INITIALIZATION.

*none

*-----------------------------------------------------------------------
*** AT SELECTION SCREEN
*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask          = '*.txt'
      static        = 'X'
    CHANGING
      file_name     = p_fname
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.


*-----------------------------------------------------------------------
*  Subroutine:   startup
*  Description:  Initialize variables and edit input parameters.
*  Passed Parms: None
*-----------------------------------------------------------------------
FORM startup.

  DATA: litab_lines    LIKE tline
        OCCURS 50
        WITH HEADER LINE.

  t_cnt          = 0.                  " initialize temporary variables
  t_err_cnt      = 0.

  CLEAR itab.
  REFRESH itab.

  WRITE sy-datum MMDDYY TO t_date_mmddyy.
  CONCATENATE sy-datum+4(2)
              sy-datum+2(2)
              INTO t_date_mmyy.
  CONCATENATE sy-datum+4(2)
              '/'
              sy-datum(4)
              INTO t_date_mmyyyy.
  t_date         = sy-datum.
  t_time         = sy-uzeit.

* set luw
  COMMIT WORK.

ENDFORM.                    "startup

*-----------------------------------------------------------------------
*  Subroutine:   select_data
*  Description:  Get matching entries.
*  Passed Parms: none
*
*-----------------------------------------------------------------------
FORM select_data.

  SELECT * FROM lfa1 INTO TABLE itab_lfa1
    WHERE lifnr IN s_lifnr
      AND land1 IN s_land1
      AND name1 IN s_name1
      AND name2 IN s_name2
      AND ort01 IN s_ort01
      AND ort02 IN s_ort02
      AND pstl2 IN s_pstl2
      AND pstlz IN s_pstlz
      AND regio IN s_regio
      AND erdat IN s_erdat
      AND ernam IN s_ernam
      AND mcod1 IN s_mcod1
      AND mcod2 IN s_mcod2
      AND nodel EQ p_nodel.


  SORT itab_lfa1 BY name1 name2.

* only include lfb1 in search
* if company code is entered

  IF s_bukrs[] IS NOT INITIAL.

    SELECT lifnr bukrs FROM lfb1
      INTO CORRESPONDING FIELDS OF TABLE itab_lfb1
      WHERE lifnr IN s_lifnr
        AND bukrs IN s_bukrs.

    SORT itab_lfb1.

*   IF p_bflag NE 'X'.

    DELETE ADJACENT DUPLICATES
      FROM itab_lfb1
      COMPARING lifnr.


    LOOP AT itab_lfa1.

      READ TABLE itab_lfb1
        WITH KEY lifnr = itab_lfa1-lifnr
        BINARY SEARCH.

      IF sy-subrc NE 0.

        DELETE itab_lfa1.

      ENDIF.

    ENDLOOP.

*   ELSE. " incl. company code

*   ENDIF.

  ENDIF.

ENDFORM.                    "select_data

*-----------------------------------------------------------------------
*  Subroutine:   create_delimited_table
*  Description:  Create internal delimited table.
*
*  Passed Parms: none
*
*-----------------------------------------------------------------------
FORM create_delimited_table.

* Create heading

  IF itab_lfa1[] IS NOT INITIAL.

    IF p_syst NE 'X'.

      CONCATENATE 'Vendor'       p_sep
                  'Name'         p_sep
                  'Street'       p_sep
                  'PO Box'       p_sep
                  'City'         p_sep
                  'State'        p_sep
                  'Zip Code'     p_sep
                  'Country'      p_sep
                  'Company Code' p_sep
                  'Search term 1' p_sep
                  'Search term 2'
             INTO itab.

    ELSE.

      CONCATENATE 'Vendor'       p_sep
                  'Name'         p_sep
                  'Street'       p_sep
                  'PO Box'       p_sep
                  'City'         p_sep
                  'State'        p_sep
                  'Zip Code'     p_sep
                  'Country'      p_sep
                  'Company Code' p_sep
                  'Search term 1' p_sep
                  'Search term 2' p_sep
                  'Client'        p_sep
                  'System'
             INTO itab.

    ENDIF.

    APPEND itab.

  ENDIF.

* create delmited table
  LOOP AT itab_lfa1.

    SELECT bukrs FROM lfb1 INTO itab_lfb1-bukrs
      WHERE lifnr EQ itab_lfa1-lifnr.

      IF p_syst NE 'X'.

        CONCATENATE itab_lfa1-lifnr p_sep
                    itab_lfa1-name1 p_sep
                    itab_lfa1-stras p_sep
                    itab_lfa1-pfach p_sep
                    itab_lfa1-ort01 p_sep
                    itab_lfa1-regio p_sep
                    itab_lfa1-pstlz p_sep
                    itab_lfa1-land1 p_sep
                    itab_lfb1-bukrs p_sep
                    itab_lfa1-mcod1 p_sep
                    itab_lfa1-mcod2
               INTO itab.

      ELSE.

        CONCATENATE itab_lfa1-lifnr p_sep
                    itab_lfa1-name1 p_sep
                    itab_lfa1-stras p_sep
                    itab_lfa1-pfach p_sep
                    itab_lfa1-ort01 p_sep
                    itab_lfa1-regio p_sep
                    itab_lfa1-pstlz p_sep
                    itab_lfa1-land1 p_sep
                    itab_lfb1-bukrs p_sep
                    itab_lfa1-mcod1 p_sep
                    itab_lfa1-mcod2 p_sep
                    syst-mandt      p_sep
                    syst-sysid
               INTO itab.

      ENDIF.

      APPEND itab.

    ENDSELECT.

  ENDLOOP.


ENDFORM.                    "create_delimited_table

*-----------------------------------------------------------------------
*  Subroutine:   download_to_desktop
*  Description:  Processed selected items into a character delimited
*                internal table then download to desktop..
*
*  Passed Parms: none
*
*-----------------------------------------------------------------------
FORM download_to_desktop.

  DATA: l_fname    TYPE string.

* check for preview

  IF p_prvw EQ c_true.

    EXIT.

  ENDIF.

* check for data

  IF itab[] IS INITIAL.

    EXIT.

  ENDIF.

  l_fname = p_fname.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = l_fname
    TABLES
      data_tab                = itab
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc NE 0.

    MESSAGE e398
       WITH text-s06.

  ENDIF.


ENDFORM.                    "download_to_desktop

*-----------------------------------------------------------------------
*  Subroutine:   download_to_server
*  Description:  Processed selected items into a tab delimited
*                internal table if download selected.
*                Then download to server specified file
*                and create trigger.
*
*  Passed Parms: none
*
*-----------------------------------------------------------------------
FORM download_to_server.

* check for preview

  IF p_prvw EQ c_true.

    EXIT.

  ENDIF.

* check for data

  IF itab_lfa1[] IS INITIAL.

    EXIT.

  ENDIF.

* output table to server file

  OPEN DATASET p_fname
   FOR OUTPUT IN LEGACY TEXT MODE
   IGNORING CONVERSION ERRORS.

  IF sy-subrc NE 0.
    MESSAGE e398
       WITH text-s07 p_fname.
  ENDIF.

  LOOP AT itab.

    TRANSFER itab TO p_fname.

    IF sy-subrc NE 0.
      MESSAGE e398
         WITH text-s08 p_fname.
    ENDIF.

  ENDLOOP.

  CLOSE DATASET p_fname.

ENDFORM.                    "download_to_server


*-----------------------------------------------------------------------
*  Subroutine:   display_report.
*  Description:  Display information about invoice.
*  Passed Parms: none
*
*-----------------------------------------------------------------------
FORM display_report.

  LOOP AT itab_lfa1.

    SELECT SINGLE bukrs FROM lfb1 INTO itab_lfb1-bukrs
      WHERE lifnr EQ itab_lfa1-lifnr.

    WRITE: /  itab_lfa1-lifnr      UNDER text-s09,
              itab_lfa1-name1      UNDER text-s10,
              itab_lfa1-stras      UNDER text-s11,
              itab_lfa1-pfach      UNDER text-s12,
              itab_lfa1-ort01      UNDER text-s13,
              itab_lfa1-regio      UNDER text-s14,
              itab_lfa1-pstlz      UNDER text-s15,
              itab_lfa1-land1      UNDER text-s16,
              itab_lfb1-bukrs      UNDER text-s22,
              itab_lfa1-mcod1      UNDER text-s20,
              itab_lfa1-mcod2      UNDER text-s21,
              syst-mandt           UNDER text-s24,
              syst-sysid           UNDER text-s23.

  ENDLOOP.

ENDFORM.                    "display_report

*-----------------------------------------------------------------------
*  Subroutine:   top_heading
*  Description:  Display report heading.
*  Passed Parms: Heading line 1, heading line 2 and heading line 3.
*                (any line may be spaces)
*-----------------------------------------------------------------------
FORM top_heading USING h1 h2 h3.

  DATA: h1start    TYPE i,             " heading line 1 start
        h2start    TYPE i,             " heading line 2 start
        h3start    TYPE i,             " heading line 3 start
        pgstart    TYPE i,             " Page title start
        pgnumstart TYPE i,             " Page num title start
        pgmstart   TYPE i,             " Program name start
        pgmlen     TYPE i.             " Program name length

*  compute the start point
*  of headings 1, 2 & 3
  h1start = ( sy-linsz - STRLEN( h1 ) ) / 2.
  h2start = ( sy-linsz - STRLEN( h2 ) ) / 2.
  h3start = ( sy-linsz - STRLEN( h3 ) ) / 2.

  pgstart = sy-linsz - 13.
  pgnumstart = pgstart + 6.
  pgmlen = STRLEN( sy-repid ).
  pgmstart = sy-linsz - pgmlen - 14.

  WRITE: 'Date:', sy-datum,
         AT h1start h1,
         AT pgstart 'Page: ',
         AT pgnumstart sy-pagno LEFT-JUSTIFIED.

  WRITE:/'Time:', sy-uzeit,
         AT h2start h2,
         AT pgmstart sy-repid(pgmlen),
         '/', sy-mandt.

  WRITE:/' ',
         AT h3start h3.
  ULINE.

  WRITE: AT /1  text-s09,
         AT 15  text-s10,
         AT 55  text-s11,
         AT 89  text-s12,
         AT 98  text-s13,
         AT 131 text-s14,
         AT 144 text-s15,
         AT 156 text-s16,
         AT 164 text-s22,
         AT 189 text-s20,
         AT 204 text-s21,
         AT 218 text-s24,
         AT 225 text-s23.

  ULINE.

ENDFORM.                    "top_heading

*-----------------------------------------------------------------------
*  Subroutine:   report_results
*  Description:  Display processing results.
*  Passed Parms: None.
*-----------------------------------------------------------------------

FORM report_results.

  ULINE.
  ULINE.

  DESCRIBE TABLE itab_lfa1 LINES t_cnt.
  WRITE: /.
  WRITE: AT /2 text-s17.
  WRITE: AT /2 text-s18, t_cnt.

  IF p_prvw EQ c_true.
    WRITE: / .
    WRITE: / text-s19.
  ENDIF.

  ULINE.

ENDFORM.                    "report_results
