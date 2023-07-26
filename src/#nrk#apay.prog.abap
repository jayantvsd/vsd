PROGRAM /nrk/apay.
*&---------------------------------------------------------------------*
*& Program   /NRK/APAY
*&---------------------------------------------------------------------*
*& PROGRAM   APayCenter
*& VERSION   1.0 - 02/24/2006
*&           1.5 - 08/09/2010
*&           2.0 - 10/09/2013
*&           2.1 - 06/13/2014
*&           3.0 - 01/01/2016
*&           3.1 - 12/15/2017
*&           3.2 - 10/22/2018
*&           4.0 - 01/15/2022
*& AUTHOR    (c) Norikkon, LLC. 2022
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
CLASS cl_gui_cfw         DEFINITION LOAD.
CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_alv_tree_base   DEFINITION LOAD.

INCLUDE <icon>.              " Include for icons
INCLUDE <cntn01>.            " Include for container macros
INCLUDE /nrk/apaytop.        " Global data
INCLUDE /nrk/apayclasses.    " Classes
INCLUDE /nrk/apayselect.     " Selection
INCLUDE /nrk/apayforms.      " Forms
INCLUDE /nrk/apaypbo.        " PBO
INCLUDE /nrk/apaypai.        " PAI

TABLES sscrfields.
SELECTION-SCREEN: FUNCTION KEY 1.

INITIALIZATION.

  sscrfields-functxt_01 = 'New'.

AT SELECTION-SCREEN.

* Clear selection fields
  CLEAR: s_apayno,
         s_bukrs,
         s_xblnr,
         s_bldat,
         s_wrbtr,
         s_waers,
         s_lifnr,
         s_cdate,
         s_cstat,
         s_hstat,
         s_belnr,
         s_gjahr,
         s_extu,
         s_arobj.

*  CLEAR: s_apayno[],
*         s_bukrs[],
*         s_xblnr[],
*         s_bldat[],
*         s_wrbtr[],
*         s_waers[],
*         s_lifnr[],
*         s_cdate[],
*         s_cstat[],
*         s_hstat[],
*         s_belnr[],
*         s_gjahr[],
*         s_extu[].

  CASE sscrfields-ucomm.
      WHEN'FC01'.

      CALL FUNCTION '/NRK/APAYUPLOADNEWDOCUMENT'
* IMPORTING
*   APAYNO                       =
*   CANCEL                       =
        EXCEPTIONS
          document_upload_failed       = 1
          OTHERS                       = 2.

      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


  ENDCASE.

START-OF-SELECTION.

  PERFORM set_screen_view.
  PERFORM get_record_data.

  CALL SCREEN 100.

*&------------------------------------------------------------------------------*
*& At selection-screen on value-request
*&------------------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cstat-low.
  PERFORM get_help_status USING s_cstat-low 'S_CSTAT'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cstat-high.
  PERFORM get_help_status USING s_cstat-high 'S_CSTAT'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_hstat-low.
  PERFORM get_help_status USING s_hstat-low 'S_HSTAT'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_hstat-high.
  PERFORM get_help_status USING s_hstat-high 'S_HSTAT'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_arobj-low.
  PERFORM get_help_ar_object USING s_arobj-low 'S_AROBJ'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_arobj-high.
  PERFORM get_help_ar_object USING s_arobj-high 'S_AROBJ'.
