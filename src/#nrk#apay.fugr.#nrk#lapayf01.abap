*----------------------------------------------------------------------*
***INCLUDE /NRK/LAPAYF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_LST_TABLE_CMNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_10     text
*      -->P_TEXT_956  text
*      -->P_0063   text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_lst_table_cmnt USING p_len
                                p_stxt
                                p_fnam
                                p_hotspot.

  DATA: ls_alv_fieldcat_cmnt TYPE slis_fieldcat_alv.

  DESCRIBE TABLE gt_alv_fieldcat_cmnt LINES sy-tfill.

  IF sy-tfill = 0.
    gv_field_pos = 1.
  ELSE.
    gv_field_pos = gv_field_pos + 1.
  ENDIF.

  ls_alv_fieldcat_cmnt-outputlen = p_len.
  ls_alv_fieldcat_cmnt-col_pos   = gv_field_pos.
  ls_alv_fieldcat_cmnt-seltext_m = p_stxt.
  ls_alv_fieldcat_cmnt-hotspot   = p_hotspot.
  ls_alv_fieldcat_cmnt-fieldname = p_fnam.

  APPEND ls_alv_fieldcat_cmnt TO gt_alv_fieldcat_cmnt.

ENDFORM.                    " BUILD_LST_TABLE_CMNT
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_I_BDCDATA  text
*      -->P_0061   text
*      -->P_0062   text
*----------------------------------------------------------------------*
FORM bdc_dynpro  TABLES   p_p_i_bdcdata STRUCTURE bdcdata
                 USING    program
                          dynpro.

  CLEAR p_p_i_bdcdata.
  p_p_i_bdcdata-program  = program.
  p_p_i_bdcdata-dynpro   = dynpro.
  p_p_i_bdcdata-dynbegin = 'X'.
  APPEND p_p_i_bdcdata.

ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_I_BDCDATA  text
*      -->P_0068   text
*      -->P_0069   text
*----------------------------------------------------------------------*
FORM bdc_field  TABLES   p_p_i_bdcdata STRUCTURE bdcdata
                USING    pfnam
                         pfval.

  IF pfval NE space.
    CLEAR p_p_i_bdcdata.
    p_p_i_bdcdata-fnam = pfnam.
    p_p_i_bdcdata-fval = pfval.

    APPEND p_p_i_bdcdata.
  ENDIF.

ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  SET_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_T01_WAERS  text
*      -->P_I_T01_WRBTR  text
*      -->P_WRBTR  text
*----------------------------------------------------------------------*
FORM set_amount  USING    p_i_waers
                          p_i_wrbtr
                          p_wrbtr.

  CLEAR p_wrbtr.
  WRITE p_i_wrbtr TO p_wrbtr CURRENCY p_i_waers.

ENDFORM.                    " SET_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  SET_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_HD_XBLDT  text
*      -->P_LV_XBLDT  text
*----------------------------------------------------------------------*
FORM set_date  USING    p_i_bldat
                        p_bldat.

  CLEAR p_bldat.
  CALL FUNCTION 'DATUMSAUFBEREITUNG'
    EXPORTING
      idate           = p_i_bldat
    IMPORTING
      tdat6           = p_bldat
    EXCEPTIONS
      datfm_ungueltig = 1
      datum_ungueltig = 2
      OTHERS          = 3.

ENDFORM.                    " SET_DATE
*&---------------------------------------------------------------------*
*&      Form  APPEND_APPROVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_APPROVER  text
*      -->P_WA_USER  text
*      -->P_ITEM  text
*----------------------------------------------------------------------*
FORM append_approver  TABLES   p_t_approver STRUCTURE /nrk/apayappr
                      USING    p_wa_user LIKE /nrk/apayuser
                               p_item
                               p_apayno.

  DATA: wa_approver TYPE /nrk/apayappr.

* Set data
  wa_approver-apayno    = p_apayno.
  wa_approver-item      = p_item.
  wa_approver-sap_user  = p_wa_user-objid.
  wa_approver-ext_user  = p_wa_user-extuser.
  wa_approver-smtp_addr = p_wa_user-smtp_addr.

  TRANSLATE wa_approver-ext_user TO UPPER CASE.
* Append to approver table
  APPEND wa_approver TO p_t_approver.

ENDFORM.                    " APPEND_APPROVER
*&---------------------------------------------------------------------*
*&      Form  GET_DEFAULT_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_APPROVER  text
*      -->P_WA_USER  text
*      -->P_ITEM  text
*      -->P_APAYNO  text
*----------------------------------------------------------------------*
FORM get_default_user  TABLES   p_t_approver STRUCTURE /nrk/apayappr
                       USING    p_wa_user LIKE /nrk/apayuser
                                p_item
                                p_apayno.

* Get default administrator
  SELECT SINGLE * FROM /nrk/apayuser INTO p_wa_user WHERE admin EQ 'X'.
  IF sy-subrc EQ 0.
* Write to approver table
    PERFORM append_approver TABLES p_t_approver USING p_wa_user p_item p_apayno.
  ELSE.
    RAISE user_not_found.
  ENDIF.

ENDFORM.                    " GET_DEFAULT_USER
*&---------------------------------------------------------------------*
*&      Form  GET_WF_STEPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wf_steps .

  DATA: BEGIN OF i_swwwihead OCCURS 0.
          INCLUDE STRUCTURE swwwihead.
  DATA: END OF i_swwwihead.

  DATA: wa_swwwihead LIKE swwwihead.

  SELECT * FROM swwwihead
          INTO   CORRESPONDING FIELDS OF TABLE i_swwwihead
          WHERE  wi_chckwi  = wi_id
          AND wi_stat NE'COMPLETED'
          AND wi_stat NE 'CANCELLED'.

  LOOP AT i_swwwihead.
    CASE i_swwwihead-wi_type.
      WHEN 'F'.
        wi_id = i_swwwihead-wi_id.
        PERFORM get_wf_steps.
      WHEN 'W' OR 'A'.
        CASE i_swwwihead-wi_stat.
          WHEN 'READY' OR 'SELECTED' OR 'STARTED'.
            MOVE-CORRESPONDING i_swwwihead TO wa_swwwihead.
            APPEND wa_swwwihead TO g_swwwihead.
          WHEN OTHERS.
        ENDCASE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " GET_WF_STEPS
