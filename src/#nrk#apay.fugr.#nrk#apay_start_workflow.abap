FUNCTION /nrk/apay_start_workflow.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"     REFERENCE(DOC_CLASS) TYPE  TOADD-DOC_TYPE OPTIONAL
*"     REFERENCE(ARCHIV_ID) TYPE  SAEARCHIVI OPTIONAL
*"     REFERENCE(ARC_DOC_ID) TYPE  SAEARDOID OPTIONAL
*"     REFERENCE(USER) TYPE  XUBNAME
*"     REFERENCE(AR_OBJECT) TYPE  SAEOBJART OPTIONAL
*"  EXPORTING
*"     REFERENCE(WF_ID) TYPE  SWWWIHEAD-WI_ID
*"  EXCEPTIONS
*"      WORKFLOW_START_FAILED
*"----------------------------------------------------------------------

  INCLUDE <cntn01>.

  DATA: wa_hd          LIKE /nrk/apayhd,
        l_object_key   TYPE swotobjid-objkey,
        l_object_type  TYPE swotobjid-objtype,
        l_apay_object  TYPE swotobjid,
        l_image_object TYPE swotobjid,
        l_creator      LIKE swwwihead-wi_creator,
        l_task_id      LIKE /nrk/apaydtype-task_id,
        l_archiv_id    LIKE toa01-archiv_id,
        l_arc_doc_id   LIKE toa01-arc_doc_id,
        l_ar_object    LIKE toa01-ar_object,
        t_connect      LIKE toav0 OCCURS 0 WITH HEADER LINE,
        l_object_id      LIKE toav0-object_id.

  swc_container: l_wf_container.

  CLEAR: wa_hd.


  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno = apayno.

  IF sy-subrc NE 0.
    RAISE workflow_start_failed.
  ENDIF.

  IF ar_object IS INITIAL.
    SELECT SINGLE task_id FROM /nrk/apaydtype INTO l_task_id
      WHERE ar_object EQ wa_hd-ar_object.
  ELSE.
    SELECT SINGLE task_id FROM /nrk/apaydtype INTO l_task_id
    WHERE ar_object EQ ar_object.
  ENDIF.

  IF l_task_id IS INITIAL.
    RAISE workflow_start_failed.
  ENDIF.

* Create Workflow Container
  swc_create_container l_wf_container.
  swc_clear_container  l_wf_container.

* Create APay Center Object
  l_object_type = '/NRK/APAY'.
  MOVE apayno TO l_object_key.

  CALL FUNCTION 'SWC_OBJECT_CREATE'
    EXPORTING
      objtype                    = l_object_type
      objkey                     = l_object_key
*   LOGICAL_SYSTEM             = ' '
    IMPORTING
      object                     = l_apay_object
    EXCEPTIONS
      objtype_not_found          = 1
      logsys_not_found           = 2
      objtype_not_released       = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
    RAISE workflow_start_failed.
  ELSE.
    swc_set_element l_wf_container 'APayCenterObject' l_apay_object.
  ENDIF.

* Create Image Object
  IF NOT archiv_id IS INITIAL AND NOT arc_doc_id IS INITIAL.
* image exists
    l_object_type = 'IMAGE'.
    CONCATENATE archiv_id arc_doc_id INTO l_object_key.
  ELSE.
* image needs to be retrieved

    SELECT SINGLE ar_object FROM /nrk/apayhd INTO l_ar_object
      WHERE apayno EQ apayno.

    MOVE apayno TO l_object_id.
    CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
      EXPORTING
        objecttype    = '/NRK/APAY'
        object_id     = l_object_id
*       documenttype  = l_ar_object
      TABLES
        connections   = t_connect
      EXCEPTIONS
        nothing_found = 1
        OTHERS        = 2.

    IF sy-subrc = 0.
      READ TABLE t_connect INDEX 1.
      l_object_type = 'IMAGE'.
      CONCATENATE t_connect-archiv_id
                  t_connect-arc_doc_id INTO l_object_key.
    ENDIF.

  ENDIF.

  CALL FUNCTION 'SWC_OBJECT_CREATE'
    EXPORTING
      objtype              = l_object_type
      objkey               = l_object_key
    IMPORTING
      object               = l_image_object
    EXCEPTIONS
      objtype_not_found    = 1
      logsys_not_found     = 2
      objtype_not_released = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    RAISE workflow_start_failed.
  ELSE.
    swc_set_element  l_wf_container 'Image'          l_image_object.
  ENDIF.

* Other Container Elements
  IF NOT ar_object IS INITIAL.
    swc_set_element  l_wf_container 'DOCUMENTTYPE' ar_object.
  ELSE.
    swc_set_element  l_wf_container 'DOCUMENTTYPE' wa_hd-ar_object.
  ENDIF.

  IF NOT doc_class IS INITIAL.
    swc_set_element  l_wf_container 'DOCCLASS' doc_class.
  ENDIF.
  swc_set_element  l_wf_container 'USER' user.

* Start Workflow
  MOVE user TO l_creator.

  CALL FUNCTION 'SWW_WI_START'
    EXPORTING
      creator                            = l_creator
      task                               = l_task_id
      workitem_type                      = 'F'
      do_commit                          = 'X'
*   DO_SYNC_CALLBACK                   = ' '
*   TEXT                               = ' '
*   DO_SYNC_WI_CHAIN                   = ' '
*   CREATED_BY_USER                    = SY-UNAME
*   CREATED_BY_ADDRESS                 = ' '
*   CALLED_IN_BACKGROUND               = ' '
*   STEP_MODELED_WI_DISPLAY            = ' '
*   NO_DEADLINE_PARAMETERS             = ' '
*   RESTRICTED_LOG                     = ' '
*   SECONDS_UNTIL_TIMEOUT              =
*   CREATE_EVENT                       = ' '
*   STATUS_EVENT                       = ' '
*   XML_PROTOCOL                       =
*   WLC_FLAGS                          =
*   START_PROPERTIES                   =
*   DEBUG_FLAG                         = ' '
*   TRACE_FLAG                         = ' '
*   WI_CONTAINER_HANDLE                =
*   START_ASYNCHRONOUS                 = ' '
    IMPORTING
      wi_id                              = wf_id
*   WI_HEADER                          =
*   RETURN                             =
*   WI_RESULT                          =
*   SWF_RETURN                         =
*   NEW_STATUS                         =
    TABLES
*   AGENTS                             =
*   DEADLINE_AGENTS                    =
*   DESIRED_END_AGENTS                 =
*   LATEST_START_AGENTS                =
*   EXCLUDED_AGENTS                    =
*   NOTIFICATION_AGENTS                =
*   SECONDARY_METHODS                  =
      wi_container                       = l_wf_container
*   COMP_EVENTS                        =
    EXCEPTIONS
      id_not_created                     = 1
      read_failed                        = 2
      immediate_start_not_possible       = 3
      execution_failed                   = 4
      invalid_status                     = 5
      OTHERS                             = 6.

  IF sy-subrc <> 0.
    RAISE workflow_start_failed.
  ENDIF.

ENDFUNCTION.
