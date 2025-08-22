*&---------------------------------------------------------------------*
*& Report ZALV_INTERACTIVE_PRAC1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zalv_interactive_prac1.

TABLES: rbkp, rseg.

TYPE-POOLS: slis.

*-- Type Definitions
TYPES: BEGIN OF ts_rbkp,
         belnr TYPE rbkp-belnr,
         gjahr TYPE rbkp-gjahr,
         blart TYPE rbkp-blart,
         bldat TYPE rbkp-bldat,
         budat TYPE rbkp-budat,
         usnam TYPE rbkp-usnam,
         bukrs TYPE rbkp-bukrs,
       END OF ts_rbkp.

TYPES: BEGIN OF ts_rseg,
         buzei TYPE rseg-buzei,
         ebeln TYPE rseg-ebeln,
         ebelp TYPE rseg-ebelp,
         zekkn TYPE rseg-zekkn,
         matnr TYPE rseg-matnr,
       END OF ts_rseg.

*-- Data Declarations
DATA: it_rbkp TYPE STANDARD TABLE OF ts_rbkp,
      wa_rbkp TYPE ts_rbkp,
      it_rseg TYPE STANDARD TABLE OF ts_rseg,
      wa_rseg TYPE ts_rseg.

DATA: v_repid TYPE sy-repid,
      alv_layout TYPE slis_layout_alv.

DATA: it_fieldcat   TYPE slis_t_fieldcat_alv,
      wa_fieldcat   TYPE slis_fieldcat_alv,
      it_event      TYPE slis_t_event,
      wa_event      TYPE slis_alv_event.

*-- Initialization
INITIALIZATION.
  v_repid = sy-repid.
  PERFORM build_fieldcatlog.
  PERFORM populate_event.

*-- Main Logic
START-OF-SELECTION.
  PERFORM data_retrieval.
  PERFORM display_alv_report.

*----------------------------------------------------------------------*
* FORM build_fieldcatlog
*----------------------------------------------------------------------*
FORM build_fieldcatlog.
  CLEAR it_fieldcat.

  DEFINE append_fieldcat.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-seltext_m = &2.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR wa_fieldcat.
  END-OF-DEFINITION.

  append_fieldcat: 'BELNR' 'Document No',
                   'GJAHR' 'Year',
                   'BUDAT' 'Posting Date',
                   'USNAM' 'User Name'.

ENDFORM.

*----------------------------------------------------------------------*
* FORM populate_event
*----------------------------------------------------------------------*
FORM populate_event.
  CLEAR wa_event.
  wa_event-name = 'USER_COMMAND'.
  wa_event-form = 'EVENT_CALL'.
  APPEND wa_event TO it_event.
ENDFORM.

*----------------------------------------------------------------------*
* FORM data_retrieval
*----------------------------------------------------------------------*
FORM data_retrieval.
  SELECT belnr gjahr blart bldat budat usnam bukrs
    INTO TABLE it_rbkp
    FROM rbkp
    UP TO 50 ROWS.
ENDFORM.

*----------------------------------------------------------------------*
* FORM display_alv_report
*----------------------------------------------------------------------*
FORM display_alv_report.
  alv_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = v_repid
      is_layout          = alv_layout
      it_fieldcat        = it_fieldcat
      it_events          = it_event
      i_save             = 'A'
    TABLES
      t_outtab           = it_rbkp.
ENDFORM.

*----------------------------------------------------------------------*
* FORM event_call
*----------------------------------------------------------------------*
FORM event_call USING r_ucomm TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  READ TABLE it_rbkp INDEX rs_selfield-tabindex INTO wa_rbkp.
  IF sy-subrc = 0.
    PERFORM fetch_line_items USING wa_rbkp-belnr wa_rbkp-gjahr.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
* FORM fetch_line_items
*----------------------------------------------------------------------*
FORM fetch_line_items USING p_belnr TYPE rbkp-belnr
                             p_gjahr TYPE rbkp-gjahr.

  CLEAR it_rseg.

  SELECT buzei ebeln ebelp zekkn matnr
    INTO TABLE it_rseg
    FROM rseg
    WHERE belnr = p_belnr AND gjahr = p_gjahr.

  IF sy-subrc = 0.
    PERFORM show_line_items.
  ELSE.
    MESSAGE 'No line items found for this document.' TYPE 'I'.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
* FORM show_line_items
*----------------------------------------------------------------------*
FORM show_line_items.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv.

  DEFINE add_field.
    ls_fieldcat-fieldname = &1.
    ls_fieldcat-seltext_m = &2.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.
  END-OF-DEFINITION.

  add_field: 'BUZEI' 'Line Item',
             'EBELN' 'PO Number',
             'EBELP' 'PO Item',
             'ZEKKN' 'Seq. No',
             'MATNR' 'Material'.

  alv_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = v_repid
      is_layout          = alv_layout
      it_fieldcat        = lt_fieldcat
      i_save             = 'A'
    TABLES
      t_outtab           = it_rseg.

ENDFORM.
