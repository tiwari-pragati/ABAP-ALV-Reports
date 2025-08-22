*&---------------------------------------------------------------------*
*& Report ZPO_REPORT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

REPORT zpo_report.

TYPE-POOLS: slis.

TABLES: ekko, ekpo.

* Types Declaration
TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ekpo-ebeln,
         werks TYPE ekpo-werks,
         ebelp TYPE ekpo-ebelp,
         matnr TYPE ekpo-matnr,
         menge TYPE ekpo-menge,
         meins TYPE ekpo-meins,
         netpr TYPE ekpo-netpr,
       END OF ty_ekpo.

TYPES: BEGIN OF ty_details,
         ebeln TYPE ekpo-ebeln,
         ebelp TYPE ekpo-ebelp,
         matnr TYPE ekpo-matnr,
         menge TYPE ekpo-menge,
         meins TYPE ekpo-meins,
         netpr TYPE ekpo-netpr,
       END OF ty_details.

* Data Declaration
DATA: it_ekpo      TYPE TABLE OF ty_ekpo,
      wa_ekpo      TYPE ty_ekpo,
      it_fieldcat  TYPE slis_t_fieldcat_alv,
      wa_fieldcat  TYPE slis_fieldcat_alv,
      v_repid      TYPE sy-repid,
      v_layout     TYPE slis_layout_alv,
      it_details   TYPE TABLE OF ty_details,
      wa_details   TYPE ty_details.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_werks FOR ekpo-werks NO INTERVALS,
                s_ebeln FOR ekpo-ebeln,
                s_date  FOR ekko-bedat.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  v_repid = sy-repid.

START-OF-SELECTION.

* PO Data
  SELECT ekko~ebeln,
         ekpo~werks,
         ekpo~ebelp,
         ekpo~matnr,
         ekpo~menge,
         ekpo~meins,
         ekpo~netpr
    INTO TABLE @it_ekpo
    FROM ekko
    INNER JOIN ekpo ON ekko~ebeln = ekpo~ebeln
    WHERE ekko~ebeln IN @s_ebeln
      AND ekpo~werks IN @s_werks
      AND ekko~aedat IN @s_date.

  IF it_ekpo IS INITIAL.
    MESSAGE 'No Matching Purchase Orders found' TYPE 'I'.
    EXIT.
  ENDIF.

  " Build ALV field catalog
  PERFORM build_fieldcatlog.



  " Display
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = v_repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      IS_LAYOUT                         =   v_layout
      IT_FIELDCAT                        =  it_fieldcat
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     IT_SORT                           =
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
*     I_SAVE                            = ' '
*     IS_VARIANT                        =
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*     O_PREVIOUS_SRAL_HANDLER           =
*     O_COMMON_HUB                      =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_ekpo
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*  FORM build_fieldcatlog

FORM build_fieldcatlog.

  wa_fieldcat-tabname = 'IT_EKPO'.
  wa_fieldcat-fieldname = 'EBELN'.
  wa_fieldcat-seltext_m = 'PO NUMBER'.
  wa_fieldcat-hotspot = 'X'.  " Enable double-click
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'IT_EKPO'.
  wa_fieldcat-fieldname = 'WERKS'.
  wa_fieldcat-seltext_m = 'PLANT'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'EBELP'.
  wa_fieldcat-seltext_m = 'ITEM NO'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-seltext_m = 'MATERIAL'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'MENGE'.
  wa_fieldcat-seltext_m = 'QUANTITY'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'MEINS'.
  wa_fieldcat-seltext_m = 'UOM'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'NETPR'.
  wa_fieldcat-seltext_m = 'PRICE'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.

*       FORM user_command

FORM user_command USING r_ucomm LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.

  DATA: lv_ebeln TYPE ekpo-ebeln.

  IF rs_selfield-fieldname = 'EBELN'.

    READ TABLE it_ekpo INTO wa_ekpo INDEX rs_selfield-tabindex.
    IF sy-subrc = 0.
      lv_ebeln = wa_ekpo-ebeln.

      CLEAR it_details.

      SELECT ebeln,
             ebelp,
             matnr,
             menge,
             meins,
             netpr
        INTO TABLE @it_details
        FROM ekpo
        WHERE ebeln = @lv_ebeln.

      IF sy-subrc = 0.
        PERFORM display_detail.
      ELSE.
        MESSAGE 'No item details found' TYPE 'I'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM display_detail.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv.

  CLEAR lt_fieldcat.

  ls_fieldcat-fieldname = 'EBELN'.
  ls_fieldcat-seltext_m = 'PO NO'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'EBELP'.
  ls_fieldcat-seltext_m = 'ITEM NO'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'MATNR'.
  ls_fieldcat-seltext_m = 'MATERIAL'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'MENGE'.
  ls_fieldcat-seltext_m = 'QUANTITY'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'MEINS'.
  ls_fieldcat-seltext_m = 'UOM'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'NETPR'.
  ls_fieldcat-seltext_m = 'PRICE'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = sy-repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
*     IS_LAYOUT                         =
      IT_FIELDCAT                       =  lt_fieldcat
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     IT_SORT                           =
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
*     I_SAVE                            = ' '
*     IS_VARIANT                        =
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*     O_PREVIOUS_SRAL_HANDLER           =
*     O_COMMON_HUB                      =
*  IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          =  it_details
    EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
