*&---------------------------------------------------------------------*
*& Report ZALV_INTERACTIVE_PRAC2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zalv_interactive_prac2.

TABLES: mseg, makt, mara, t023t.


TYPES: BEGIN OF ty_tab,
         ebeln TYPE mseg-ebeln,
         matnr TYPE mseg-matnr,
         meins TYPE mseg-meins,
         menge TYPE mseg-menge,
         bwart TYPE mseg-bwart,
         maktx TYPE makt-maktx,
         matkl TYPE mara-matkl,
         wgbez TYPE t023t-wgbez,
       END OF ty_tab.

DATA : it_tab   TYPE STANDARD TABLE OF ty_tab,
       wa_tab   TYPE          ty_tab,
       it_mseg  TYPE STANDARD TABLE OF mseg,
       wa_mseg  TYPE          mseg,
       it_makt  TYPE STANDARD TABLE OF makt,
       wa_makt  TYPE          makt,
       it_mara  TYPE STANDARD TABLE OF mara,
       wa_mara  TYPE          mara,
       it_t023t TYPE STANDARD TABLE OF t023t,
       wa_t023t TYPE          t023t.

DATA : ty_tab_fcat    TYPE  lvc_t_fcat,
       it_tab_fcat    LIKE LINE OF ty_tab_fcat,
       it_tab_layout  TYPE lvc_s_layo,
       it_tab_variant TYPE disvariant,
       it_tab_grid    TYPE lvc_s_glay,
       it_tab_cnt     TYPE  i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
  s_mblnr FOR mseg-mblnr.

SELECTION-SCREEN  END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM build_data.
  PERFORM field_data.
  PERFORM display_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  SELECT * FROM mseg INTO TABLE it_mseg
    WHERE mblnr  IN s_mblnr.

  IF it_mseg IS NOT INITIAL.

    SELECT * FROM makt INTO TABLE it_makt
      FOR ALL ENTRIES IN it_mseg
      WHERE matnr = it_mseg-matnr.

    IF it_makt IS NOT INITIAL.

      SELECT * FROM  mara INTO TABLE @it_mara
        FOR ALL ENTRIES IN @it_mseg
        WHERE matnr = @it_mseg-matnr.

      IF it_mara IS NOT INITIAL.

        SELECT * FROM t023t INTO TABLE @it_t023t
          FOR ALL ENTRIES IN @it_mara
          WHERE matkl = @it_mara-matkl
          AND spras   = 'ZH'.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form build_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_data .

  LOOP AT it_mseg INTO wa_mseg.
     wa_tab-ebeln = wa_mseg-ebeln.
     wa_tab-matnr = wa_mseg-matnr.
     wa_tab-meins = wa_mseg-meins.
     wa_tab-menge = wa_mseg-menge.
     wa_tab-bwart = wa_mseg-bwart.

     READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_mseg-matnr.
     wa_tab-maktx = wa_makt-maktx.

     READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mseg-matnr.
     wa_tab-matkl = wa_mara-matkl.

     READ TABLE it_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl.
     wa_tab-wgbez = wa_t023t-wgbez.

     APPEND wa_tab TO it_tab.
     CLEAR: wa_tab,wa_mseg,wa_makt,wa_mara,wa_t023t.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form display_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
 EXPORTING
*   I_INTERFACE_CHECK                   = ' '
*   I_BYPASSING_BUFFER                  =
*   I_BUFFER_ACTIVE                     =
    I_CALLBACK_PROGRAM                  = SY-REPID
*   I_CALLBACK_PF_STATUS_SET            = ' '
*   I_CALLBACK_USER_COMMAND             = ' '
*   I_CALLBACK_TOP_OF_PAGE              = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE         = ' '
*   I_CALLBACK_HTML_END_OF_LIST         = ' '
*   I_CALLBACK_DATA_INFO_SETTINGS       = ' '
*   I_STRUCTURE_NAME                    =
*   I_BACKGROUND_ID                     = ' '
*   I_GRID_TITLE                        =
*   I_GRID_SETTINGS                     =
*   IS_LAYOUT_LVC                       =
    IT_FIELDCAT_LVC                     = ty_tab_fcat[]
*   IT_EXCLUDING                        =
*   IT_SPECIAL_GROUPS_LVC               =
*   IT_SORT_LVC                         =
*   IT_FILTER_LVC                       =
*   IT_HYPERLINK                        =
*   IS_SEL_HIDE                         =
    I_DEFAULT                           = 'X'
*   I_SAVE                              = ' '
*   IS_VARIANT                          =
*   IT_EVENTS                           =
*   IT_EVENT_EXIT                       =
*   IS_PRINT_LVC                        =
*   IS_REPREP_ID_LVC                    =
*   I_SCREEN_START_COLUMN               = 0
*   I_SCREEN_START_LINE                 = 0
*   I_SCREEN_END_COLUMN                 = 0
*   I_SCREEN_END_LINE                   = 0
*   I_HTML_HEIGHT_TOP                   =
*   I_HTML_HEIGHT_END                   =
*   IT_ALV_GRAPHICS                     =
*   IT_EXCEPT_QINFO_LVC                 =
*   O_COMMON_HUB                        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER             =
*   ES_EXIT_CAUSED_BY_USER              =
  TABLES
    t_outtab                            = it_tab
 EXCEPTIONS
   PROGRAM_ERROR                        = 1
   OTHERS                               = 2
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form field_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM field_data .

*it_tab_layout-cwidth_opt = 'X'.
it_tab_layout-box_fname = 'SEL'.
it_tab_variant-report   = sy-repid.

it_tab_cnt = it_tab_cnt + 1.
it_tab_fcat-col_pos = it_tab_cnt.
it_tab_fcat-fieldname = 'EBELN'.
it_tab_fcat-coltext   = 'Purchase Order Number'.
*it_tab-emphasize      = 'C3'.
APPEND it_tab_fcat TO ty_tab_fcat.
CLEAR it_tab_fcat.

it_tab_cnt = it_tab_cnt + 1.
it_tab_fcat-col_pos = it_tab_cnt.
it_tab_fcat-fieldname = 'BWART'.
it_tab_fcat-coltext   = 'Movement Type'.
*it_tab-emphasize      = 'C3'.
APPEND it_tab_fcat TO ty_tab_fcat.
CLEAR it_tab_fcat.

it_tab_cnt = it_tab_cnt + 1.
it_tab_fcat-col_pos = it_tab_cnt.
it_tab_fcat-fieldname = 'MATNR'.
it_tab_fcat-coltext   = 'Material Number'.
*it_tab-emphasize      = 'C3'.
APPEND it_tab_fcat TO ty_tab_fcat.
CLEAR it_tab_fcat.


ENDFORM.
