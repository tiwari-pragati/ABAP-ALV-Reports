*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report ZPROJECT
*&---------------------------------------------------------------------*

REPORT zproject.

TABLES: kna1, knb1, lfa1, lfb1, vbak, ekko.

"---- Radio buttons for object selection ----

SELECTION-SCREEN BEGIN OF BLOCK b1k1 WITH FRAME TITLE text-001. "In this 001 - Choose your details type
PARAMETERS: p_cust RADIOBUTTON GROUP r1 USER-COMMAND rad,
            p_vend RADIOBUTTON GROUP r1,
            p_so   RADIOBUTTON GROUP r1,
            p_po   RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b1k1.

"---- Customer ----
SELECT-OPTIONS: s_kunnr FOR kna1-kunnr MODIF ID c1,
                s_bukc  FOR knb1-bukrs MODIF ID c1.

"---- Vendor ----
SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr MODIF ID c2,
                s_erdv  FOR lfa1-erdat MODIF ID c2,
                s_bukv  FOR lfb1-bukrs MODIF ID c2.

"---- Sales Order ----
SELECT-OPTIONS: s_vbel_s FOR vbak-vbeln    MODIF ID c3,
                s_erds   FOR vbak-erdat    MODIF ID c3,
                s_kuns   FOR vbak-kunnr    MODIF ID c3,
                s_buks   FOR vbak-bukrs_vf MODIF ID c3.

"---- Purchase Order ----

SELECT-OPTIONS: s_ebeln FOR ekko-ebeln MODIF ID c4,
                s_bedp  FOR ekko-bedat MODIF ID c4,
                s_lifp  FOR ekko-lifnr MODIF ID c4.

"---- Structure ----

TYPES: BEGIN OF ty_data,
         col1 TYPE char20,   " Key (PO/SO/Customer/Vendor)
         col2 TYPE dats,     " Date
         col3 TYPE char20,   " Company code / Customer / Vendor
         col4 TYPE char40,   " Name
       END OF ty_data.

DATA: it_data TYPE TABLE OF ty_data,
      wa_data TYPE ty_data,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE slis_fieldcat_alv.



AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'C1' OR screen-group1 = 'C2'
    OR screen-group1 = 'C3' OR screen-group1 = 'C4'.
      screen-active = 0.
    ENDIF.

    IF p_cust = 'X' AND screen-group1 = 'C1'.
      screen-active = 1.
    ELSEIF p_vend = 'X' AND screen-group1 = 'C2'.
      screen-active = 1.
    ELSEIF p_so   = 'X' AND screen-group1 = 'C3'.
      screen-active = 1.
    ELSEIF p_po   = 'X' AND screen-group1 = 'C4'.
      screen-active = 1.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.


" Data Fetch

START-OF-SELECTION.

  CLEAR it_data.

  "---- Customer ----

  IF p_cust = 'X'.
    SELECT a~kunnr, b~bukrs, a~name1
      FROM kna1 AS a
      INNER JOIN knb1 AS b
        ON a~kunnr = b~kunnr
      INTO ( @wa_data-col1, @wa_data-col3, @wa_data-col4 )
      WHERE a~kunnr IN @s_kunnr
        AND b~bukrs IN @s_bukc.
      wa_data-col2 = sy-datum. "Default date
      APPEND wa_data TO it_data.
    ENDSELECT.
  ENDIF.

  "---- Vendor ----

  IF p_vend = 'X'.
    SELECT a~lifnr, a~erdat, b~bukrs, a~name1
      FROM lfa1 AS a
      INNER JOIN lfb1 AS b
        ON a~lifnr = b~lifnr
      INTO ( @wa_data-col1, @wa_data-col2, @wa_data-col3, @wa_data-col4 )
      WHERE a~lifnr IN @s_lifnr
        AND a~erdat IN @s_erdv
        AND b~bukrs IN @s_bukv.
      APPEND wa_data TO it_data.
    ENDSELECT.
  ENDIF.

  "---- Sales Order ----
  IF p_so = 'X'.
    SELECT vbeln, erdat, kunnr, bukrs_vf
      FROM vbak
      INTO ( @wa_data-col1, @wa_data-col2, @wa_data-col3, @wa_data-col4 )
      WHERE vbeln   IN @s_vbel_s
        AND erdat   IN @s_erds
        AND kunnr   IN @s_kuns
        AND bukrs_vf IN @s_buks.
      APPEND wa_data TO it_data.
    ENDSELECT.
  ENDIF.

  "---- Purchase Order ----
  IF p_po = 'X'.
    SELECT ebeln, bedat, lifnr, bukrs
      FROM ekko
      INTO ( @wa_data-col1, @wa_data-col2, @wa_data-col3, @wa_data-col4 )
      WHERE ebeln IN @s_ebeln
        AND bedat IN @s_bedp
        AND lifnr IN @s_lifp.
      APPEND wa_data TO it_data.
    ENDSELECT.
  ENDIF.

  " Build Field Catalog
  PERFORM build_fieldcatalog.

  " Display ALV

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
      IT_FIELDCAT                       =  gt_fieldcat
   TABLES
      t_outtab                         = it_data.
*  EXCEPTIONS
*     PROGRAM_ERROR                     = 1
*     OTHERS                            = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

 " Form build  field catalog

FORM build_fieldcatalog.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos = 1.
  gs_fieldcat-fieldname = 'COL1'.
  gs_fieldcat-seltext_m = 'Key'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos = 2.
  gs_fieldcat-fieldname = 'COL2'.
  gs_fieldcat-seltext_m = 'Date'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos = 3.
  gs_fieldcat-fieldname = 'COL3'.
  gs_fieldcat-seltext_m = 'Field 3'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos = 4.
  gs_fieldcat-fieldname = 'COL4'.
  gs_fieldcat-seltext_m = 'Field 4'.
  APPEND gs_fieldcat TO gt_fieldcat.

ENDFORM.
