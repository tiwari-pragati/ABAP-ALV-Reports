*---------------------------------------------------------------------*

* Report ZVEND_MON_PROC_MATRIX

*---------------------------------------------------------------------*

REPORT zvend_mon_proc_matrix.

TYPE-POOLS: slis.

TABLES: mseg, lfa1, ekko, zmm_vpvend." zmm_rpavend.

"------------------ Selection Screen ------------------

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.

  SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME .

    SELECT-OPTIONS: s_werks FOR mseg-werks NO-EXTENSION NO INTERVALS OBLIGATORY,     "Plant

                    s_bsart FOR ekko-bsart,                                          "PO Doctype

                    s_lifnr FOR mseg-lifnr,                                          "Vendor Code

                    s_budat FOR mseg-budat_mkpf OBLIGATORY,                          "GRN Posting Date

                    s_bstyp FOR ekko-bstyp.                                          "PO Doc Cat

  SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.

  " Default Plant (werks)

  s_werks-sign   = 'I'.

  s_werks-option = 'EQ'.

  s_werks-low    = '1010'.

  APPEND s_werks.

  s_budat-sign   = 'I'.

  s_budat-option = 'BT'.

  CONCATENATE sy-datum(4) '0401' INTO s_budat-low.  " 01-jan-2025

  CONCATENATE sy-datum(4) '1231' INTO s_budat-high. " 31-dec-2025

  APPEND s_budat.

  "------------------ Type Definitions ------------------

  TYPES: BEGIN OF ty_final,

           type       TYPE char10,            "Blank

           rpa        TYPE char10,            "Blank

           lifnr      TYPE mseg-lifnr,        "Vendor Code

           name1      TYPE lfa1-name1,        "Vendor Name

           regio      TYPE lfa1-regio,        "State

           state_name TYPE t005u-bezei,       "State name

           ort01      TYPE lfa1-ort01,        "City

           werks      TYPE mseg-werks,        "Plant

           ktokk      TYPE lfa1-ktokk,

*          yyyymm     TYPE char6,

           "for monthly invoices

           apr_25_inv TYPE i,

           apr_25_val TYPE p DECIMALS 2,

           may_25_inv TYPE i,

           may_25_val TYPE p DECIMALS 2,

           jun_25_inv TYPE i,

           jul_25_inv TYPE i,

           aug_25_inv TYPE i,

           sep_25_inv TYPE i,

           oct_25_inv TYPE i,

           nov_25_inv TYPE i,

           dec_25_inv TYPE i,

           jan_25_inv TYPE i,

           feb_25_inv TYPE i,

           mar_25_inv TYPE i,

           tot_inv    TYPE i,

           "for invoices values

           jun_25_val TYPE p DECIMALS 2,

           jul_25_val TYPE p DECIMALS 2,

           aug_25_val TYPE p DECIMALS 2,

           sep_25_val TYPE p DECIMALS 2,

           oct_25_val TYPE p DECIMALS 2,

           nov_25_val TYPE p DECIMALS 2,

           dec_25_val TYPE p DECIMALS 2,

           jan_25_val TYPE p DECIMALS 2,

           feb_25_val TYPE p DECIMALS 2,

           mar_25_val TYPE p DECIMALS 2,

           tot_val    TYPE p DECIMALS 2,

         END OF ty_final.

  TYPES: BEGIN OF ty_temp,

           type       TYPE char10,            "Blank

           rpa        TYPE char10,            "Blank

           lifnr      TYPE mseg-lifnr,        "Vendor Code

           name1      TYPE lfa1-name1,        "Vendor Name

           regio      TYPE lfa1-regio,        "State

           state_name TYPE t005u-bezei,       "State name

           ort01      TYPE lfa1-ort01,        "City

           werks      TYPE mseg-werks,        "Plant

           yyyymm     TYPE char6,

           ktokk      TYPE lfa1-ktokk,        "Vendor Account Group

           "for monthly invoices

           apr_25_inv TYPE i,

           may_25_inv TYPE i,

           jun_25_inv TYPE i,

           jul_25_inv TYPE i,

           aug_25_inv TYPE i,

           sep_25_inv TYPE i,

           oct_25_inv TYPE i,

           nov_25_inv TYPE i,

           dec_25_inv TYPE i,

           jan_25_inv TYPE i,

           feb_25_inv TYPE i,

           mar_25_inv TYPE i,

           tot_inv    TYPE i,

           "for invoices values

           apr_25_val TYPE p DECIMALS 2,

           may_25_val TYPE p DECIMALS 2,

           jun_25_val TYPE p DECIMALS 2,

           jul_25_val TYPE p DECIMALS 2,

           aug_25_val TYPE p DECIMALS 2,

           sep_25_val TYPE p DECIMALS 2,

           oct_25_val TYPE p DECIMALS 2,

           nov_25_val TYPE p DECIMALS 2,

           dec_25_val TYPE p DECIMALS 2,

           jan_25_val TYPE p DECIMALS 2,

           feb_25_val TYPE p DECIMALS 2,

           mar_25_val TYPE p DECIMALS 2,

           tot_val    TYPE p DECIMALS 2,

         END OF ty_temp.

  TYPES: BEGIN OF ty_temp1,

           bukrs      TYPE mseg-bukrs,       "Company Code

           werks      TYPE mseg-werks,       "Plant

           lifnr      TYPE mseg-lifnr,       "Vendor Code

           mblnr      TYPE mseg-mblnr,       "Number of Material Document

           mjahr      TYPE mseg-mjahr,       "Material Document Year

           zeile      TYPE mseg-zeile,       "Item in Material Document

           ebeln      TYPE mseg-ebeln,       "Purchase order

           ebelp      TYPE mseg-ebelp,       "Item Number of Purchasing Document

           budat_mkpf TYPE mseg-budat_mkpf,  "Posting Date

           dmbtr      TYPE mseg-dmbtr,       "Net amount

           bstyp      TYPE ekko-bstyp,

           bsart      TYPE ekko-bsart,

         END OF ty_temp1.

*---------------------Data Declaration------------

  DATA: lt_mseg TYPE STANDARD TABLE OF ty_temp1,

        ls_mseg TYPE ty_temp1.

  DATA: gt_final  TYPE STANDARD TABLE OF ty_final,

        lt_portal TYPE STANDARD TABLE OF zmm_vpvend-lifnr,

        lt_rpa    TYPE STANDARD TABLE OF zmm_rpavend-lifnr,

        gs_final  TYPE ty_final.

  DATA: gt_temp TYPE STANDARD TABLE OF ty_temp,

        ls_temp TYPE ty_temp.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,

        ls_fieldcat TYPE slis_fieldcat_alv.

*******for monthly logic*************

  DATA: lv_month   TYPE c LENGTH 2,

        lv_year    TYPE c LENGTH 4,

        lv_idx     TYPE sy-tabix,

        lv_yr_char TYPE c LENGTH 2.  "for month -> name

  " Labels for field catalog

  DATA: fname(20)  TYPE c,

        dname(20)  TYPE c,

        fname1(20) TYPE c,

        dname1(20) TYPE c,

        apr1(20)   TYPE c,

        may2(20)   TYPE c,

        jun3(20)   TYPE c,

        jul4(20)   TYPE c,

        aug5(20)   TYPE c,

        sep6(20)   TYPE c,

        oct7(20)   TYPE c,

        nov8(20)   TYPE c,

        dec9(20)   TYPE c,

        jan10(20)  TYPE c,

        feb11(20)  TYPE c,

        mar12(20)  TYPE c.

  "------------------ Select Data --------------------

START-OF-SELECTION.

  DATA: lv_mm TYPE vtbbewe-atage.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      i_date_from = s_budat-low
      i_date_to   = s_budat-high
    IMPORTING
      e_months    = lv_mm.

  PERFORM get_data.

  IF gt_final IS NOT INITIAL.

    " Build field catalog and display

    PERFORM build_fieldcat.

    PERFORM display_alv.

  ELSE.

    MESSAGE 'Data is not found' TYPE 'W' DISPLAY LIKE 'I'.

  ENDIF.

*&---------------------------------------------------------------------*

*& Form Get_data

*&---------------------------------------------------------------------*

*& text

*&---------------------------------------------------------------------*

*& -->  p1        text

*& <--  p2        text

*&---------------------------------------------------------------------*

FORM get_data .

  " Get MSEG records (only movement type 101 )

  SELECT a~bukrs,

        a~werks,

        a~lifnr,

        a~mblnr,

        a~mjahr,

        a~zeile,

        a~ebeln,

        a~ebelp,

        a~budat_mkpf,

        a~dmbtr,

        b~bstyp,

        b~bsart

   INTO TABLE @lt_mseg FROM mseg AS a  INNER JOIN ekko AS b   ON b~ebeln = a~ebeln

  WHERE a~werks     IN @s_werks

    AND a~lifnr     IN @s_lifnr

    AND a~budat_mkpf IN @s_budat

    AND a~bwart     = '101'

    AND b~bsart     IN @s_bsart

    AND b~bstyp     IN @s_bstyp.

  IF lt_mseg IS NOT INITIAL.

    SELECT lifnr, name1, regio, ort01, land1,ktokk

           FROM lfa1 INTO TABLE @DATA(lt_lfa1)

           FOR ALL ENTRIES IN @lt_mseg

            WHERE lifnr = @lt_mseg-lifnr ORDER BY PRIMARY KEY.

    SELECT spras, land1, bland, bezei

            FROM t005u

            INTO TABLE @DATA(lt_t005u)

            FOR ALL ENTRIES IN @lt_lfa1

            WHERE land1 = 'IN' AND bland = @lt_lfa1-regio.

    SELECT lifnr FROM zmm_vpvend INTO TABLE @lt_portal.

    SELECT lifnr FROM zmm_rpavend INTO TABLE @lt_rpa.

  ENDIF.

  " Remove empty vendor rows

  DELETE lt_mseg WHERE lifnr IS INITIAL.

  " Sort for structured processing

  SORT lt_mseg BY bukrs werks lifnr mblnr mjahr zeile budat_mkpf.

  LOOP AT lt_mseg INTO ls_mseg.

    " mark portal

    READ TABLE lt_portal WITH KEY table_line = ls_mseg-lifnr TRANSPORTING NO FIELDS.    "--- Portal check from ZMM_VPVEND ---

    IF sy-subrc = 0.

      ls_temp-type = 'VP'.

    ELSE.

      ls_temp-type = 'NP'.

    ENDIF.

    READ TABLE lt_rpa WITH KEY table_line = ls_mseg-lifnr TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.

      ls_temp-rpa  = 'Yes'.

    ELSE.

      ls_temp-rpa  = 'No'.

    ENDIF.

    ls_temp-lifnr       = ls_mseg-lifnr.

    SHIFT ls_temp-lifnr LEFT DELETING LEADING '0'.

    ls_temp-lifnr       = ls_mseg-lifnr.

    SHIFT ls_temp-lifnr LEFT DELETING LEADING '0'.

    READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = ls_mseg-lifnr.

    IF sy-subrc = 0.

      ls_temp-name1 = ls_lfa1-name1.

      ls_temp-ort01 = ls_lfa1-ort01.

      ls_temp-ktokk = ls_lfa1-ktokk.

      READ TABLE lt_t005u INTO DATA(ls_t005u) WITH KEY land1 = 'IN'   bland = ls_lfa1-regio.

*        spras = sy-langu.

      IF sy-subrc = 0.

        ls_temp-state_name = ls_t005u-bezei.

      ENDIF.

    ENDIF.

    ls_temp-werks = ls_mseg-werks.

    IF ls_mseg-budat_mkpf+4(2)     = '04'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-apr_25_inv  = 1.

      ls_temp-apr_25_val  = ls_mseg-dmbtr.

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '05'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-may_25_inv  = 1.

      ls_temp-may_25_val  = ls_mseg-dmbtr.

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '06'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-jun_25_inv  = 1.

      ls_temp-jun_25_val  = ls_mseg-dmbtr.

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '07'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-jul_25_inv  = 1.

      ls_temp-jul_25_val  = ls_mseg-dmbtr.

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '08'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-aug_25_inv  = 1.

      ls_temp-aug_25_val  = ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '09'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-sep_25_inv  = 1.

      ls_temp-sep_25_val  = ls_mseg-dmbtr.

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '10'.

      ls_temp-yyyymm   = ls_mseg-budat_mkpf+0(6) .

      ls_temp-oct_25_inv  = 1.

      ls_temp-oct_25_val  = ls_mseg-dmbtr.

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '11'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-nov_25_inv  = 1.

      ls_temp-nov_25_val  = ls_mseg-dmbtr.

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '12'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-dec_25_inv  = 1.

      ls_temp-dec_25_val  = ls_mseg-dmbtr.

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '01'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-jan_25_inv  = 1.

      ls_temp-jan_25_val  = ls_mseg-dmbtr.

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '02'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-feb_25_inv  = 1.

      ls_temp-feb_25_val  = ls_mseg-dmbtr .

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ELSEIF ls_mseg-budat_mkpf+4(2) = '03'.

      ls_temp-yyyymm      = ls_mseg-budat_mkpf+0(6) .

      ls_temp-mar_25_inv  = 1.

      ls_temp-mar_25_val  = ls_mseg-dmbtr .

      ls_temp-tot_inv     = ls_temp-tot_inv + 1.

      ls_temp-tot_val     = ls_temp-tot_val + ls_mseg-dmbtr.

    ENDIF.

    APPEND ls_temp TO gt_temp.

    CLEAR:ls_temp.

  ENDLOOP.

  SORT gt_temp BY lifnr yyyymm.

* Use COLLECT to summarize the data

  LOOP AT gt_temp INTO ls_temp.

    MOVE-CORRESPONDING ls_temp TO gs_final.

    COLLECT gs_final INTO gt_final.

  ENDLOOP.

ENDFORM.

"------------------ ALV Display Form ------------------

FORM  display_alv.

  DATA: ls_layout TYPE slis_layout_alv.

  ls_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat
    TABLES
      t_outtab           = gt_final
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.

*---------------------------------------------------------------------*

* Field Catalog Build

*---------------------------------------------------------------------*

FORM build_fieldcat.

  DATA: lv_months   TYPE i,

        lv_yy       TYPE char4,

        lv_yy1      TYPE char4,

        lv_date     TYPE sy-datum,

        lv_yr_char  TYPE c LENGTH 2,

        lv_yr_char1 TYPE c LENGTH 2.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'WERKS'.

  ls_fieldcat-seltext_m = 'Plant'.

*  ls_fieldcat-outputlen  = 0.

  ls_fieldcat-just       = 'L'.

  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname  = 'KTOKK'.

  ls_fieldcat-seltext_m  = 'VAGrp'.

*  ls_fieldcat-outputlen  = 0.

  ls_fieldcat-just       = 'L'.

  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'LIFNR'.

  ls_fieldcat-seltext_m = 'VendCode'.

*  ls_fieldcat-outputlen  = 0.

  ls_fieldcat-just       = 'L'.

  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'NAME1'.

  ls_fieldcat-seltext_m = 'Vendor Name'.

*  ls_fieldcat-outputlen  = 0.

*  ls_fieldcat-col_opt    = 'X'.

  ls_fieldcat-just       = 'L'.

  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'ORT01'.

  ls_fieldcat-seltext_m = 'City'.

*  ls_fieldcat-outputlen  = 0.

  ls_fieldcat-just       = 'L'.

  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'STATE_NAME'.

  ls_fieldcat-seltext_m = 'State'.

*  ls_fieldcat-outputlen  = 0.

  ls_fieldcat-just       = 'L'.

  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'TYPE'.

  ls_fieldcat-seltext_m = 'Type'.

*  ls_fieldcat-outputlen  = 0.

  ls_fieldcat-just       = 'L'.

  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'RPA'.

  ls_fieldcat-seltext_m = 'RPA'.

*  ls_fieldcat-outputlen  = 0.

  ls_fieldcat-just       = 'L'.

  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.

  lv_date = s_budat-low.

  CLEAR:lv_yr_char,  lv_yr_char1.

  " ========== First loop for INV ==========

  DO lv_mm TIMES.

    IF lv_yr_char IS INITIAL.

      lv_yr_char = s_budat-low+2(2).

      lv_months = s_budat-low+4(2).

      lv_yy1 = lv_yy = s_budat-low+0(4).

      lv_yr_char1 = lv_months.

    ENDIF.

    "last-two-digits of start year

    IF lv_yr_char1     = '4' AND lv_yy = lv_yy1.

      CONCATENATE 'Apr_' lv_yr_char '_Inv' INTO fname."apr1.

      CONCATENATE 'Apr_' lv_yr_char '_Inv' INTO dname."apr1.

    ELSEIF lv_yr_char1 = '5' AND lv_yy = lv_yy1.

      CONCATENATE 'May_' lv_yr_char '_Inv' INTO fname.""may2.

      CONCATENATE 'May_' lv_yr_char '_Inv' INTO dname."

    ELSEIF lv_yr_char1 = '6' AND lv_yy = lv_yy1.

      CONCATENATE 'Jun_' lv_yr_char '_Inv' INTO fname."jun3.

      CONCATENATE 'Jun_' lv_yr_char '_Inv' INTO dname."

    ELSEIF lv_yr_char1 = '7' AND lv_yy = lv_yy1.

      CONCATENATE 'Jul_' lv_yr_char '_Inv' INTO fname."jul4.

      CONCATENATE 'Jul_' lv_yr_char '_Inv' INTO dname."

    ELSEIF lv_yr_char1 = '8' AND lv_yy = lv_yy1.

      CONCATENATE 'Aug_' lv_yr_char '_Inv' INTO fname."aug5.

      CONCATENATE 'Aug_' lv_yr_char '_Inv' INTO dname."

    ELSEIF lv_yr_char1 = '9' AND lv_yy = lv_yy1.

      CONCATENATE 'Sep_' lv_yr_char '_Inv' INTO fname."sep6.

      CONCATENATE 'Sep_' lv_yr_char '_Inv' INTO dname."

    ELSEIF lv_yr_char1 = '10' AND lv_yy = lv_yy1.

      CONCATENATE 'Oct_' lv_yr_char '_Inv' INTO fname."oct7.

      CONCATENATE 'Oct_' lv_yr_char '_Inv' INTO dname.".

    ELSEIF lv_yr_char1 = '11' AND lv_yy = lv_yy1.

      CONCATENATE 'Nov_' lv_yr_char '_Inv' INTO fname."nov8.

      CONCATENATE 'Nov_' lv_yr_char '_Inv' INTO dname.".

    ELSEIF lv_yr_char1 = '12' AND lv_yy = lv_yy1.

      CONCATENATE 'Dec_' lv_yr_char '_Inv' INTO fname."dec9.

      CONCATENATE 'Dec_' lv_yr_char '_Inv' INTO dname.".

    ELSEIF lv_yr_char1 = '1' AND  lv_yy = lv_yy1.

      CONCATENATE 'Jan_' lv_yr_char '_Inv' INTO fname."jan10.

      CONCATENATE 'Jan_' lv_yr_char '_Inv' INTO dname."

    ELSEIF lv_yr_char1 = '2' AND lv_yy = lv_yy1.

      CONCATENATE 'Feb_' lv_yr_char '_Inv' INTO fname."feb11.

      CONCATENATE 'Feb_' lv_yr_char '_Inv' INTO dname."

    ELSEIF lv_yr_char1 = '3' AND lv_yy = lv_yy1.

      CONCATENATE 'Mar_' lv_yr_char '_Inv' INTO fname."mar12.

      CONCATENATE 'Mar_' lv_yr_char '_Inv' INTO dname."

    ENDIF.

* For each selected month in s_budat

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = fname."'APR_25_INV'.
    ls_fieldcat-seltext_m = dname."apr1 && '-Inv'.
    ls_fieldcat-just       = 'R'.
    APPEND ls_fieldcat TO lt_fieldcat.

    IF lv_yr_char1 EQ 12.

      CLEAR:    lv_yr_char1.

      lv_yr_char1 = lv_yr_char1 + 1.

      lv_yy = lv_yy1 = lv_yy1 + 1.

      lv_yr_char =  lv_yr_char + 1.

    ELSE.

      lv_yr_char1 = lv_yr_char1 + 1.

      lv_yy = lv_yy1.

    ENDIF.

  ENDDO.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'TOT_INV'.

  ls_fieldcat-seltext_m = 'Total Inv'.

*  ls_fieldcat-outputlen  = 0.

  ls_fieldcat-just       = 'R'.

  APPEND ls_fieldcat TO lt_fieldcat.

*================================

  lv_date = s_budat-low.
  CLEAR: lv_yr_char, lv_yr_char1.
  DO lv_mm TIMES.
    IF lv_yr_char IS INITIAL.
      lv_yr_char  = s_budat-low+2(2).
      lv_months   = s_budat-low+4(2).
      lv_yy1      = lv_yy = s_budat-low+0(4).
      lv_yr_char1 = lv_months.
    ENDIF.

    IF lv_yr_char1     = '4' AND lv_yy = lv_yy1.

      CONCATENATE 'Apr_' lv_yr_char '_Val' INTO fname1."apr1.

      CONCATENATE 'Apr_' lv_yr_char '_Val' INTO dname1."apr1.


    ELSEIF lv_yr_char1 = '5' AND lv_yy = lv_yy1.

      CONCATENATE 'May_' lv_yr_char '_Val' INTO fname1.""may2.

      CONCATENATE 'May_' lv_yr_char '_Val' INTO dname1."

    ELSEIF lv_yr_char1 = '6' AND lv_yy = lv_yy1.

      CONCATENATE 'Jun_' lv_yr_char '_Val' INTO fname1."jun3.

      CONCATENATE 'Jun_' lv_yr_char '_Val' INTO dname1."

    ELSEIF lv_yr_char1 = '7' AND lv_yy = lv_yy1.

      CONCATENATE 'Jul_' lv_yr_char '_Val' INTO fname1."jul4.

      CONCATENATE 'Jul_' lv_yr_char '_Val' INTO dname1."


    ELSEIF lv_yr_char1 = '8' AND lv_yy = lv_yy1.
      CONCATENATE 'Aug_' lv_yr_char '_Val' INTO fname1."aug5.

      CONCATENATE 'Aug_' lv_yr_char '_Val' INTO dname1."

    ELSEIF lv_yr_char1 = '9' AND lv_yy = lv_yy1.

      CONCATENATE 'Sep_' lv_yr_char '_Val' INTO fname1."sep6.

      CONCATENATE 'Sep_' lv_yr_char '_Val' INTO dname1."

    ELSEIF lv_yr_char1 = '10' AND lv_yy = lv_yy1.

      CONCATENATE 'Oct_' lv_yr_char '_Val' INTO fname1."oct7.

      CONCATENATE 'Oct_' lv_yr_char '_Val' INTO dname1."

    ELSEIF lv_yr_char1 = '11' AND lv_yy = lv_yy1.

      CONCATENATE 'Nov_' lv_yr_char '_Val' INTO fname1."nov8.

      CONCATENATE 'Nov_' lv_yr_char '_Val' INTO dname1."

    ELSEIF lv_yr_char1 = '12' AND lv_yy = lv_yy1.

      CONCATENATE 'Dec_' lv_yr_char '_Val' INTO fname1."dec9.

      CONCATENATE 'Dec_' lv_yr_char '_Val' INTO dname1."

    ELSEIF lv_yr_char1 = '1' AND  lv_yy = lv_yy1.

      CONCATENATE 'Jan_' lv_yr_char '_Val' INTO fname1."jan10.

      CONCATENATE 'Jan_' lv_yr_char '_Val' INTO dname1."

    ELSEIF lv_yr_char1 = '2' AND lv_yy = lv_yy1.

      CONCATENATE 'Feb_' lv_yr_char '_Val' INTO fname1."feb11.

      CONCATENATE 'Feb_' lv_yr_char '_Val' INTO dname1."
    ELSEIF lv_yr_char1 = '3' AND lv_yy = lv_yy1.

      CONCATENATE 'Mar_' lv_yr_char '_Val' INTO fname1."mar12.

      CONCATENATE 'Mar_' lv_yr_char '_Val' INTO dname1."

    ENDIF.

    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = fname1."'APR_25_INV'.

    ls_fieldcat-seltext_m = dname1."apr1 && '-Inv'.

    ls_fieldcat-just       = 'R'.

    APPEND ls_fieldcat TO lt_fieldcat.

    IF lv_yr_char1 EQ 12.
      CLEAR lv_yr_char1.
      lv_yr_char1 = lv_yr_char1 + 1.
      lv_yy  = lv_yy1 = lv_yy1 + 1.
      lv_yr_char = lv_yr_char + 1.
    ELSE.
      lv_yr_char1 = lv_yr_char1 + 1.
      lv_yy = lv_yy1.
    ENDIF.
  ENDDO.



  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'TOT_VAL'.

  ls_fieldcat-seltext_m = 'Total Val'.

*  ls_fieldcat-outputlen  = 0.

  ls_fieldcat-just       = 'R'.

  APPEND ls_fieldcat TO lt_fieldcat.

ENDFORM.
