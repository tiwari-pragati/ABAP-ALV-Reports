*---------------------------------------------------------------------*
* Report ZVEND_MAT_DEL_REJ.
*---------------------------------------------------------------------*
REPORT zvend_mat_del_rej.

TYPE-POOLS: slis.

TABLES: mseg, lfa1, ekko, zmm_vpvend, zmm_rpavend, makt.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS :  s_werks FOR mseg-werks NO-EXTENSION NO INTERVALS OBLIGATORY,     "Plant
                    s_bsart FOR ekko-bsart,                                          "PO Doctype
                    s_lifnr FOR mseg-lifnr,                                          "Vendor Code
                    s_matnr FOR mseg-matnr,                                          "Material
                    s_budat FOR mseg-budat_mkpf OBLIGATORY,                          "GRN Posting Date
                    s_bstyp FOR ekko-bstyp.
SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION.
  s_werks-sign = 'I'. s_werks-option = 'EQ'. s_werks-low = '1010'. APPEND s_werks.
  s_budat-sign = 'I'. s_budat-option = 'BT'.
  CONCATENATE sy-datum(4) '0401' INTO s_budat-low.
  s_budat-high = s_budat-low + 364.
  APPEND s_budat.

  TYPES: BEGIN OF ty_final,
           lifnr      TYPE mseg-lifnr,        "Vendor Code
           name1      TYPE lfa1-name1,        "Vendor Name
           matnr      TYPE mseg-matnr,        "Material Number
           maktx      TYPE makt-maktx,        "Material Description
           werks      TYPE mseg-werks,        "Plant
           regio      TYPE lfa1-regio,        "State
           state_name TYPE t005u-bezei,       "State name
           ort01      TYPE lfa1-ort01,        "City
           ktokk      TYPE lfa1-ktokk,        "Vendor Account Group
           type       TYPE char10,            "VP/NP
           rpa        TYPE char10,            "Yes/No
           "Monthly GR Quantities
           apr_gr     TYPE menge_d,
           may_gr     TYPE menge_d,
           jun_gr     TYPE menge_d,
           jul_gr     TYPE menge_d,
           aug_gr     TYPE menge_d,
           sep_gr     TYPE menge_d,
           oct_gr     TYPE menge_d,
           nov_gr     TYPE menge_d,
           dec_gr     TYPE menge_d,
           jan_gr     TYPE menge_d,
           feb_gr     TYPE menge_d,
           mar_gr     TYPE menge_d,
           tot_gr     TYPE menge_d,
           "Monthly Rejection Quantities
           apr_rej    TYPE menge_d,
           may_rej    TYPE menge_d,
           jun_rej    TYPE menge_d,
           jul_rej    TYPE menge_d,
           aug_rej    TYPE menge_d,
           sep_rej    TYPE menge_d,
           oct_rej    TYPE menge_d,
           nov_rej    TYPE menge_d,
           dec_rej    TYPE menge_d,
           jan_rej    TYPE menge_d,
           feb_rej    TYPE menge_d,
           mar_rej    TYPE menge_d,
           tot_rej    TYPE menge_d,
           "Rejection Percentage
           rej_perc   TYPE char10,
         END OF ty_final.

  TYPES: BEGIN OF ty_temp,
           lifnr      TYPE mseg-lifnr,
           matnr      TYPE mseg-matnr,
           name1      TYPE lfa1-name1,
           maktx      TYPE makt-maktx,
           werks      TYPE mseg-werks,
           regio      TYPE lfa1-regio,
           state_name TYPE t005u-bezei,
           ort01      TYPE lfa1-ort01,
           ktokk      TYPE lfa1-ktokk,
           type       TYPE char10,
           rpa        TYPE char10,
           yyyymm     TYPE char6,
           bwart      TYPE mseg-bwart,
           "Monthly GR Quantities
           apr_gr     TYPE menge_d,
           may_gr     TYPE menge_d,
           jun_gr     TYPE menge_d,
           jul_gr     TYPE menge_d,
           aug_gr     TYPE menge_d,
           sep_gr     TYPE menge_d,
           oct_gr     TYPE menge_d,
           nov_gr     TYPE menge_d,
           dec_gr     TYPE menge_d,
           jan_gr     TYPE menge_d,
           feb_gr     TYPE menge_d,
           mar_gr     TYPE menge_d,
           tot_gr     TYPE menge_d,
           "Monhly Rejection Quantities
           apr_rej    TYPE menge_d,
           may_rej    TYPE menge_d,
           jun_rej    TYPE menge_d,
           jul_rej    TYPE menge_d,
           aug_rej    TYPE menge_d,
           sep_rej    TYPE menge_d,
           oct_rej    TYPE menge_d,
           nov_rej    TYPE menge_d,
           dec_rej    TYPE menge_d,
           jan_rej    TYPE menge_d,
           feb_rej    TYPE menge_d,
           mar_rej    TYPE menge_d,
           tot_rej    TYPE menge_d,
           "Rejection Percentage
           rej_perc   TYPE char10,

         END OF ty_temp.

  TYPES: BEGIN OF ty_temp1,
           bukrs      TYPE mseg-bukrs,
           werks      TYPE mseg-werks,
           lifnr      TYPE mseg-lifnr,
           matnr      TYPE mseg-matnr,        "Material Number
           mblnr      TYPE mseg-mblnr,
           mjahr      TYPE mseg-mjahr,
           zeile      TYPE mseg-zeile,
           ebeln      TYPE mseg-ebeln,
           ebelp      TYPE mseg-ebelp,
           budat_mkpf TYPE mseg-budat_mkpf,
           menge      TYPE mseg-menge,
           bwart      TYPE mseg-bwart,
           bstyp      TYPE ekko-bstyp,
           bsart      TYPE ekko-bsart,
         END OF ty_temp1.

  DATA : lt_mseg  TYPE STANDARD TABLE OF ty_temp1,
         ls_mseg  TYPE ty_temp1,
         gt_final TYPE STANDARD TABLE OF ty_final,
         gs_final TYPE ty_final.

  DATA : lt_portal TYPE STANDARD TABLE OF zmm_vpvend,
         lt_rpa    TYPE STANDARD TABLE OF zmm_rpavend,
         lt_lfa1   TYPE STANDARD TABLE OF lfa1,
         lt_makt   TYPE STANDARD TABLE OF makt,
         lt_t005u  TYPE STANDARD TABLE OF t005u,
         ls_lfa1   TYPE lfa1,
         ls_makt   TYPE makt.

  DATA : gt_temp TYPE STANDARD TABLE OF ty_temp,
         ls_temp TYPE ty_temp.

  DATA : lt_fieldcat TYPE slis_t_fieldcat_alv,
         ls_fieldcat TYPE slis_fieldcat_alv.

*************** for monthly logic ***************
  DATA: lv_month   TYPE c LENGTH 2,
        lv_year    TYPE c LENGTH 4,
        lv_idx     TYPE sy-tabix,
        lv_yr_char TYPE c LENGTH 2.

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

  IF lv_mm LE 12.
    PERFORM get_data.
    IF gt_final IS NOT INITIAL.
      " Build field catalog and display
      PERFORM build_fieldcat.
      PERFORM display_alv.
    ELSE.
      MESSAGE 'Data is not found' TYPE 'I'.
    ENDIF.
  ELSE.
    MESSAGE 'Please select maximum one Year Data Range.' TYPE 'I'.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .
  DATA: lv_lifnr TYPE lfa1-lifnr,
        lv_matnr TYPE mara-matnr,
        lv_per   TYPE p DECIMALS 2.

  " Get MSEG records
  SELECT a~bukrs,
         a~werks,
         a~lifnr,
         a~matnr,
         a~mblnr,
         a~mjahr,
         a~zeile,
         a~ebeln,
         a~ebelp,
         a~budat_mkpf,
         a~menge,
         a~bwart,
         b~bstyp,
         b~bsart
    INTO TABLE @lt_mseg
    FROM mseg AS a
    INNER JOIN ekko AS b  ON b~ebeln = a~ebeln
    WHERE a~werks      IN @s_werks
     AND  a~lifnr      IN @s_lifnr
     AND  a~matnr      IN @s_matnr
     AND  a~budat_mkpf IN @s_budat
     AND  a~bwart      IN ('101','122')
     AND  b~bsart      IN @s_bsart
     AND  b~bstyp      IN @s_bstyp.

  IF lt_mseg IS NOT INITIAL.
    "Vendor Data
    SELECT lifnr, name1, regio, ort01, land1, ktokk
           FROM lfa1 INTO TABLE @DATA(lt_lfa1)
           FOR ALL ENTRIES IN @lt_mseg
            WHERE lifnr = @lt_mseg-lifnr ORDER BY PRIMARY KEY..

    "Material Description
    SELECT matnr, maktx
           FROM makt INTO TABLE @DATA(lt_makt)
           FOR ALL ENTRIES IN @lt_mseg
            WHERE matnr = @lt_mseg-matnr
            AND spras = @sy-langu.

    "State Names
    SELECT spras, land1, bland, bezei
            FROM t005u
            INTO TABLE @DATA(lt_t005u)
            FOR ALL ENTRIES IN @lt_lfa1
            WHERE land1 = 'IN'
            AND bland = @lt_lfa1-regio
            AND spras = @sy-langu.

    "Portal Vendors
    SELECT * FROM zmm_vpvend INTO TABLE @lt_portal
        FOR ALL ENTRIES IN @lt_mseg
        WHERE lifnr = @lt_mseg-lifnr AND
        enddt GE @sy-datum ORDER BY PRIMARY KEY.

    "RPA Vendors
    SELECT * FROM zmm_rpavend INTO TABLE @lt_rpa
        FOR ALL ENTRIES IN @lt_mseg
        WHERE lifnr = @lt_mseg-lifnr AND
        enddt GE @sy-datum ORDER BY PRIMARY KEY..
  ENDIF.

  "INITIALIZE GT_TEMP
  REFRESH: gt_temp.

  LOOP AT lt_mseg INTO ls_mseg.

    CLEAR: ls_temp.

    "Vendor + Material combination ke liye
    ls_temp-lifnr = ls_mseg-lifnr.
    ls_temp-matnr = ls_mseg-matnr.
    ls_temp-bwart = ls_mseg-bwart.

    SHIFT ls_temp-lifnr LEFT DELETING LEADING '0'.
    SHIFT ls_temp-matnr LEFT DELETING LEADING '0'.

    "Vendor Data
    READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = ls_mseg-lifnr.
    IF sy-subrc = 0.
      ls_temp-name1 = ls_lfa1-name1.
      ls_temp-ort01 = ls_lfa1-ort01.
      ls_temp-regio = ls_lfa1-regio.
      ls_temp-ktokk = ls_lfa1-ktokk.

      READ TABLE lt_t005u INTO DATA(ls_t005u) WITH KEY land1 = 'IN' bland = ls_lfa1-regio.
      IF sy-subrc = 0.
        ls_temp-state_name = ls_t005u-bezei.
      ELSE.
        CLEAR ls_temp-state_name.
      ENDIF.
    ELSE.
      "Vendor not found, skip
      CONTINUE.
    ENDIF.

    "Material Description
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_mseg-matnr.
    IF sy-subrc = 0.
      ls_temp-maktx = ls_makt-maktx.
    ELSE.
      CLEAR ls_temp-maktx.
    ENDIF.

    "Portal Check
    READ TABLE lt_portal WITH KEY lifnr = ls_mseg-lifnr TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ls_temp-type = 'VP'.
    ELSE.
      ls_temp-type = 'NP'.
    ENDIF.

    "RPA Check
    READ TABLE lt_rpa WITH KEY lifnr = ls_mseg-lifnr TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ls_temp-rpa = 'Yes'.
    ELSE.
      ls_temp-rpa = 'No'.
    ENDIF.

    ls_temp-werks = ls_mseg-werks.

    DATA(lv_month) = ls_mseg-budat_mkpf+4(2).

    " Goods Receipt (101)
    IF ls_mseg-bwart = '101'.        "GR entries
      IF lv_month = '04'.
        ls_temp-apr_gr = ls_mseg-menge.
        ls_temp-tot_gr   = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '05'.
        ls_temp-may_gr = ls_mseg-menge.
        ls_temp-tot_gr   = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '06'.
        ls_temp-jun_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '07'.
        ls_temp-jul_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '08'.
        ls_temp-aug_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '09'.
        ls_temp-sep_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '10'.
        ls_temp-oct_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '11'.
        ls_temp-nov_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '12'.
        ls_temp-dec_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '01'.
        ls_temp-jan_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '02'.
        ls_temp-feb_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ELSEIF lv_month = '03'.
        ls_temp-mar_gr = ls_mseg-menge.
        ls_temp-tot_gr = ls_temp-tot_gr + ls_mseg-menge.
      ENDIF.

      " Rejection (122)
    ELSEIF ls_mseg-bwart = '122'.      "Rejection entries
      IF lv_month = '04'.
        ls_temp-apr_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '05'.
        ls_temp-may_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '06'.
        ls_temp-jun_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '07'.
        ls_temp-jul_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '08'.
        ls_temp-aug_rej = ls_mseg-menge.
        ls_temp-tot_rej = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '09'.
        ls_temp-sep_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '10'.
        ls_temp-oct_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '11'.
        ls_temp-nov_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '12'.
        ls_temp-dec_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '01'.
        ls_temp-jan_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '02'.
        ls_temp-feb_rej = ls_mseg-menge.
        ls_temp-tot_rej  = ls_temp-tot_rej + ls_mseg-menge.
      ELSEIF lv_month = '03'.
        ls_temp-mar_rej = ls_mseg-menge.
        ls_temp-tot_rej   = ls_temp-tot_rej + ls_mseg-menge.
      ENDIF.
    ENDIF.

    APPEND ls_temp TO gt_temp.
  ENDLOOP.

  "DELETE RECORDS WITH BLANK VENDOR, MATERIAL, STATE OR CITY
  DELETE gt_temp WHERE lifnr IS INITIAL
                  OR matnr IS INITIAL
                  OR name1 IS INITIAL
                  OR ort01 IS INITIAL
                  OR state_name IS INITIAL.

  SORT gt_temp BY lifnr matnr.

  "INITIALIZE GT_FINAL
  REFRESH: gt_final.

  "collect Vendor + Material wise data
  LOOP AT gt_temp INTO ls_temp.
    AT NEW matnr.
      CLEAR: gs_final.
    ENDAT.

    " set Key fields
    gs_final-lifnr = ls_temp-lifnr.
    gs_final-matnr = ls_temp-matnr.
    gs_final-name1 = ls_temp-name1.
    gs_final-maktx = ls_temp-maktx.
    gs_final-werks = ls_temp-werks.
    gs_final-ort01 = ls_temp-ort01.
    gs_final-state_name = ls_temp-state_name.
    gs_final-type = ls_temp-type.
    gs_final-rpa = ls_temp-rpa.

    " Sum Monthly quantities
    gs_final-apr_gr  = gs_final-apr_gr  + ls_temp-apr_gr.
    gs_final-may_gr  = gs_final-may_gr  + ls_temp-may_gr.
    gs_final-jun_gr  = gs_final-jun_gr  + ls_temp-jun_gr.
    gs_final-jul_gr  = gs_final-jul_gr  + ls_temp-jul_gr.
    gs_final-aug_gr  = gs_final-aug_gr  + ls_temp-aug_gr.
    gs_final-sep_gr  = gs_final-sep_gr  + ls_temp-sep_gr.
    gs_final-oct_gr  = gs_final-oct_gr  + ls_temp-oct_gr.
    gs_final-nov_gr  = gs_final-nov_gr  + ls_temp-nov_gr.
    gs_final-dec_gr  = gs_final-dec_gr  + ls_temp-dec_gr.
    gs_final-jan_gr  = gs_final-jan_gr  + ls_temp-jan_gr.
    gs_final-feb_gr  = gs_final-feb_gr  + ls_temp-feb_gr.
    gs_final-mar_gr  = gs_final-mar_gr  + ls_temp-mar_gr.
    gs_final-tot_gr  = gs_final-tot_gr     + ls_temp-tot_gr.

    gs_final-apr_rej = gs_final-apr_rej + ls_temp-apr_rej.
    gs_final-may_rej = gs_final-may_rej + ls_temp-may_rej.
    gs_final-jun_rej = gs_final-jun_rej + ls_temp-jun_rej.
    gs_final-jul_rej = gs_final-jul_rej + ls_temp-jul_rej.
    gs_final-aug_rej = gs_final-aug_rej + ls_temp-aug_rej.
    gs_final-sep_rej = gs_final-sep_rej + ls_temp-sep_rej.
    gs_final-oct_rej = gs_final-oct_rej + ls_temp-oct_rej.
    gs_final-nov_rej = gs_final-nov_rej + ls_temp-nov_rej.
    gs_final-dec_rej = gs_final-dec_rej + ls_temp-dec_rej.
    gs_final-jan_rej = gs_final-jan_rej + ls_temp-jan_rej.
    gs_final-feb_rej = gs_final-feb_rej + ls_temp-feb_rej.
    gs_final-mar_rej = gs_final-mar_rej + ls_temp-mar_rej.
    gs_final-tot_rej = gs_final-tot_rej    + ls_temp-tot_rej.

    AT END OF matnr.
      " Calculate rejection percentage - FINAL calculation
      IF gs_final-tot_gr > 0.
        lv_per = ( gs_final-tot_rej / gs_final-tot_gr ) * 100.

        WRITE lv_per TO gs_final-rej_perc DECIMALS 2.
        CONDENSE gs_final-rej_perc.  " Remove extra spaces
        CONCATENATE gs_final-rej_perc '%' INTO gs_final-rej_perc.
      ELSE.
        gs_final-rej_perc = 0.
      ENDIF.

      APPEND gs_final TO gt_final.
    ENDAT.
  ENDLOOP.

  "DELETE blank Record from final table
  DELETE gt_final WHERE lifnr IS INITIAL
                   OR matnr IS INITIAL
                   OR name1 IS INITIAL
                   OR ort01 IS INITIAL
                   OR state_name IS INITIAL.

  SORT gt_final BY werks lifnr matnr.
ENDFORM.


FORM  display_alv.
  DATA: ls_layout TYPE slis_layout_alv.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat
      i_save             = 'U'
    TABLES
      t_outtab           = gt_final
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.


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
  ls_fieldcat-just       = 'L'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LIFNR'.
  ls_fieldcat-seltext_m = 'VendCode'.
  ls_fieldcat-just       = 'L'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NAME1'.
  ls_fieldcat-seltext_m = 'Vendor Name'.
  ls_fieldcat-just       = 'L'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ORT01'.
  ls_fieldcat-seltext_m = 'City'.
  ls_fieldcat-just       = 'L'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE_NAME'.
  ls_fieldcat-seltext_m = 'State'.
  ls_fieldcat-just       = 'L'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TYPE'.
  ls_fieldcat-seltext_m = 'VP'.
  ls_fieldcat-just       = 'L'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'RPA'.
  ls_fieldcat-seltext_m = 'RPA'.
  ls_fieldcat-just       = 'L'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MATNR'.
  ls_fieldcat-seltext_m = 'Material'.
  ls_fieldcat-just       = 'L'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MAKTX'.
  ls_fieldcat-seltext_m = 'Material text'.
  ls_fieldcat-just       = 'L'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  lv_date = s_budat-low.
  CLEAR:lv_yr_char,  lv_yr_char1.

  DO lv_mm TIMES.
    IF lv_yr_char IS INITIAL.
      lv_yr_char = s_budat-low+2(2).
      lv_months = s_budat-low+4(2).
      lv_yy1 = lv_yy = s_budat-low+0(4).
      lv_yr_char1 = lv_months.
    ENDIF.
    "last-two-digits of start year
    IF lv_yr_char1     = '4' AND lv_yy = lv_yy1.
*      CONCATENATE 'APR_GR' INTO fname."apr1.
      fname = 'APR_GR'.
      CONCATENATE 'Apr' lv_yr_char '-GR'  INTO dname."apr1.
    ELSEIF lv_yr_char1 = '5' AND lv_yy = lv_yy1.
*      CONCATENATE 'MAY_' lv_yr_char '_GR' INTO fname.""may2.
      fname = 'MAY_GR'.
      CONCATENATE 'May' lv_yr_char '-GR' INTO dname."
    ELSEIF lv_yr_char1 = '6' AND lv_yy = lv_yy1.
*      CONCATENATE 'JUN_' lv_yr_char '_GR' INTO fname."jun3.
      fname = 'JUN_GR'.
      CONCATENATE 'Jun' lv_yr_char '-GR' INTO dname."
    ELSEIF lv_yr_char1 = '7' AND lv_yy = lv_yy1.
*      CONCATENATE 'JUL_' lv_yr_char '_GR' INTO fname."jul4.
      fname = 'JUL_GR'.
      CONCATENATE 'Jul' lv_yr_char '-GR' INTO dname."
    ELSEIF lv_yr_char1 = '8' AND lv_yy = lv_yy1.
*      CONCATENATE 'AUG_' lv_yr_char '_GR' INTO fname."aug5.
      fname = 'AUG_GR'.
      CONCATENATE 'Aug' lv_yr_char '-GR' INTO dname."
    ELSEIF lv_yr_char1 = '9' AND lv_yy = lv_yy1.
*      CONCATENATE 'SEP_' lv_yr_char '_GR' INTO fname."sep6.
      fname = 'SEP_GR'.
      CONCATENATE 'Sep' lv_yr_char '-GR' INTO dname."
    ELSEIF lv_yr_char1 = '10' AND lv_yy = lv_yy1.
*      CONCATENATE 'OCT_' lv_yr_char '_GR' INTO fname."oct7.
      fname = 'OCT_GR'.
      CONCATENATE 'Oct' lv_yr_char '-GR' INTO dname.".
    ELSEIF lv_yr_char1 = '11' AND lv_yy = lv_yy1.
*      CONCATENATE 'NOV_' lv_yr_char '_GR' INTO fname."nov8.
      fname = 'NOV_GR'.
      CONCATENATE 'Nov' lv_yr_char '-GR' INTO dname."
    ELSEIF lv_yr_char1 = '12' AND lv_yy = lv_yy1.
*      CONCATENATE 'DEC_' lv_yr_char '_GR' INTO fname."dec9.
      fname = 'DEC_GR'.
      CONCATENATE 'Dec' lv_yr_char '-GR' INTO dname."
    ELSEIF lv_yr_char1 = '1' AND  lv_yy = lv_yy1.
*      CONCATENATE 'JAN_' lv_yr_char '_GR' INTO fname."jan10.
      fname = 'JAN_GR'.
      CONCATENATE 'Jan' lv_yr_char '-GR' INTO dname."
    ELSEIF lv_yr_char1 = '2' AND lv_yy = lv_yy1.
*      CONCATENATE 'FEB_' lv_yr_char '_GR' INTO fname."feb11.
      fname = 'FEB_GR'.
      CONCATENATE 'Feb' lv_yr_char '-GR' INTO dname."
    ELSEIF lv_yr_char1 = '3' AND lv_yy = lv_yy1.
*      CONCATENATE 'MAR_' lv_yr_char '_GR' INTO fname."mar12.
      fname = 'MAR_GR'.
      CONCATENATE 'Mar' lv_yr_char '-GR' INTO dname."
    ENDIF.
* For each selected month in s_budat
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = fname."'APR_25_GR'.
    ls_fieldcat-seltext_m = dname."apr1 && '-GR'.
    ls_fieldcat-just       = 'R'.
*    ls_fieldcat-do_sum = 'X'.
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
  ls_fieldcat-fieldname = 'TOT_GR'.
  ls_fieldcat-seltext_m = 'Tot-GR '.
  ls_fieldcat-just       = 'R'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  CLEAR:lv_yr_char,  lv_yr_char1.
  lv_date = s_budat-low.

  DO lv_mm TIMES.
    IF lv_yr_char IS INITIAL.
      lv_yr_char = s_budat-low+2(2).
      lv_months = s_budat-low+4(2).
      lv_yy1 = lv_yy = s_budat-low+0(4).
      lv_yr_char1 = lv_months.
    ENDIF.

    "last-two-digits of start year
    IF lv_yr_char1     = '4' AND lv_yy = lv_yy1.
*      CONCATENATE 'APR_' lv_yr_char '_REJ' INTO fname1."apr1.
      fname1 = 'APR_REJ'.
      CONCATENATE 'Apr' lv_yr_char '-REJ' INTO dname1."apr1.
    ELSEIF lv_yr_char1 = '5' AND lv_yy = lv_yy1.
*      CONCATENATE 'MAY_' lv_yr_char '_REJ' INTO fname1.""may2.
       fname1 = 'MAY_REJ'.
      CONCATENATE 'May' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '6' AND lv_yy = lv_yy1.
*      CONCATENATE 'JUN_' lv_yr_char '_REJ' INTO fname1."jun3.
       fname1 = 'JUN_REJ'.
      CONCATENATE 'Jun' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '7' AND lv_yy = lv_yy1.
*      CONCATENATE 'JUL_' lv_yr_char '_REJ' INTO fname1."jul4.
       fname1 = 'JUL_REJ'.
      CONCATENATE 'Jul' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '8' AND lv_yy = lv_yy1.
*      CONCATENATE 'AUG_' lv_yr_char '_REJ' INTO fname1."aug5.
       fname1 = 'AUG_REJ'.
      CONCATENATE 'Aug' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '9' AND lv_yy = lv_yy1.
*      CONCATENATE 'SEP_' lv_yr_char '_REJ' INTO fname1."sep6.
       fname1 = 'SEP_REJ'.
      CONCATENATE 'Sep' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '10' AND lv_yy = lv_yy1.
*      CONCATENATE 'OCT_' lv_yr_char '_REJ' INTO fname1."oct7.
       fname1 = 'OCT_REJ'.
      CONCATENATE 'Oct' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '11' AND lv_yy = lv_yy1.
*      CONCATENATE 'NOV_' lv_yr_char '_REJ' INTO fname1."nov8.
       fname1 = 'NOV_REJ'.
      CONCATENATE 'Nov' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '12' AND lv_yy = lv_yy1.
*      CONCATENATE 'DEC_' lv_yr_char '_REJ' INTO fname1."dec9.
       fname1 = 'DEC_REJ'.
      CONCATENATE 'Dec' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '1' AND  lv_yy = lv_yy1.
*      CONCATENATE 'JAN_' lv_yr_char '_REJ' INTO fname1."jan10.
       fname1 = 'JAN_REJ'.
      CONCATENATE 'Jan' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '2' AND lv_yy = lv_yy1.
*      CONCATENATE 'FEB_' lv_yr_char '_REJ' INTO fname1."feb11.
       fname1 = 'FEB_REJ'.
      CONCATENATE 'Feb' lv_yr_char '-REJ' INTO dname1."
    ELSEIF lv_yr_char1 = '3' AND lv_yy = lv_yy1.
*      CONCATENATE 'MAR_' lv_yr_char '_REJ' INTO fname1."mar12.
       fname1 = 'MAR_REJ'.
      CONCATENATE 'Mar' lv_yr_char '-REJ' INTO dname1."
    ENDIF.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = fname1."'APR_25_REJ'.
    ls_fieldcat-seltext_m = dname1."apr1 && '-Rej'.
    ls_fieldcat-just       = 'R'.
*    ls_fieldcat-do_sum = 'X'.
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

*  APPEND ls_fieldcat TO lt_fieldcat.


  ls_fieldcat-fieldname = 'TOT_REJ'.
  ls_fieldcat-seltext_m = 'Tot-REJ'.
  ls_fieldcat-just       = 'R'.
  APPEND ls_fieldcat TO lt_fieldcat.

*  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'REJ_PERC'.
  ls_fieldcat-seltext_m = '%REJ'.
  ls_fieldcat-just       = 'R'.
  APPEND ls_fieldcat TO lt_fieldcat.


ENDFORM.
