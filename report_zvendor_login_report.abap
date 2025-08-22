*&---------------------------------------------------------------------*
*& Report ZVENDOR_LOGIN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZVENDOR_LOGIN.


TYPE-POOLS : slis.

TABLES : zvendor_login, zpo_ack, zvendor_feedback, zasn_data, zinv_upload.

TYPES : BEGIN OF ts_zvendor_login,
        vendor_id TYPE zvendor_login-vendor_id,
        username  TYPE zvendor_login-username,
        password  TYPE zvendor_login-password,
      END OF ts_zvendor_login.

DATA : it_zvendor_login TYPE TABLE OF ts_zvendor_login INITIAL SIZE 0,
       wa_zvendor_login TYPE ts_zvendor_login.

TYPES :BEGIN OF ts_zpo_ack,
        po_number         TYPE zpo_ack-po_number,
        po_item           TYPE zpo_ack-po_item,
        ack_date          TYPE zpo_ack-ack_date,
        exp_delivery_date TYPE zpo_ack-exp_delivery_date,
        ack_flag          TYPE zpo_ack-ack_flag,
        comments          TYPE zpo_ack-comments,
        created_on        TYPE zpo_ack-created_on,
      END OF ts_zpo_ack.

DATA : it_zpo_ack TYPE TABLE OF ts_zpo_ack INITIAL SIZE 0,
       wa_zpo_ack TYPE ts_zpo_ack.

TYPES : BEGIN OF ts_zasn_data,
        asn_id            TYPE zasn_data-asn_id,
        delivery_date     TYPE zasn_data-delivery_date,
        transport_details TYPE zasn_data-transport_details,
        quantity          TYPE zasn_data-quantity,
        status            TYPE zasn_data-status,
      END OF ts_zasn_data.

DATA : it_zasn_data TYPE TABLE OF ts_zasn_data INITIAL SIZE 0,
       wa_zasn_data TYPE ts_zasn_data.

TYPES : BEGIN OF ts_zinv_upload,
        invoice_id  TYPE zinv_upload-invoice_id,
        file_name   TYPE zinv_upload-file_name,
        file_path   TYPE zinv_upload-file_path,
        upload_date TYPE zinv_upload-upload_date,
        amount      TYPE zinv_upload-amount,
      END OF ts_zinv_upload.

DATA : it_zinv_upload TYPE TABLE OF ts_zinv_upload INITIAL SIZE 0,
       wa_zinv_upload TYPE ts_zinv_upload.

TYPES : BEGIN OF ts_zvendor_feedback,
           feedback_id     TYPE zvendor_feedback-feedback_id,
           issue_type      TYPE zvendor_feedback-issue_type,
           feedback_text   TYPE zvendor_feedback-feedback_text,
           attachment_name TYPE zvendor_feedback-attachment_name,
          entry_date      TYPE zvendor_feedback-entry_date,
      END OF ts_zvendor_feedback.



DATA : it_zvendor_feedback TYPE TABLE OF ts_zvendor_feedback INITIAL SIZE 0,
       wa_zvendor_feedback TYPE ts_zvendor_feedback.


DATA : it_fieldcat TYPE slis_t_fieldcat_alv.
DATA : it_fieldcat1 TYPE slis_t_fieldcat_alv,
       wa_fieldcat TYPE slis_fieldcat_alv.


DATA: v_repid  TYPE sy-repid,
      v_layout TYPE slis_layout_alv.

SELECT-OPTIONS: p_vid   FOR zvendor_login-vendor_id OBLIGATORY,
                p_usrnme  FOR  zvendor_login-username OBLIGATORY NO INTERVALS,
                p_pass  FOR zvendor_login-password OBLIGATORY NO INTERVALS.

START-OF-SELECTION.

  SELECT vendor_id username password
    INTO TABLE it_zvendor_login
    FROM zvendor_login
    WHERE vendor_id IN p_vid.


IF it_zvendor_login IS INITIAL.
    MESSAGE 'Invalid login data!' TYPE 'E'.
    EXIT.
  ENDIF.


PERFORM build_fieldcat_zvendor_login.

v_repid = sy-repid.
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program      = v_repid
    i_callback_user_command = 'USER_COMMAND'
    is_layout               = v_layout
    it_fieldcat             = it_fieldcat
  TABLES
    t_outtab                = it_zvendor_login
  EXCEPTIONS
    program_error           = 1
    OTHERS                  = 2.


FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CLEAR it_zpo_ack.

  READ TABLE it_zvendor_login INTO wa_zvendor_login INDEX rs_selfield-tabindex.
  IF sy-subrc = 0.

    SELECT po_number po_item ack_date exp_delivery_date ack_flag comments created_on
      INTO TABLE it_zpo_ack
      FROM zpo_ack
      WHERE vendor_id = zvendor_login-vendor_id.

    IF it_zpo_ack IS INITIAL.
      MESSAGE 'No item data found for selected order' TYPE 'I'.
      EXIT.
    ENDIF.

REFRESH :it_fieldcat.
    PERFORM build_fieldcat_zpo_ack.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = v_repid
        is_layout          = v_layout
        it_fieldcat        = it_fieldcat
      TABLES
        t_outtab           = it_zpo_ack.

  ENDIF.

ENDFORM.


FORM build_fieldcat_zvendor_login.

  CLEAR: it_fieldcat, wa_fieldcat.

  wa_fieldcat-fieldname = 'VENDOR_ID'.
  wa_fieldcat-seltext_m = 'Vendor ID'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'USERNAME'.
  wa_fieldcat-seltext_m = 'Username'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'PASSWORD'.
  wa_fieldcat-seltext_m = 'Password'.
  APPEND wa_fieldcat TO it_fieldcat.

ENDFORM.

FORM build_fieldcat_zpo_ack.

  CLEAR: it_fieldcat, wa_fieldcat.

  wa_fieldcat-fieldname = 'PO_NUMBER'.
  wa_fieldcat-seltext_m = 'PO Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'PO_ITEM'.
  wa_fieldcat-seltext_m = 'PO Item'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'ACK_DATE'.
  wa_fieldcat-seltext_m = 'Ack Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'EXP_DELIVERY_DATE'.
  wa_fieldcat-seltext_m = 'Expected Delivery Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'ACK_FLAG'.
  wa_fieldcat-seltext_m = 'Acknowledged'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'COMMENTS'.
  wa_fieldcat-seltext_m = 'Comments'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname = 'CREATED_ON'.
  wa_fieldcat-seltext_m = 'Created On'.
  APPEND wa_fieldcat TO it_fieldcat.

ENDFORM.
