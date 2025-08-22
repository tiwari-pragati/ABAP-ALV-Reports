*&---------------------------------------------------------------------*
*& Report ZBARRYEMP_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbarryemp_report.

DATA : ls_emp TYPE zbarry_emp,
       lv_msg TYPE char20.

PARAMETERS : p_empid TYPE int4,
             p_flag  TYPE char1,
             p_fname TYPE char20,
             p_lname TYPE char20,
             p_phone TYPE char20.

CALL FUNCTION 'ZBARRYEMPLOYEE_FM'
  EXPORTING
    emp_id  = p_empid
    flag    = p_flag
    i_fname = p_fname
    i_lname = p_lname
    i_phone = p_phone
  IMPORTING
    fname   = ls_emp-fname
    lname   = ls_emp-lname
    phone   = ls_emp-phone
    msg     = lv_msg.

  ls_emp-emp_id = p_empid.

WRITE: 'The impored data is'.
WRITE:/3 ls_emp-emp_id, 20 ls_emp-fname,35 ls_emp-lname,70 ls_emp-phone.
WRITE: 'The message is',lv_msg.
