*&---------------------------------------------------------------------*
*& Report ZGT_RELATORIO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgt_relatorio.
TYPE-POOLS: icon.
TYPES: BEGIN OF ty_ekpo,
         ebeln TYPE ekko-ebeln,
         ekorg TYPE ekko-ekorg,
         lifnr TYPE ekko-lifnr,
         matnr TYPE ekpo-matnr,

       END OF ty_ekpo.

DATA gt_ekpo TYPE TABLE OF ty_ekpo. "global table
*DATA gt_ekpo_aux LIKE TABLE OF gt_ekpo.
*DATA st_ekpo TYPE ty_ekpo. "estrutura/headerline


DATA: i_ekkos  TYPE TABLE OF ty_ekpo,
      ekko     TYPE ekko,
      cl_table TYPE REF TO cl_salv_table.
*      p_compra         TYPE ekko-ekorg,
*      p_cod_fornecedor TYPE ekko-lifnr,
*      p_material       TYPE ekpo-matnr.


SELECTION-SCREEN: BEGIN OF BLOCK data1 WITH FRAME.

  PARAMETERS: p_comp  LIKE ekko-ekorg OBLIGATORY,
              p_codf  LIKE ekko-lifnr,
              p_matnr LIKE ekpo-matnr.
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN PUSHBUTTON 69(4) but1 USER-COMMAND search.

SELECTION-SCREEN: END OF BLOCK data1.



INITIALIZATION.
  but1 = '@0V@'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'SEARCH'.
      PERFORM get_dados USING p_comp p_codf p_matnr.
      PERFORM display_alv.
  ENDCASE.

**********************************************************************
FORM get_dados USING p_comp TYPE ekko-ekorg
                     p_codf TYPE ekko-lifnr
                     p_matnr TYPE ekpo-matnr.

  IF p_codf <> ''.
    IF p_matnr <> ''.
      SELECT k~ebeln, k~ekorg, k~lifnr, p~matnr
        FROM ekko AS k
        INNER JOIN ekpo AS p
        ON k~ebeln = p~ebeln
        INTO  CORRESPONDING FIELDS
        OF  TABLE @i_ekkos
        WHERE ekorg = @p_comp AND
              matnr = @p_matnr AND
              lifnr = @p_codf.
    ELSE.
      SELECT k~ebeln, k~ekorg, k~lifnr, p~matnr
        FROM ekko AS k
        INNER JOIN ekpo AS p
        ON k~ebeln = p~ebeln
        INTO  CORRESPONDING FIELDS
        OF  TABLE @i_ekkos
        WHERE ekorg = @p_comp AND
              lifnr = @p_codf.
    ENDIF.
  ELSE.
    IF p_matnr <> ''.
      SELECT k~ebeln, k~ekorg, k~lifnr, p~matnr
        FROM ekko AS k
        INNER JOIN ekpo AS p
        ON k~ebeln = p~ebeln
        INTO  CORRESPONDING FIELDS
        OF  TABLE @i_ekkos
        WHERE ekorg = @p_comp AND
              matnr = @p_matnr.
    ELSE.
      SELECT k~ebeln, k~ekorg, k~lifnr, p~matnr
        FROM ekko AS k
        INNER JOIN  ekpo AS p
        ON k~ebeln = p~ebeln
        INTO  CORRESPONDING FIELDS
        OF  TABLE @i_ekkos
        WHERE ekorg = @p_comp.

*      SELECT p~ebeln, k~ekorg
*        FROM ekko AS k
*          ekpo AS p "FOR ALL ENTRIES
*        ON k~ebeln = p~ebeln
*        INTO  CORRESPONDING FIELDS
*        OF  TABLE @i_ekkos
*        WHERE ekorg = @p_comp.
    ENDIF.
  ENDIF.

*  LOOP AT ekkos INTO ekko.
*    WRITE ekko-ebeln.
*  ENDLOOP.
ENDFORM.

FORM display_alv. "abap list view
  CALL METHOD cl_salv_table=>factory
*    EXPORTING
*      list_display = abap_true
    IMPORTING
      r_salv_table = cl_table
    CHANGING
      t_table      = i_ekkos.

  PERFORM feed_functions. "items de filtro e etc standart

  CALL METHOD cl_table->display.


ENDFORM.


FORM feed_functions.

  DATA: lc_functions TYPE REF TO cl_salv_functions.

  lc_functions = cl_table->get_functions( ).
  lc_functions->set_all( abap_true ).

ENDFORM.



*************************************************************
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_save        = 'A'
*    CHANGING
*      t_outtab      = i_ekkos
*    EXCEPTIONS
*      program_error = 1
*      OTHERS        = 2.
