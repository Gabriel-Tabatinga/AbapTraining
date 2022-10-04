*&---------------------------------------------------------------------*
*& Report zgt_insert
*&---------------------------------------------------------------------*
*&Tabelas Personalizadas dentro da database, código só funciona no ambiente de testes específico
*&---------------------------------------------------------------------*
REPORT zgt_insert.
TYPE-POOLS: icon.
TABLES: zgt_tabela.

SELECTION-SCREEN BEGIN OF BLOCK bloco01 WITH FRAME.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS: p_matri  LIKE zgt_tabela-matricula OBLIGATORY.

  SELECTION-SCREEN PUSHBUTTON 40(4) but3 USER-COMMAND delete.
  SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN END OF BLOCK bloco01.
SELECTION-SCREEN BEGIN OF BLOCK bloco02 WITH FRAME.

  PARAMETERS: p_nome   LIKE zgt_tabela-nome,
              p_cpf    LIKE zgt_tabela-cpf,
              p_sexo   LIKE zgt_tabela-sexo,
              p_dtnasc LIKE zgt_tabela-data_nascimento.
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN PUSHBUTTON 33(4) but1 USER-COMMAND insert.
  SELECTION-SCREEN PUSHBUTTON 39(4) but2 USER-COMMAND update.

SELECTION-SCREEN END OF BLOCK bloco02.

* TABELA INTERNA
DATA: BEGIN OF ti_ztg_cadfunc OCCURS 0,
        matricula      LIKE zgt_tabela-matricula,
        nome           LIKE zgt_tabela-nome,
        cpf            LIKE zgt_tabela-cpf,
        sexo           LIKE zgt_tabela-sexo,
        datanascimento LIKE zgt_tabela-data_nascimento,
      END OF ti_ztg_cadfunc.

INITIALIZATION.
  but1 = '@0V@'.
  but2 = '@0Z@'.
  but3 = '@11@'.


AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'DELETE'.
      SELECT SINGLE nome FROM zgt_tabela INTO @DATA(lv_nome) WHERE matricula = @p_matri.
      IF sy-subrc = 0.
        DELETE FROM zgt_tabela WHERE matricula = p_matri.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          MESSAGE | Usuario { lv_nome } excluido com sucesso | TYPE 'S'.
        ELSE.
          ROLLBACK WORK.
          MESSAGE | Erro ao excluir usuário | TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE | Erro ao excluir usuário | TYPE 'E'.
      ENDIF.
    WHEN 'INSERT'.
      ti_ztg_cadfunc-matricula = p_matri.
      ti_ztg_cadfunc-nome = p_nome.
      ti_ztg_cadfunc-cpf = p_cpf.
      ti_ztg_cadfunc-sexo = p_sexo.
      ti_ztg_cadfunc-datanascimento = p_dtnasc.

      APPEND ti_ztg_cadfunc.

      MOVE: ti_ztg_cadfunc-matricula TO zgt_tabela-matricula,
      ti_ztg_cadfunc-nome TO zgt_tabela-nome,
      ti_ztg_cadfunc-cpf TO zgt_tabela-cpf,
      ti_ztg_cadfunc-sexo TO zgt_tabela-sexo,
      ti_ztg_cadfunc-datanascimento TO zgt_tabela-data_nascimento.

      INSERT zgt_tabela.

      IF  sy-subrc IS INITIAL.
        MESSAGE | Usuario { p_nome } adicionado com sucesso | TYPE 'S'.
        COMMIT WORK.
      ELSE.
        MESSAGE | Erro ao inserir usuário | TYPE 'E'.
        ROLLBACK WORK.
      ENDIF.
*  BREAK-POINT.
    WHEN 'UPDATE'.
      SELECT SINGLE matricula FROM zgt_tabela INTO @DATA(lv_matricula) WHERE matricula = @p_matri.

      IF sy-subrc IS INITIAL.
        ti_ztg_cadfunc-matricula = p_matri.
        ti_ztg_cadfunc-nome = p_nome.
        ti_ztg_cadfunc-cpf = p_cpf.
        ti_ztg_cadfunc-sexo = p_sexo.
        ti_ztg_cadfunc-datanascimento = p_dtnasc.
        APPEND ti_ztg_cadfunc.

        MOVE: ti_ztg_cadfunc-matricula TO zgt_tabela-matricula,
        ti_ztg_cadfunc-nome TO zgt_tabela-nome,
        ti_ztg_cadfunc-cpf TO zgt_tabela-cpf,
        ti_ztg_cadfunc-sexo TO zgt_tabela-sexo,
        ti_ztg_cadfunc-datanascimento TO zgt_tabela-data_nascimento.

        SELECT SINGLE nome FROM zgt_tabela INTO @DATA(tmp_nome) WHERE matricula = @p_matri.
        SELECT SINGLE cpf FROM zgt_tabela INTO @DATA(tmp_cpf) WHERE matricula = @p_matri.
        SELECT SINGLE sexo FROM zgt_tabela INTO @DATA(tmp_sexo) WHERE matricula = @p_matri.
        SELECT SINGLE data_nascimento FROM zgt_tabela INTO @DATA(tmp_datana) WHERE matricula = @p_matri.


        IF p_nome <> ''.
          ti_ztg_cadfunc-nome = p_nome.
        ENDIF.
        IF p_cpf <> ''.
          ti_ztg_cadfunc-nome = p_nome.
        ENDIF.
        IF p_sexo <> ''.
          ti_ztg_cadfunc-nome = p_nome.
        ENDIF.
        IF p_dtnasc <> ''.
          ti_ztg_cadfunc-nome = p_nome.
        ENDIF.

        APPEND ti_ztg_cadfunc.

        IF p_nome <> ''.
          MOVE ti_ztg_cadfunc-nome TO zgt_tabela-nome.
        ELSE.
          MOVE tmp_nome TO zgt_tabela-nome.
        ENDIF.
        IF p_cpf <> ''.
          MOVE ti_ztg_cadfunc-cpf TO zgt_tabela-cpf.
        ELSE.
          MOVE tmp_cpf TO zgt_tabela-cpf.
        ENDIF.
        IF p_sexo <> ''.
          MOVE ti_ztg_cadfunc-sexo TO zgt_tabela-sexo.
        ELSE.
          MOVE tmp_sexo TO zgt_tabela-sexo.
        ENDIF.
        IF p_dtnasc <> '00000000'.
          MOVE ti_ztg_cadfunc-datanascimento TO zgt_tabela-data_nascimento.
        ELSE.
          MOVE tmp_datana TO zgt_tabela-data_nascimento.
        ENDIF.

        DELETE FROM zgt_tabela WHERE matricula = p_matri.
        INSERT zgt_tabela.

        IF sy-subrc IS INITIAL.
          MESSAGE | Usuario { p_nome } atualizado com sucesso | TYPE 'S'.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
          MESSAGE | Erro ao atualizar usuário | TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE | Usuário não encontrado | TYPE 'E'.
      ENDIF.
  ENDCASE.
