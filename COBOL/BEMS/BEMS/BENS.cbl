      *TO-DO - Arrumar Tela Inclusao
      *Arrumar espa�o "Continua S/N" as vezes n�o limpa o texto anterior
      *Implementar Consulta por Descricao 
       
       Identification Division.
       Program-Id. BEMS.

       Environment Division.
       special-names.   decimal-point is comma.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT BEMS ASSIGN TO DISK
              ORGANIZATION INDEXED
              ACCESS MODE DYNAMIC
              RECORD KEY CODIGO-BEM 
              ALTERNATE RECORD KEY DESCRICAO-BEM WITH DUPLICATES
              FILE STATUS ARQST.
              
              
       data division.
       file section.
       fd  BEMS LABEL RECORD STANDARD
       value of file-id is "PATRIMO.DAT  ".
       01 reg-BEMS.
           02 Chaves.
               03 CODIGO-BEM     pic 9(06). 
           02 DESCRICAO-BEM      pic x(30).
           02 CODIGO-TIPO        pic 9(05).
           02 DT-AQUISICAO       PIC 99999999.
           02 VALOR-COMPRA       PIC 9(06)V99.
           02 NOTA-FISCAL        PIC 9(15). 
       01 codibem  pic x(04).
       
       WORKING-STORAGE SECTION.
       
       01 reg-BEMS-E.
           02 Chaves-E.
               03 CODIGO-BEM-E     pic 9(06). 
           02 DESCRICAO-BEM-E      pic x(30).
           02 CODIGO-TIPO-E        pic 9(05).
           02 DT-AQUISICAO-E       PIC 99/99/9999.
           02 VALOR-COMPRA-E       PIC 9(06)V99.
           02 NOTA-FISCAL-E        PIC 9(15).
           
      
         
       01 data-sis.
           02 ano   pic 9999.
           02 mes   pic 99.
           02 dia   pic 99.
       01 desmes.
          02 filler pic x(10) value "Janeiro".
          02 filler pic x(10) value "Fevereiro".
          02 filler pic x(10) value "Marco".
          02 filler pic x(10) value "Abril".
          02 filler pic x(10) value "Maio".
          02 filler pic x(10) value "Junho".
          02 filler pic x(10) value "Julho".
          02 filler pic x(10) value "Agosto".
          02 filler pic x(10) value "Setembro".
          02 filler pic x(10) value "Outubro".
          02 filler pic x(10) value "Novembro".
          02 filler pic x(10) value "Dezembro".
       01 tabela-meses redefines desmes.
           02 mes-t pic x(10) occurs 12 times.
       01 arqst        pic x(2).
       01 op           pic x(1) value spaces.
       01 salva        pic x(1) value spaces.
       01 wigual       pic 9 value zeros.
       01 espaco       pic x(10) value spaces.
       01 op-continua  pic x(1)  value spaces.
     
       
       01 TIPO.
           02 filler pic x(18) value "MOBILIARIO".
           02 filler pic x(18) value "ELETRO-ELETRONICO".
           02 filler pic x(18) value "INFORMATICA".
           02 filler pic x(18) value "TELEFONIA".
       01 TAB-TIPOS redefines TIPO.
           02 TIPO-T pic x(18) occurs 4 times.

       screen section.
       01 tela-inicial.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 25 value "Cadastro dos Bens Patrimoniais".
          02 line 7 col 25 value "       Menu Principal" highlight.
          02 line 8 col 3 VALUE "1 - Inclusao do Bems".
          02 line 9 col 3 VALUE "2 - Alteracao de Bem".
          02 line 8 col 40 VALUE "3 - Exclusao de Bem".
          02 line 9 col 40 VALUE "4 - Consulta por codigo".
          02 line 10 col 3 VALUE "5 - Consulta por descricao".
          02 line 13 col 20 value "6 - Sair ".
          02 Line 18 col 5 value "Digite Sua Escolha".
       01 Tela-inclusao.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "INCLUSAO DE BEMS" highlight.
          02 line 9 col 3 VALUE "Codigo do Bem".
          02 line 11 col 3 VALUE "Descricao do Bem".
          02 line 9 col 45 VALUE "Codigo do Tipo".
          02 line 11 col 45 VALUE "Data da Aquisicao".
          02 line 13 col 3 VALUE "Valor da Compra".
          02 line 13 col 45 VALUE "O N� da Nota Fiscal".
       01 Tela-alteracao.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "ALTERACAO DE BEMS" highlight.
          02 line 9 col 3 VALUE "Codigo do Bem".
          02 line 11 col 3 VALUE "Descricao do Bem".
          02 line 9 col 45 VALUE "Codigo do Tipo".
          02 line 11 col 45 VALUE "Data da Aquisicao".
          02 line 13 col 3 VALUE "Valor da Compra".
          02 line 13 col 45 VALUE "O N� da Nota Fiscal".
       01 Tela-esclusao.
          02 line 2 col 0 value "Santos,    de            de     .".
          02 line 3 col 27 value "Nome da Empresa".
          02 line 4 col 25 value "Controle de Patrimonio".
          02 line 5 col 20 value "EXCLUSAO DE BEMS" highlight.
          02 line 9 col 3 VALUE "Codigo do BEM".
       01 Tela-consulta.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "CONSULTA DE BEMS" highlight.
          02 line 9 col 3 VALUE "Codigo do Bem".
          02 line 11 col 3 VALUE "Descricao do Bem".
          02 line 9 col 45 VALUE "Codigo do Tipo".
          02 line 11 col 45 VALUE "Data da Aquisicao".
          02 line 13 col 3 VALUE "Valor da Compra".
          02 line 13 col 45 VALUE "O N� da Nota Fiscal".
       01 Tela-consulta-D.
          02 line 2 col 2 value "Santos,    de            de     .".
          02 line 4 col 30 value "Nome da Empresa".
          02 line 6 col 29 value "Controle de Patrimonio".
          02 line 7 col 27 value "CONSULTA DE BEMS" highlight.
          02 line 9 col 3 VALUE "Codigo do Bem".
          02 line 11 col 3 VALUE "Descricao do Bem".
          02 line 9 col 45 VALUE "Codigo do Tipo".
          02 line 11 col 45 VALUE "Data da Aquisicao".
          02 line 13 col 3 VALUE "Valor da Compra".
          02 line 13 col 45 VALUE "O N� da Nota Fiscal".
       01 MENSAGENS.
           02 line 21 col 10 value "ERRO: Valor Invalido".
           02 line 21 col 10 value "ERRO: Depto nao encontrado".
           
           
       procedure division.
      *-----------------------------------------------------------------
       Inicio.
           Perform abre-arq.
           Perform abertura until op = "5".
           exit program.
      *-----------------------------------------------------------------    
       sai.
         exit program.
      *-----------------------------------------------------------------   
       abre-arq.
       
                  OPEN I-O BEMS.
           IF ARQST NOT = "00"
               DISPLAY "ERRO DE ABERTURA"
               STOP " "
               CLOSE BEMS
               OPEN OUTPUT BEMS
               CLOSE BEMS
               OPEN I-O BEMS.
       
      *-----------------------------------------------------------------   
       abertura.
           display erase at 0101.
           display tela-inicial at 0101.
           display erase at 0101.
           Perform mostra-data.
           accept op at 1845.
           perform trata-opcao.  
         
      *-----------------------------------------------------------------   
       trata-opcao.
           move spaces to op-continua
           evaluate op
           when "1"
               perform inclusao until op-continua = "n" or "N"
           when "2"
               perform alteracao until op-continua = "n" or "N"
           when "3"
               perform exclusao until op-continua = "n" or "N"
           when "4"
               perform consulta until op-continua = "n" or "N"
           when "5"
               perform consulta-d until op-continua = "n" or "N"
           when "6"
               perform sai.  
      *-----------------------------------------------------------------   
       mostra-data.
           move function current-date to data-sis.
           display dia at 0213.
           display mes-t(mes) at 0219.
           display ano at 0233.
      *-----------------------------------------------------------------     
       inclusao.
           perform tela-inclu.
           move zeros to wigual.
           perform inicializar.
           perform testa-codbem until wigual = 1.
           perform recebe-dados.
           perform grava.
           perform continua.
      *-----------------------------------------------------------------     
       tela-inclu.
	      display erase at 0101.
          display erase at 0101.
	      display Tela-inclusao at 0101.
          
          perform mostra-data.    
      *-----------------------------------------------------------------   
       inicializar.
	       move spaces to op op-continua salva.
           move spaces to  DESCRICAO-BEM-E.
           move zeros to   CODIGO-BEM-E CODIGO-TIPO-E. 
           move zeros to DT-AQUISICAO-E VALOR-COMPRA-E NOTA-FISCAL-E.                   
           move zeros to wigual.
           display espaco at 2315.
      *----------------------------------------------------------------- 
       recebe-dados.
           perform testa-descbem     until DESCRICAO-BEM-E not = spaces.
           perform testa-tipo        until CODIGO-TIPO-E   not = zeros.
           perform testa-data  until DT-AQUISICAO-E  not = "00/00/0000"
           perform testa-valor       until VALOR-COMPRA-E  not = zeros.
           perform testa-nota        until NOTA-FISCAL-E   not = zeros.
      *-----------------------------------------------------------------
       testa-codbem.
           move 1 to wigual
           move zeros to CODIGO-BEM-E
           accept CODIGO-BEM-E at 0926 with prompt auto
           if CODIGO-BEM-E = spaces or "0000" then
                display espaco at 2321
                display "Digite um codigo diferente de zero." at 2321
                set wigual to 0
           else
                move CODIGO-BEM-E to CODIGO-BEM
                read BEMS not invalid key perform ja-cadastrado.
       
       ja-cadastrado.
           display espaco at 2321
           display "Codigo ja� cadastrado" at 2321
           set wigual to 0
           perform testa-codbem.
           
           
       testa-descbem.
           accept DESCRICAO-BEM-E at 1126
           if DESCRICAO-BEM-E = spaces then
                display "Digite a descricao do bem." at 2321
                else
                display espaco at 2321.
           
       testa-tipo.
           accept CODIGO-TIPO-E at 0966
           if CODIGO-TIPO-E = zeros then
                display "Digite o codigo do tipo do produto." at 2321
	            else
                display espaco at 2321.
                
       testa-data.
           accept DT-AQUISICAO-E at 1166
           if DT-AQUISICAO-E = zeros then
                display "Digite a data da aquisicao." at 2321
	            else
                display espaco at 2321.
       
       testa-valor.
           accept VALOR-COMPRA-E at 1336
           if VALOR-COMPRA-E = zeros then
                display "Digite o valor." at 2321
	            else
                display espaco at 2321.
                
       testa-nota.
           accept NOTA-FISCAL-E at 1366
           if NOTA-FISCAL-E = zeros then
                display "Digite a Nota fiscal." at 2110
	            else
                display espaco at 1336.         
                
                
       grava.
           display "Salvar <S/N> [ ]" at 2321.
           accept salva at 2335 with prompt auto.
           if salva = "S" or "s" then
                move reg-BEMS-E to reg-BEMS
                write reg-BEMS invalid key perform estuda-erro
                display arqst at 2221.         
                
                
       continua.
           display espaco at 2321.
           display "Continua <S/N> [ ]" at 2321.
           accept op-continua at 2337 with prompt auto.
           if op-continua = "S" or "s" then
                perform inicializar
                display espaco at 2321.         
                
       exclusao.
           perform inicializar.
           display erase at 0101.
           display Tela-esclusao at 0101.
           perform inicializar.
           perform le-dados.
           if arqst = "00" then
               display "Deseja excluir o registro<S/N> [ ]" at 2319
               accept salva at 2351 with prompt auto
           else
               perform inicializar
               display espaco at 2319
               display "Registro nao encontrado." at 2321.
           if salva = "S" or "s" then
               display espaco at 2319
               Display "Registro apagado." at 2321
               delete BEMS.
           stop " ".
           display espaco at 2315.
           perform continua.         
                
       estuda-erro.
           display "Codigo nao encontrado." at 2321.
	   stop " ".         
                
       consulta.
           display erase at 0101.
           display Tela-consulta at 0101.
           display "Consulta de Bems" at 0730 with highlight.
           perform le-dados.
           perform continua.  
           
           
       consulta-d.
           display erase at 0101.
           display Tela-consulta-d at 0101.
           display "Consulta de Bems" at 0730 with highlight.
           perform le-dados.
           perform continua.     
               
       le-dados.
           perform inicializar.
           perform mostra-data.
           accept CODIGO-BEM-E at 0832.
           move CODIGO-BEM-E to CODIGO-BEM.
           read bems key is CODIGO-BEM invalid key
                display "Registro nao encontrado" at 2320
                move 1 to wigual
                stop " ".
           if arqst = "00" then
              display espaco at 0832
              perform mostra-tela.        
               
               
       mostra-tela.
       perform inicializar.
           move reg-BEMS to reg-BEMS-E.
           display CODIGO-BEM-E at 0926.
           display DESCRICAO-BEM-E at 1126.
           display CODIGO-TIPO-E at 0966.
           display DT-AQUISICAO-E at 1166.        
           display VALOR-COMPRA-E  at 1336.
           display NOTA-FISCAL-E at 1366.
               
               
       altera-dados.
       perform inicializar.
           accept CODIGO-BEM-E at 0832.
           accept DESCRICAO-BEM-E at 1032.
           accept CODIGO-TIPO-E at 1132.
           accept DT-AQUISICAO-E at 1232.        
           accept VALOR-COMPRA-E  at 1332.
           accept NOTA-FISCAL-E at 1432.     
               
               
       alteracao.
           perform inicializar.
           display erase at 0101.
           display Tela-alteracao.
           perform le-dados.
           if wigual <> 1
              perform altera-dados
              perform recebe-dados
              display "Deseja salvar alteracao <S/N> [ ]" at 2319
              accept salva at 2350 with prompt auto
              if salva = "S" or "s" then
                 move reg-BEMS-E to reg-BEMS
                 rewrite reg-BEMS invalid key perform estuda-erro
                 display espaco at 2315.
           perform continua.
       end program BEMS.        
       
