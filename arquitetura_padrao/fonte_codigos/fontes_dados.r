# ações: baixa a base de dados para máquina local

# pré-requisitos: pacotes instalados
# install.packages('tidyverse')
# install.packages('abjutils')
#

baixa_historico_receitasSP_2010_2019 = function(){
	# baixa historico mas não junta os anos

	# criando diretorio para baixar os dados
	caminho = '../dados/dados_receitas'
	ifelse(!dir.exists(caminho), dir.create(caminho), FALSE)

	# entrando nele
	setwd(caminho)

	# sequencia de anos
	seq_anos = seq(2010, 2019)
	url_download="https://transparencia.tce.sp.gov.br/sites/default/files/conjunto-dados/receitas-ANO.zip"    

	for(ano in seq_anos){

	    print(stringr::str_c('Iniciando ANO = ', as.character(ano)))

	    # variáveis auxiliares
	    url_temp = stringr::str_replace(url_download, 'ANO', as.character(ano))
	    nomeArquivo_zip = stringr::str_c('ano',ano,'.zip')
	    nomeArquivo_csv = stringr::str_replace('receitas-ANO.csv', 'ANO', as.character(ano))

	    # baixa
	    print('baixando...')
	    download.file(url_temp, nomeArquivo_zip)
	    # dezipa
	    unzip(nomeArquivo_zip)
	    # remove .zip baixado
	    file.remove(nomeArquivo_zip)

	    # desncasa 3 segundos
	    print('descansando...')
	    Sys.sleep(3)
	}

	# volto para o diretorio inicial
	setwd('../../fonte_codigos')
}

baixa_historico_receitasSP_2020 = function(){
	# baixa historico mas não junta os anos
	library(magrittr)

	# criando diretorio para baixar os dados
	caminho = '../dados/dados_receitas'
	ifelse(!dir.exists(caminho), dir.create(caminho), FALSE)

	# entrando nele
	setwd(caminho)

	cidades = readr::read_csv('../cidades_sao_paulo.csv')

	df = cidades %>% 
	    dplyr::mutate(
	        nome_ok = stringr::str_replace_all(nome, ' ','-') %>% 
	                    stringr::str_replace_all("'",'-') %>% 
	                    abjutils::rm_accent() %>% 
	                    stringr::str_to_lower(),
	        url = 'https://transparencia.tce.sp.gov.br/sites/default/files/csv/receitas-cidade-2020.zip',
	        url = stringr::str_replace(url, 'cidade', nome_ok),
	        nome_csv = url %>% 
	                    stringr::str_replace('https://transparencia.tce.sp.gov.br/sites/default/files/csv/','') %>% 
	                    stringr::str_replace('.zip', '.csv')
	    )

	primeiro = TRUE

	for(linha in seq(1, nrow(df))){

	    nome_arq_zip = stringr::str_c(df[linha, 'nome_ok'], '.zip')
	    url_cidade = df[linha, 'url']
	    nome_arq_csv = df[linha, 'nome_csv']

	    nome_arq_zip = as.character(nome_arq_zip)
	    nome_arq_csv = as.character(nome_arq_csv)
	    url_cidade = as.character(url_cidade)


	    # excecoes de sites nao encontrados
	    if(url_cidade != 'https://transparencia.tce.sp.gov.br/sites/default/files/csv/receitas-florinea-2020.zip' &
	    	url_cidade != 'https://transparencia.tce.sp.gov.br/sites/default/files/csv/receitas-maraba-paulista-2020.zip' &
	    	url_cidade != 'https://transparencia.tce.sp.gov.br/sites/default/files/csv/receitas-mococa-2020.zip' &
	    	url_cidade != 'https://transparencia.tce.sp.gov.br/sites/default/files/csv/receitas-nantes-2020.zip' &
	    	url_cidade != 'https://transparencia.tce.sp.gov.br/sites/default/files/csv/receitas-nova-granada-2020.zip' &
	    	url_cidade != 'https://transparencia.tce.sp.gov.br/sites/default/files/csv/receitas-pauliceia-2020.zip' &
	    	url_cidade != 'https://transparencia.tce.sp.gov.br/sites/default/files/csv/receitas-sao-lourenco-da-serra-2020.zip' &
	    	url_cidade != 'https://transparencia.tce.sp.gov.br/sites/default/files/csv/receitas-sao-paulo-2020.zip'){
	    	    
	    	    # baixa
	    	    print('baixando...')
	    	    print(linha)
	    	    download.file(url_cidade, nome_arq_zip)
	    	    # dezipa
	    	    unzip(nome_arq_zip)
	    	    # remove .zip baixado
	    	    file.remove(nome_arq_zip)
	    
	    	    df_aux = readr::read_csv2(nome_arq_csv, locale = readr::locale(encoding = 'latin1'))
	    
	    	    if(primeiro){
	    	        df_aux %>% readr::write_csv('receita-2020.csv')
	    	        primeiro = FALSE
	    	    }else{
	    	        df_aux %>% readr::write_csv('receita-2020.csv', append = TRUE)
	    	    }
	    
	    	    file.remove(nome_arq_csv)
	    }
	    
	}

	# volto para o diretorio inicial
	setwd('../../fonte_codigos')
}


gera_csv_receitas_10a19_Marc_Abr_Maio = function(){
	# compila em um unico .csv
	# meses de março, abril e maio
	library(magrittr)

	# criando diretorio para baixar os dados
	caminho = '../dados/dados_receitas/filtrados_mes_Marc_Abr_Maio'
	ifelse(!dir.exists(caminho), dir.create(caminho), FALSE)

	# entrando nele
	setwd(caminho)


	lista_mes = c('março','abril','maio','Março','Abril','Maio')
	arquivos = dir()
	lista_anos = seq(2010, 2019)
	primeiro = TRUE

	for(i in seq(1,10)){
	    
	    print(arquivos[i])
	    print(lista_anos[i])
	    print('')
	    
	    df = readr::read_csv2_chunked(arquivos[i],
	                      callback = DataFrameCallback$new(
	                      function(entrada, pos){
	                            entrada  %>% 
	                            dplyr::filter(
	                                mes_ref_extenso %in% lista_mes
	                            )
	                          }
	                      ),
	                      chunk_size=0.5e6,
	                      locale = readr::locale(encoding = 'latin1'))

	    if(primeiro){
	        df %>% readr::write_csv('filtrado_Marc_Abr_Mai.csv')
	        primeiro = FALSE
	    }else{
	        df %>% readr::write_csv('filtrado_Marc_Abr_Mai.csv', append = TRUE)
	    }
	    
	    Sys.sleep(4)
	}

	# volto para o diretorio inicial
	setwd('../../../fonte_codigos')
}


df_cols_especif_valorSomado = function(){
	# pegas todos anos das receitas e soma o valor agrupando pelas colunas especificadas
	# return: dataframe consolidado
	library(magrittr)

	# indo para diretorio de dados
	caminho = '../dados/dados_receitas/filtrados_mes_Marc_Abr_Maio'
	setwd(caminho)

	# le dados sumarizando eles
	cols_select = c('ano_exercicio',
	               'ds_municipio',
	               'ds_orgao',
	               'mes_ref_extenso',
	               'vl_arrecadacao')

	# preparando csv
	df = readr::read_csv_chunked('filtrado_Marc_Abr_Mai.csv',
	                      callback = DataFrameCallback$new(
	                      function(entrada, pos){
	                            entrada  %>% 
	                            dplyr::select(cols_select) %>% 
	                            dplyr::group_by(
	                               ano_exercicio,
	                               ds_municipio,
	                               ds_orgao,
	                               mes_ref_extenso
	                            ) %>% 
	                            dplyr::summarise(soma_vl_arrec = sum(vl_arrecadacao, na.rm = TRUE)) %>% 
	                            dplyr::ungroup()
	                          }
	                      ),
	                      chunk_size=0.5e6)

	return(df) 
}

