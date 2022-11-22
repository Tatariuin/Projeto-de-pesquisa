
library(stringr)
library(microbenchmark)
library(dplyr)
#library(dplyr)
tidyverse_conflicts()

#Lê o arquivo CSV 
logCSV <- read.csv(file.choose())

#Cria um dataFrame como as notas de todos os alunos
notasLog = read.csv(file.choose())


#elimina duplicação dos nomes
nomesLog = unique(logCSV$nome);
#É importante ter o nome de cada estudante para que possamos contar a quantidade de interações de cada um
#nomesLog

#Cria um vetor que terá apenas os estudantes, removendo os professores, coordenadores, assitentes, etc...
alunosLog = vector();


#dadosMineracao = read.csv(file.choose())
#indice do vetor dos alunos
j= 1;


#laço de repetiçõa que irá passar pelo vetor que tem todas as pessoas e só incluirá o nome da pessoa no novo vetor (alunosLog), 
#caso a pessoa seja um estudnate
for (i in 1:length(nomesLog)){
  if ( (str_detect(nomesLog[i], "ADM") != TRUE) && (str_detect(nomesLog[i], "CAP") != TRUE) && 
       (str_detect(nomesLog[i], "PMD") != TRUE) && (str_detect(nomesLog[i], "PMP") != TRUE) &&
       (str_detect(nomesLog[i], "CPM") != TRUE) && (str_detect(nomesLog[i], "CAT") != TRUE) &&
       (str_detect(nomesLog[i], "PF") != TRUE) && (str_detect(nomesLog[i], "Webdesigner") != TRUE) &&
       (str_detect(nomesLog[i], "SISTEC") != TRUE) && (str_detect(nomesLog[i], "APed") != TRUE) &&
       (str_detect(nomesLog[i], "CC") != TRUE) && (str_detect(nomesLog[i], "AGSI") != TRUE) &&
       (str_detect(nomesLog[i], "APMD") != TRUE) && (str_detect(nomesLog[i], "PADM") != TRUE) &&
       (str_detect(nomesLog[i], "ACP") != TRUE) && (str_detect(nomesLog[i], "e-Tec") != TRUE) &&
       (str_detect(nomesLog[i], "AME") != TRUE) && (str_detect(nomesLog[i], "Curso de Extensão") != TRUE) &&
       (str_detect(nomesLog[i], "DDA") != TRUE) && (str_detect(nomesLog[i], "Adm") != TRUE) &&
       (str_detect(nomesLog[i], "Auditor") != TRUE) && (str_detect(nomesLog[i], "Pronatec") != TRUE) &&
       (str_detect(nomesLog[i], "AFP") != TRUE) && (str_detect(nomesLog[i], "Tutor") != TRUE)&&
       (str_detect(nomesLog[i],"AGI") != TRUE)&&(str_detect(nomesLog[i],"Moodle") != TRUE)&&
       (str_detect(nomesLog[i],"SUPORTE") != TRUE)&&(str_detect(nomesLog[i],"DG") != TRUE)&&
       (str_detect(nomesLog[i], "SUAP") != TRUE)&&(nchar(nomesLog[i]) != 1)&&
       (str_detect(nomesLog[i], "PWEB") != TRUE)&&(str_detect(nomesLog[i], "Analista de TI") != TRUE)&&
       (str_detect(nomesLog[i], "VPV") != TRUE)&&(str_detect(nomesLog[i], "Leobino N") != TRUE)&&
       (str_detect(nomesLog[i], "Jordana de Almeida Canedo") != TRUE)&&(str_detect(nomesLog[i], "Maira Sena Silva") != TRUE)&&
       (str_detect(nomesLog[i], "CDP") != TRUE) && (str_detect(nomesLog[i], "Aluno") != TRUE)&&
       (str_detect(nomesLog[i], "Usuário") != TRUE) && (str_detect(nomesLog[i], "Desistente") != TRUE)&&
       (str_detect(nomesLog[i], "Coord") != TRUE) && (str_detect(nomesLog[i], "Milena Reis Fernandes") != TRUE)&&
       (str_detect(nomesLog[i], " MD-") != TRUE) && (str_detect(nomesLog[i], "PAV") != TRUE)&&
       (str_detect(nomesLog[i], "Debora Santos Maciel") != TRUE)&&(str_detect(nomesLog[i], "Manoel Amaral de Souza Neto") != TRUE)&&
       (str_detect(nomesLog[i], "sINET") != TRUE)&& (nomesLog[i] != "Gustavo Vieira da Silva - UBA-SMSI-T4")&&
       (nomesLog[i] != "Leandro de Jesus Santos - UBA-SMSI-T8")&& (nomesLog[i] != "Micaelle Cardoso da Silva - UBA-SMSI-T8")&&
       (nomesLog[i] != "Rafaline dos Santos Silva - UBA-SMSI-T8")&& (str_detect(nomesLog[i], "Avaliador") != TRUE)&&
       (str_detect(nomesLog[i], "AAV") != TRUE)&& (str_detect(nomesLog[i], "SINF") != TRUE)&& 
       (str_detect(nomesLog[i], "Isaías Abdon Melo Rêgo") != TRUE)&& (str_detect(nomesLog[i], "CAJ") != TRUE)&&
       (str_detect(nomesLog[i],"SINET") != TRUE)&& (str_detect(nomesLog[i],"Marcos Vinicius Bezerra Pereira da Silva") != TRUE)&&
       (str_detect(nomesLog[i],"luan araujo") != TRUE)&& (nomesLog[i] != "Gabriel Carneiro da Silva Amaral - LFT-CRC-T4") && 
       ((nomesLog[i] != "Lismara Ribeiro Macedo")) && (str_detect(nomesLog[i],"Pró-Reitora") != TRUE) && 
       ((nomesLog[i] != "Maria Helena Silva Vieira Santos - LFT-CINF-T2"))  && (str_detect(nomesLog[i],"SMSI") != TRUE)
  ) {
    
    
    alunosLog[j] <- nomesLog[i]; 
    j = j+1;
  }
}
alunosLog






# Cria um data frame vazio que terá os dados dos estudantes.
dadosMineracao <- data.frame(nome = character(),
                             quantAcessoForum = numeric(),
                             quantAcessoSlides = numeric(),
                             quantAcessoChat = numeric(),
                             quantAcessoVideos = numeric(),
                             quantiAcessoDiscussaoForum = numeric(),
                             quantAcessoSemestre = numeric(),
                             quantArquivosEnviados = numeric(),
                             quantAcessosAtividades = numeric(),
                             mediaSemestre = numeric(),
                             quantAcessoArquivos = numeric(),
                             Status = logical());
#quantAcesso_1_Semestre guarda a soma dos acessos nas disciplinas do primeiro semestre

#AdicionaLinha é um dataFrame que adicionará linhas em dados mineração
#Obs. AdicionaLinha é um dataFrame com uma unica linha
AdicionaLinha = data.frame(nome = NaN,
                           quantAcessoForum = NaN,
                           quantAcessoSlides = NaN,
                           quantAcessoChat = NaN,
                           quantAcessoVideos = NaN,
                           quantiAcessoDiscussaoForum = NaN,
                           quantAcessoSemestre = NaN,
                           quantArquivosEnviados = NaN,
                           quantAcessosAtividades = NaN,
                           mediaSemestre = NaN,
                           quantAcessoArquivos = NaN,
                           Status = NaN);

#Este laço de repetição vai adicionar lihas ao dataFrame dadosMineração de acordo com a quantidade de alunos.
for(i in 1:length(alunosLog)){
  
  dadosMineracao = rbind(dadosMineracao,AdicionaLinha)
  
}


# exemplo de como filtrar uma informação
ZZZ = dplyr::filter(logCSV, nome == alunosLog[1] & componente == "Sistema" ); 

dplyr::filter(logCSV,nome == alunosLog[66] & nome_evento == "Módulo do curso visualizado" & componente == "Fórum")

logCSV %>% dplyr::filter(nome == alunosLog[1] & nome_evento == "Módulo do curso visualizado" & componente == "Fórum" & evento == "Fórum: Fórum de dúvidas") %>% nrow;

alunosLog




media1semestre
quantAcesso_1_Semestre
for (i in 1:length(alunosLog)){
  
  #Está condição tem como finalidade reconhcer e tratar os alunos que não são do curso de redes de computadores, preenchendo todos os seus dados com NaN(a pedido seu)
  if((str_detect(alunosLog[i], "SMSI") == TRUE) || (str_detect(alunosLog[i], "SINET") == TRUE) || (str_detect(alunosLog[i], "UBA-CRC-T3") == TRUE)
      ){
    
    dadosMineracao[i,"nome"] = alunosLog[i];
    dadosMineracao[i,"quantAcessoForum"] = NA;
    dadosMineracao[i,"quantAcessoSlides"] = NA;
    dadosMineracao[i,"quantAcessoChat"] = NA;
    dadosMineracao[i,"quantAcessoVideos"] = NA;
    dadosMineracao[i,"quantiAcessoDiscussaoForum"] = NA;
    dadosMineracao[i,"quantAcessoSemestre"] = NA
    dadosMineracao[i,"quantArquivosEnviados"] = NA;
    dadosMineracao[i,"quantAcessosAtividades"] = NA;
    dadosMineracao[i,"mediaSemestre"] = NA;
    dadosMineracao[i,"quantAcessoArquivos"] = NA;
    dadosMineracao[i,"Status"] = NA;
    
  }
  
  else{
    
    
    #Contabiliza a quantidade de vezes que o aluno visualizou o Fórum de Dúvidas 
    countAcessoForum = logCSV %>% dplyr::filter(nome == alunosLog[i] & nome_evento == "Módulo do curso visualizado" & componente == "Fórum" & evento == "Fórum: Fórum de dúvidas") %>% nrow;
  
    #Contabiliza a quantidade de vezes que o aluno acessou os slides 
    countAcessoSlide = logCSV %>% dplyr::filter(nome == alunosLog[i] & ((nome_evento == "Módulo do curso visualizado" & componente == "Arquivo" & str_detect(evento,"Slides")) | ( nome_evento == "Módulo do curso visualizado" & componente == "Página" & evento == "Página: Slides"))) %>% nrow;
    
    #Contabiliza a quantidade de vezes que o aluno acessou o chat
    countAcessoChat  = logCSV %>% dplyr::filter(nome == alunosLog[i] & nome_evento == "Módulo do curso visualizado" & componente == "Chat") %>% nrow;
    
    #Contabiliza a quantidade de vezes que o aluno acessou a disciplina fundamentos de lógica computacional
    countAcessoSemestre = logCSV %>% dplyr::filter(nome == alunosLog[i] & nome_evento == "Curso visto" & componente == "Sistema" & str_detect(evento, "Curso:")) %>% nrow;
    
    #Contabiliza a quantidade de vezes que o aluno acessou aos videos disponibilizados
    countAcessoVideos = logCSV %>% dplyr::filter(nome == alunosLog[i] & ((nome_evento == "Módulo do curso visualizado" & componente == "URL" & str_detect(evento,"URL: Videoaula")) | (nome_evento == "Módulo do curso visualizado" & componente == "Página" & evento == "Página: Vídeos"))) %>% nrow;
    
    #Contabiliza a quantidade de arquivos enviados pelo aluno
    countArquivosEnviados = logCSV %>% dplyr::filter(nome == alunosLog[i] & nome_evento == "Um arquivo foi enviado." & componente == "Envio de arquivos") %>% nrow;
    
    #Contabiliza a quantidade de vezes que o aluno acessou uma atividade
    countAcessosAtividades = logCSV %>% dplyr::filter(nome == alunosLog[i] & nome_evento == "O status da submissão foi visualizado." & componente == "Tarefa") %>% nrow;
    
    #Contabiliz a quantidade de acessos aos arquivos disponibilizados nas disciplinas pelo aluno
    countAcessoArquivos = logCSV %>% dplyr::filter(nome == alunosLog[i] & (nome_evento == "Módulo do curso visualizado" & componente == "Arquivo" & str_detect(evento,"Arquivo:"))) %>% nrow;
    
    
    #Calcula a media do semestre do aluno
    
    notaAux = dplyr::filter(notasLog,nome == alunosLog[i])
    notaMediaSemestre = notaAux$mediaSemestre
    
    
    #Contabiliza a quantidade de vezes que o aluno acessou as discussões de forum
    countAcessoDiscussaoForum = logCSV %>% dplyr::filter(nome == alunosLog[i] & nome_evento == "Discussão visualizada" & componente == "Fórum" & evento == "Fórum: Fórum de dúvidas") %>% nrow;
    
    #O 'i' que está dentro dos colchetes vem da variável do laço de repetição for acima e os nomes após a virgula se referenciam os nomes das colunas do dataFrame, dadosMineracao 
    dadosMineracao[i,"nome"] = alunosLog[i];
    dadosMineracao[i,"quantAcessoForum"] = countAcessoForum;
    dadosMineracao[i,"quantAcessoSlides"] = countAcessoSlide;
    dadosMineracao[i,"quantAcessoChat"] = countAcessoChat;
    dadosMineracao[i,"quantAcessoVideos"] = countAcessoVideos;
    dadosMineracao[i,"quantiAcessoDiscussaoForum"] = countAcessoDiscussaoForum;
    
    dadosMineracao[i,"quantAcessoSemestre"] = countAcessoSemestre;
    
    dadosMineracao[i,"quantArquivosEnviados"] = countArquivosEnviados;
    dadosMineracao[i,"quantAcessosAtividades"] = countAcessosAtividades;
    dadosMineracao[i,"mediaSemestre"] = notaAux$mediaSemestre;
    dadosMineracao[i,"quantAcessoArquivos"] = countAcessoArquivos;
    dadosMineracao[i,"Status"] = notaAux$status;
    
  } 
}
dadosMineracao$nome = alunosLog
#Atributos excluidos do data frame
dadosMineracao$quantAcessoDisciplina_FundLogiComputacional = NULL
dadosMineracao$quantAcessoDisciplina_redesComputadores = NULL
dadosMineracao$quantAcessoDisciplinaArquiteturaComputadores = NULL
dadosMineracao$quantAcessoDisciplinaProgramasAplicativos = NULL
dadosMineracao$quantAcessoDisciplinaEmpreendedorismo = NULL
dadosMineracao$quantAcessoDisciplinaEticaProfissional = NULL
dadosMineracao$quantAcessoDisciplinaFundamentosInformática = NULL
dadosMineracao$quantAcessoDisciplinaAmbientaçãoEAD = NULL
dadosMineracao$quantAcessoSessãoChat = NULL
dadosMineracao$quantAcessoLink = NULL
dadosMineracao$quantPostForum = NULL

setwd("C:\\Users\\IFBA\\Desktop\\Concomitante\\3 semestre\\tec. Redes")
write.csv(dadosMineracao, file = "dadosMineracao Redes 3semestre(08-11-22)).csv")


countAcessoForum
scan()

summary(notasLog)




#Para exportar dataFrames para csv's 
#setwd("C:/Users/IFBA/Desktop/Logs_csv")
#write.csv(dadosMineracao, file = "dadosMineracao2.csv", row.names = false)
