dadosMineracao  = read.csv(file.choose());

dadosMineracaoDiscretizado = data.frame(
  nome = character(),
  quantAcessoForum = character(),
  quantAcessoSlides = character(),
  quantAcessoChat = character(),
  quantMensagemenviadaChat = character(),
  quantAcessoVideos = character(),
  quantiAcessoDiscussaoForum = character(),
  quantAcesso_1_Semestre = character(),
  quantArquivosEnviados = character(),
  quantAcessosAtividades = character(),
  media1Semestre = character(),
  quantAcessoArquivos = character(),
  Status = logical())


media = str_replace(dadosMineracao$media1Semestre, "[,]",".")
media
dadosMineracao$media1Semestre = media

dadosMineracao$media1Semestre  = as.numeric(dadosMineracao$media1Semestre)
sum(dadosMineracao$media1Semestre)
summary(dadosMineracao$media1Semestre)

AdicionaLinha = data.frame(nome = NA,
                           quantAcessoForum = NA,
                           quantAcessoSlides = NA,
                           quantAcessoChat = NA,
                           quantMensagemenviadaChat = NA,
                           quantAcessoVideos = NA,
                           quantiAcessoDiscussaoForum = NA,
                           quantAcesso_1_Semestre = NA,
                           quantArquivosEnviados = NA,
                           quantAcessosAtividades = NA,
                           media1Semestre = NA,
                           quantAcessoArquivos = NA,
                           Status = NA)


alunosDiscretos = vector()
alunosDiscretos = dadosMineracao$nome  
c = numeric()

for(c in 1: length(alunosDiscretos)){
  
  dadosMineracaoDiscretizado = rbind(dadosMineracaoDiscretizado,AdicionaLinha)
  
}

for(c in 1: length(alunosDiscretos)){
  
  dadosMineracaoDiscretizado[c,"nome"] = alunosDiscretos[c]
  
  if((str_detect(alunosDiscretos[c], "SMSI") == TRUE) || 
     (str_detect(alunosDiscretos[c], "SINET") == TRUE)){
    
    dadosMineracaoDiscretizado[c,"quantAcessoForum"] = NA;
    dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = NA;
    dadosMineracaoDiscretizado[c,"quantAcessoChat"] = NA;
    dadosMineracaoDiscretizado[c,"quantMensagemenviadaChat"] = NA;
    dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = NA;
    dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = NA;
    dadosMineracaoDiscretizado[c,"quantAcesso_1_Semestre"] = NA;
    dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = NA;
    dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = NA;
    dadosMineracaoDiscretizado[c,"media1Semestre"] = NA;
    dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = NA;
    dadosMineracaoDiscretizado[c,"Status"] = NA;
    
  }
  else{
    #quantAcessoForum
    MED = mean(dadosMineracao$quantAcessoForum)
   
    if(dadosMineracao[c,"quantAcessoForum"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "acima da média";
    }
    
    
    #quantAcessoSlides
    MED = mean(dadosMineracao$quantAcessoSlides)
   
    if(dadosMineracao[c,"quantAcessoSlides"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "acima da média";
    }
   
    
    #quantAcessoChat
    MED = mean(dadosMineracao$quantAcessoChat)
   
    if(dadosMineracao[c,"quantAcessoChat"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "acima da média";
    }
    
    
    #quantMensagemenviadaChat
    MED = mean(dadosMineracao$quantMensagemenviadaChat)
   
    if(dadosMineracao[c,"quantMensagemenviadaChat"] < MED){
      dadosMineracaoDiscretizado[c,"quantMensagemenviadaChat"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantMensagemenviadaChat"] = "acima da média";
    }
   
    #quantAcessoVideos
    MED = mean(dadosMineracao$quantAcessoVideos)
   
    if(dadosMineracao[c,"quantAcessoVideos"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "acima da média";
    }
 
    
    #quantiAcessoDiscussaoForum
    MED = mean(dadosMineracao$quantiAcessoDiscussaoForum)
   
    if(dadosMineracao[c,"quantiAcessoDiscussaoForum"] < MED){
      dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "acima da média";
    }
  
    
    #quantAcesso_1_Semestre
    MED = mean(dadosMineracao$quantAcesso_1_Semestre)
   
    if(dadosMineracao[c,"quantAcesso_1_Semestre"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcesso_1_Semestre"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcesso_1_Semestre"] = "acima da média";
    }
   
    
    #quantArquivosEnviados
    MED = mean(dadosMineracao$quantArquivosEnviados)
   
    if(dadosMineracao[c,"quantArquivosEnviados"] < MED){
      dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "Abaixo_da_média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "acima da média";
    }

    
    #quantAcessosAtividades
    MED = mean(dadosMineracao$quantAcessosAtividades)
   
    if(dadosMineracao[c,"quantAcessosAtividades"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "acima da média";
    }
    
    #media1Semestre
    MED = mean(dadosMineracao$media1Semestre)
   
    if(dadosMineracao[c,"media1Semestre"] < MED){
      dadosMineracaoDiscretizado[c,"media1Semestre"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"media1Semestre"] = "acima da média";
    }
   
    
    #quantAcessoArquivos
    MED = mean(dadosMineracao$quantAcessoArquivos)
   
    if(dadosMineracao[c,"quantAcessoArquivos"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "Abaixo da média";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "acima da média";
    }
    
    
    if(dadosMineracao[c,"Status"] == 0){
      dadosMineracaoDiscretizado[c,"Status"] = FALSE;
    }
    if(dadosMineracao[c,"Status"] == 1){
      dadosMineracaoDiscretizado[c,"Status"] = TRUE;
    }
}  
}
summary(dadosMineracao)

setwd("C:/Users/IFBA/Desktop/WEKA_DadosMineração/dadosWeka/2 conceitos")
write.csv(dadosMineracaoDiscretizado, file = "(2 conceitos)dadosMineracaoFullDiscretizado,InfoNet e Manutenção(atualizado 16-08).csv")
