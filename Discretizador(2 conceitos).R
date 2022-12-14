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
      dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoForum"] = "acima da m??dia";
    }
    
    
    #quantAcessoSlides
    MED = mean(dadosMineracao$quantAcessoSlides)
   
    if(dadosMineracao[c,"quantAcessoSlides"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoSlides"] = "acima da m??dia";
    }
   
    
    #quantAcessoChat
    MED = mean(dadosMineracao$quantAcessoChat)
   
    if(dadosMineracao[c,"quantAcessoChat"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoChat"] = "acima da m??dia";
    }
    
    
    #quantMensagemenviadaChat
    MED = mean(dadosMineracao$quantMensagemenviadaChat)
   
    if(dadosMineracao[c,"quantMensagemenviadaChat"] < MED){
      dadosMineracaoDiscretizado[c,"quantMensagemenviadaChat"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantMensagemenviadaChat"] = "acima da m??dia";
    }
   
    #quantAcessoVideos
    MED = mean(dadosMineracao$quantAcessoVideos)
   
    if(dadosMineracao[c,"quantAcessoVideos"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoVideos"] = "acima da m??dia";
    }
 
    
    #quantiAcessoDiscussaoForum
    MED = mean(dadosMineracao$quantiAcessoDiscussaoForum)
   
    if(dadosMineracao[c,"quantiAcessoDiscussaoForum"] < MED){
      dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantiAcessoDiscussaoForum"] = "acima da m??dia";
    }
  
    
    #quantAcesso_1_Semestre
    MED = mean(dadosMineracao$quantAcesso_1_Semestre)
   
    if(dadosMineracao[c,"quantAcesso_1_Semestre"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcesso_1_Semestre"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcesso_1_Semestre"] = "acima da m??dia";
    }
   
    
    #quantArquivosEnviados
    MED = mean(dadosMineracao$quantArquivosEnviados)
   
    if(dadosMineracao[c,"quantArquivosEnviados"] < MED){
      dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "Abaixo_da_m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantArquivosEnviados"] = "acima da m??dia";
    }

    
    #quantAcessosAtividades
    MED = mean(dadosMineracao$quantAcessosAtividades)
   
    if(dadosMineracao[c,"quantAcessosAtividades"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessosAtividades"] = "acima da m??dia";
    }
    
    #media1Semestre
    MED = mean(dadosMineracao$media1Semestre)
   
    if(dadosMineracao[c,"media1Semestre"] < MED){
      dadosMineracaoDiscretizado[c,"media1Semestre"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"media1Semestre"] = "acima da m??dia";
    }
   
    
    #quantAcessoArquivos
    MED = mean(dadosMineracao$quantAcessoArquivos)
   
    if(dadosMineracao[c,"quantAcessoArquivos"] < MED){
      dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "Abaixo da m??dia";
    }
    else{
      dadosMineracaoDiscretizado[c,"quantAcessoArquivos"] = "acima da m??dia";
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

setwd("C:/Users/IFBA/Desktop/WEKA_DadosMinera????o/dadosWeka/2 conceitos")
write.csv(dadosMineracaoDiscretizado, file = "(2 conceitos)dadosMineracaoFullDiscretizado,InfoNet e Manuten????o(atualizado 16-08).csv")
