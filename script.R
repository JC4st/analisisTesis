frequenciesCrosstabs <- function( dependiente = dependiente, independiente = independiente ) 
{
      library(gmodels)
      dependiente.independiente <- table(dependiente, independiente)
      dependiente.independiente <- addmargins(dependiente.independiente) # Adding row/col margins
      tabla <- CrossTable(dependiente,independiente, format=c("SPSS"), digits=1)
      chi2 <- chisq.test(dependiente.independiente) # Do chisq test Ho: no relathionship
      fisher <- fisher.test(dependiente.independiente) # Do fisher'exact test Ho: no relationship
      x <- list(tabla, chi2, fisher )
      names(x) <- c("tabla", "chi2", "fisher")
      x
}

frequenciesCrosstabs(datos$LRA, datos$HTA)

#glm(LRA ~. -ID -historia,family=binomial(link='logit'),data=datos)


summary(glm(LRA ~ ascitis + edad, data = datos, family = "binomial"))

c("ID", "historia", "edad", "sexo", "peso", "talla", "IMC", "HTA", 
  "DM", "EPOC", "HTP", "ERC", "cardiopatiaIsquemica", "valvulopatia", 
  "enferemdadAutoinmune", "indicacionTrasplanteCronico", "indicacionTrasplanteAgudo", 
  "creatinina", "bilirrubinaTotal", "PT", "INR", "albumina", "MELD", 
  "CP", "hospitalizado", "hospitalizacionPrevia", "encefalopatia", 
  "ascitis", "infeccionPrevia", "murteDelDonante", "infeccionDelDonante", 
  "fechaDelTrasplante", "esquemaInmunosupresor", "diasHospitalizacion", 
  "otraCausaIndicacionCronica", "otracasuaIndicacionAguda", "otrasCausasDeMuerteDonante", 
  "otrosDatos", "cuasaMuerte", "mortalidad", "mesesSobrevida", 
  "year", "tiempoQuirurgico", "tiempoDeIsquemiaFria", "tiempoDeIsquemiaTibia", 
  "faseAnhepatica", "bypassVenovenoso", "cristaloides", "coloides", 
  "UGRE", "PFC", "CUP", "crioprecipitado", "sodio", "bilirrubinas", 
  "hipotension", "soporteVasopresor", "hemoglobina", "creatininaBasal", 
  "LRA", "fechaLRA", "creatininaMaxima", "Max", "TRR", "fechaTRR", 
  "censura")

library(Amelia)
missmap(datos, main = "Missing values vs observed")