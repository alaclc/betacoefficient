#' @title Raju's Beta Coefficient
#' @description Calculate the internal consistency coefficient of a battery of tests whose subscales are independent
#' @author Alexis Junior La Cruz
#' @param df Assign the database containing only subscales total scores of a test
#' @param ... Assign the number of items containing every subscales
#' @return internal consistency coefficient
#' @export Beta
#' @examples
#' Beta(df,3,4,5,6,7)
Beta <- function(df, ...){
  varianza_global<-var(rowSums(df))
  varianza_subtest<-sum(sapply(df,var))
  numerador <- varianza_global-varianza_subtest
  total_items<-sum(...)
  argumentos <- list(...)
  items_subtest <- unlist(argumentos)
  resultado <- (items_subtest/total_items)^2
  resta <- 1-sum(resultado)
  denominador <- varianza_global*resta
  Coeficiente_Beta <- numerador/denominador
  return(Coeficiente_Beta)
}
