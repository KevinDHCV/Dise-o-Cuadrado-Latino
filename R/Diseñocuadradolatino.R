#' Tabla ANOVA de un Diseño Cuadrado Latino
#'
#' Genera la tabla de Análisis de Varianza (ANOVA) para un Diseño Cuadrado Latino (DCL)
#'
#' @param respuesta (string) nombre de la variable respuesta.
#' @param tratamiento (string) nombre de la variable que representan a los tratamientos.
#' @param columna (string) nombre de la variable que representa a las columnas.
#' @param fila (string) nombre de la variable que representa a las filas.
#' @param data (\code{data.frame}) Tabla de los datos en formato largo con los datos de los tratamientos, columnas, filas y de la variable respuesta.
#' @return Devuelve una tabla en formato \code{data.frame} con los cálculos correspondientes al análisis de varianza de un Diseño Cuadrado Latino.
#' @export
#'
#' @examples
#' \dontrun{
#' ## 1 Directorio de trabajo
#' ruta<-"D:/ruta/test 1.csv"
#' df<-read.csv(ruta)
#'
#' ## 2 Cargo la librería
#' library(DCL)
#'
#' ## Ejecutamos la función ANOVA_DCL
#' ADCL(respuesta = "Respuesta",tratamiento = "Tratamiento",columna = "Columna",fila = "Fila",data = df)
#' }
ADCL<-function(respuesta,tratamiento,columna,fila,data){
  y<-data[,respuesta]
  trt <-factor(data[,tratamiento])
  col<-factor(data[,columna])
  fila<-factor(data[,fila])
  t<-nlevels(trt)
  #Corrección para la media
  sumxcol<-tapply(y,INDEX=col,FUN=sum)
  sumdcol<-sum(sumxcol)
  C<-sumdcol^2/t^2
  #Suma de cuadrados total
  sc_total<-sum(y^2)-C
  gl_total<-t^2-1
  #suma de cuadrados tratamientos
  sumaxtrt<-tapply(y,INDEX=trt,FUN=sum)
  sc_trt<-sum(sumaxtrt^2)/t-C
  gl_trt<-t-1
  cm_trt<-sc_trt/gl_trt
  #suma de cuadrados de columnas
  sumaxcol<-tapply(y,INDEX=col,FUN=sum)
  sc_col<-sum(sumaxcol^2)/t-C
  gl_col<-t-1
  cm_col<-sc_col/gl_col
  #suma de cuadrados de filas
  sumaxfila<-tapply(y,INDEX=fila,FUN=sum)
  sc_fila<-sum(sumaxfila^2)/t-C
  gl_fila<-t-1
  cm_fila<-sc_fila/gl_fila
  #suma de cuadrados de error
  sc_error<-sc_total-sc_trt-sc_col-sc_fila
  gl_error<-(t-1)*(t-2)
  cm_error<-sc_error/gl_error
  #Valor de f
  f_cal_trt<-cm_trt/cm_error
  f_cal_col<-cm_col/cm_error
  f_cal_fila<-cm_fila/cm_error
  #Crear el dataframe
  tabla<-data.frame(FuenteV=c("Tratamientos","Columnas","Filas","Error exp.","Total"),
                    SumaC=c(sc_trt,sc_col,sc_fila,sc_error,sc_total),
                    GradosL=c(gl_trt,gl_col,gl_fila,gl_error,gl_total),
                    CMedio=c(cm_trt,cm_col,cm_fila,cm_error,NA),
                    FCalculada=c(f_cal_trt,f_cal_col,f_cal_fila,NA,NA),
                    check.names = FALSE)
  rownames(tabla)<-NULL
  anava<-format(tabla)
  anava[is.na(tabla)]<-""

  return(anava)
}

