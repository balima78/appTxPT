install.packages("plotly")
install.packages("shinydashboard")
install.packages("plotly")
install.packages("DT")
install.packages("tidyr")
install.packages("shinythemes")

install.packages('rsconnect')

rsconnect::removeAccount("balima")

rsconnect::setAccountInfo(name='bioestatisticas',
                          token='7ABE1E14AF786A7227F6F95837E44861',
                          secret='4DGi2kzm/a5i6ww9XiruPwzkNk0FVjTWNaPfV2Jm')

library(rsconnect)
rsconnect::deployApp(getwd())



y



