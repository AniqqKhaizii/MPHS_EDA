install.packages("rsconnect")
install.packages("renv")
renv::install("rsconnect")
library(rsconnect)
library(renv)

#rsconnect::setAccountInfo(name='aniqqkhaizii',token='8E408831FE32DA1C124D8BC85D71C14B',secret='tb3sF0iKLCEjUPuZfqDaCWbQ1YNwrp1ifir1GovO')
#rsconnect::deployApp(appDir = 'C:/Users/Admin/Desktop/BCT/MPHS')


rsconnect::setAccountInfo(name='aniqqkhaizii',
                          token='8E408831FE32DA1C124D8BC85D71C14B',
                          secret='tb3sF0iKLCEjUPuZfqDaCWbQ1YNwrp1ifir1GovO')
rsconnect::deployApp(appDir = "C:/Users/Admin/Desktop/BCT/DEPLOYMENT/MPHSNew")