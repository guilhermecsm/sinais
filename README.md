# R Package

Este pacote tem a finalidade de construir uma base de covariáveis de maneira automatizada através do processo de feature engineering (função do R: featuresSinais)
Além disso, esse pacote cria o label da variável target de maneira automatizada em: pico, vale ou nível. (função do R: classificadorPico).

Após esse processo, o usuário terá uma base analítica pronta e preparada para ser usada em cojunto com um modelo classifcador de machine learning, por exemplo, random forest.

## Instalação e importe da biblioteca
devtools::install_github("guilhermecsm/sinais")
library(sinais)

## Documentação da biblioteca
help(package = "sinais")