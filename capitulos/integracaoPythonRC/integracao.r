if (!require("pacman")) install.packages("pacman")


#--- INTEGRAÇÃO COM PYTHON ---#
# Instalando pacote e listando seus comandos #
p_load(reticulate)
ls("package:reticulate")

# Fazendo a instalação dos pacotes do Python através do R #
use_virtualenv("./venv", required = TRUE)
# py_install("pandas")
# py_install("numpy")

# Importando os pacotes por diferentes métodos (import e REPL) #
# O REPL é um ambiente semelhante a um console em que você pode experimentar dinamicamente expressões UITest
pd <- import("pandas", as = "pd", convert = FALSE)
repl_python(
  input = "import numpy as np
  array = np.array([[1,2],[3,4],[5.0,6]])
  print(array)
  print(type(array))"
)

# Convertendo o objeto do Python para objeto do R
array <- py$array
class(array)

# Gerando data frame do R por leitura de csv
r_df <- read.csv("capitulos/scikit-learn/breast_cancer.csv")
head(r_df)
class(r_df)

# Convertendo o data frame do R para o Python (Pandas)
py_df <- r_to_py(r_df)
class(py_df)

# Criando dicionário no Python
repl_python(
  input = "
  dictionary = {'alpha': 1, 'beta': 2, 'lista': list(range(5))}
  print(dictionary)
  print(type(dictionary))"
)

# Convertendo dicionário do Python para lista do R
(dictionary <- py$dictionary)
class(dictionary)



#--- INTEGRAÇÃO COM C++ ---#
library(Rcpp)
ls("package:Rcpp")


# Criando função de soma no R
soma_r <- function(v) {
  total <- 0
  for (e in v) {
    if (e < 0) {
      total <- total - e
    } else if (e > 0.75) {
      total <- total + e / 2
    } else {
      total <- total + e
    }
  }

  return(total)
}

# Criando função de soma no C++
cppFunction(
  "double soma_c(NumericVector v) {
    double total = 0;
    for (int i = 0; i < v.size(); i++) {
      if (v[i] < 0) { total -= v[i]; }
      else if (v[i] > 0.75) { total += v[i]/2; }
      else { total += v[i]; }
    }

    return(total);
  }"
)

# Atribuindo um vetor de 100000 valores a v e rodando as funções no R e no C++
v <- runif(100000, -1, 1)
microbenchmark::microbenchmark(soma_r(v), soma_c(v))