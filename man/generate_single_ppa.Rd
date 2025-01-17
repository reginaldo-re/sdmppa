% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_ppa.R
\name{.generate_single_ppa}
\alias{.generate_single_ppa}
\title{.generate_single_ppa}
\usage{
.generate_single_ppa(
  ES = NULL,
  P = NULL,
  ES_idx = NULL,
  sp_name = NULL,
  ppa_function = NULL,
  ...
)
}
\arguments{
\item{ES}{é o espaço ambiental composto pelas variáveis ambientais.
Representado sempre por um dataframe, deve conter uma
coluna que que é um índice (\code{ES_idx}). Esse índice pode referenciar uma grid
de polígonos ou um raster que representa o espaço geográfico. Ou, então,
apenas um índice de ligação entre \strong{P} e \strong{ES}. Essa coluna
deve identificar univocamente cada célula da grid ou raster.}

\item{P}{é o espaço ambiental das presenças de uma dada espécie.
Representado por um dataframe, deve conter um subconjunto das variáveis de
\strong{ES}, e também um índice, pelo qual será feita a junção com \strong{ES}. Deve
conter, além do índice, uma coluna com o nome da espécie (\code{sp_name}) cujos
valores representam as presenças. Qualquer valor acima de zero é considerado
uma presença. \strong{P} e \strong{ES} sofrerão uma junção por meio de \code{ES_idx}, de maneira
que todas linhas de \strong{P} tenham correspondência em \strong{ES}, e \strong{ES} tenha linhas
suficientes para serem geradas as pseudo-ausências.}

\item{ES_idx}{armazena o nome da coluna de \strong{ES} que deve ser usada como
índice.}

\item{sp_name}{armazena o nome da coluna de \strong{ES} que representa a espécie.}

\item{ppa_function}{função que será invocada para efetivamente gera as
presenças e pseudo-ausências.}
}
\value{
Retorna um dataframe com o espaço ambiental de \strong{ES} que representa
as presenças (\strong{P}) e as pseudo-ausências geradas (\code{PA}). O número de
presenças é sempre o mesmo número de pseudo-ausências.
}
\description{
Produz como resultado um dataframe com as presenças e as pseudo-ausências
geradas pela função que está em \code{ppa_function}.
}
\details{
Essa função valida a entrada \strong{P}, \strong{ES}, \code{ES_idx}, \code{sp_name} e
\code{ppa_function} invoca a função de geração especifica que está em
\code{ppa_function} e checa a saida produzida, que chamamos de \code{PPA}, representada
por um dataframe contendo em mesmo número as presenças e as pseudo-ausências.
}
\keyword{internal}
