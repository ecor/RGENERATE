\name{generate}
\alias{generate}
\alias{generate.GPCAvarest2}
\alias{generate.MonthlyList}
\alias{generate.default}
\alias{generate.list}
\alias{generate.matrix}
\alias{generate.varest}
\alias{generate.varest2}
\title{generate}
\usage{
generate(x = NULL, ...)

\method{generate}{default}(x, FUN = rnorm, n = 100, K = 3, names = NULL,
  cov = NULL, gap.filling = NULL, ...)

\method{generate}{varest}(x, FUN = rnorm, n = 100, names = NULL,
  noise = NULL, exogen = NULL, xprev = NULL, gap.filling = NULL, ...)

\method{generate}{varest2}(x, FUN = rnorm, n = 100, names = NULL,
  noise = NULL, exogen = NULL, xprev = NULL, gap.filling = NULL, ...)

\method{generate}{GPCAvarest2}(x, FUN = rnorm, n = 100, names = NULL,
  noise = NULL, exogen = NULL, xprev = NULL, extremes = TRUE,
  type = 3, gap.filling = NULL, GPCA.row.gap.filling.option = TRUE, ...)

\method{generate}{matrix}(x, FUN = rnorm, n = 100, noise = NULL,
  xprev = NULL, names = NULL, gap.filling = NULL,
  type = c("autoregression", "covariance"), ...)

\method{generate}{list}(x, factor.series = names(x), n = NA, ...)

\method{generate}{MonthlyList}(x, origin, n, ...)
}
\arguments{
  \item{x}{null object or the model used for random
  generation , e.g. a VAR model as a
  \code{\link{varest-class}} or \code{\link{varest2-class}}
  object. Default is \code{NULL}.}

  \item{FUN}{random function of the probability
  distribution used for noise random generation. Default is
  \code{\link{rnorm}}. See
  \url{http://cran.r-project.org/web/views/Distributions.html}}

  \item{n}{number of generations requested}

  \item{names}{null object or string vectors or names of
  the variables to be generated simultaneously. Default is
  \code{NULL}.}

  \item{K}{number of the variables to be generated
  simultaneously, i.e. the K parameters of a VAR. It is
  automatically detected by \code{x}, \code{names} or
  \code{cov}, if one of these is not \code{NULL}.}

  \item{cov}{null object or covariance matrix of the random
  variables to be generated simultaneously. Default is
  \code{NULL}, not used in case this information can be
  detected from \code{x}.}

  \item{noise}{null object or a generic external noise for
  \code{x} model residuals, e.g. standard white noise, for
  random generation with the model \code{x}. Default is
  \code{NULL}. If \code{NULL} the noise is automatically
  calculated.}

  \item{exogen}{null object or amatrix or data frame with
  exogeneous variables (predictors) id requested by
  \code{x}. Default is \code{NULL}}

  \item{xprev}{null object or initial condition of the
  multivariate random process to be generated. Default is
  \code{NULL}.}

  \item{gap.filling}{data frame with time series with gabs
  (\code{NA} values) to be filled. Default is \code{NULL}
  and not considered, otherwise the method returns this
  data frame with \code{NA} row replaced with generated
  (e.g auto-regressed) values.}

  \item{GPCA.row.gap.filling.option}{logical value. Default
  is \code{TRUE}. In case of
  \code{\link{GPCAvarest2-class}} objects, If
  \code{gap.filling} contains both \code{NA} and finite
  values in the same row, this row will contains all
  \code{NA} values after GPCA. In this case all row values
  are generated through auto-regression. If
  \code{GPCA.row.gap.filling.option} all insterted non-NA
  \code{gap.filling} values are repleced before returning
  the function value. Otherwise, in the rows with
  \code{NA}s all values are re-generated. The option
  \code{TRUE} is not safe in case the gaps are vary long
  becouse the genereted values is used for subsequent
  auto-regrossion.}

  \item{type}{character string used in some method
  implementations. See \code{\link{inv_GPCA}}. In the
  matrix implementation, default is
  \code{"autoregression"}, i.e. the matrix is used as a
  vector auto-regression coefficient, if it is
  \code{"covariance"} the method genereted a sample with
  covariance matrix given by \code{x}.}

  \item{...}{further arguments for \code{FUN}}

  \item{extremes}{see \code{\link{inv_GPCA}}}

  \item{factor.series}{factor series used by
  'factor.series'}

  \item{origin}{start date for generation. See
  \code{\link{adddate}}}
}
\value{
a matrix or a data frame object
}
\description{
generate

It generates a multivarite random series according to the
model \code{x}
}
\examples{
library(RGENERATE)

set.seed(122)
NSTEP <- 1000
x <- rnorm(NSTEP)
y <- x+rnorm(NSTEP)
z <- c(rnorm(1),y[-1]+rnorm(NSTEP-1))
df <- data.frame(x=x,y=y,z=z)
var <- VAR(df,type="none")
gg <- generate(var,n=20)

cov <- cov(gg)

ggg <- generate(FUN=rnorm,n=NSTEP,cov=cov)


library(RMAWGEN)
exogen <- as.data.frame(x+5)
gpcavar <- getVARmodel(data=df,suffix=NULL,p=3,n_GPCA_iteration=5,
n_GPCA_iteration_residuals=5,exogen=exogen)
gpcagg <- generate(gpcavar,n=20,exogen=exogen)

## Generate an auto-regrassive time-series with a generic matrix

A <- diag(c(1,-1,1))
mgg <- generate(A,n=100)

### Gap Filling Examples

 dfobs <- df
 dfobs[20:30,] <- NA
 n <- nrow(df)
 dffill <- generate(gpcavar,n=n,exogen=exogen,gap.filling=dfobs,names=names(dfobs))

qqplot(dfobs$y,dffill$y)
abline(0,1)

### Gap filling with matrix

mgg_n <- mgg
mgg_n[20:30,2] <- NA

mgg_nfill <- generate(A,gap.filling=mgg_n)

print(mgg_n[1:31,])
print(mgg_nfill[1:31,])

dfobs2 <- df
dfobs2[20:30,2] <- NA
n <- nrow(df)
dffill2 <- generate(gpcavar,n=n,exogen=exogen,gap.filling=dfobs2,names=names(dfobs2))

qqplot(dfobs$y,dffill$y)
abline(0,1)

### generation with 'generetion.matrix'
### and matrix 'x' is a covariance matrix

covariance <- array(0.5,c(3,3))

diag(covariance) <- 1

set.seed(127)
ngns <- 1000
gg1 <- generate(FUN=rnorm,n=ngns,cov=covariance)
set.seed(127)
gg2 <- generate(covariance,type="covariance",n=ngns)


## generate with a list of covariance matrix
ndim <- 5
dim <- c(ndim,ndim)
CS1 <- array(0.3,dim)
CS2 <- array(0.5,dim)
CS3 <- array(0.7,dim)
CS4 <- array(0.1,dim)

diag(CS1) <- 1
diag(CS2) <- 1
diag(CS3) <- 1
diag(CS4) <- 1

list <- list(CS1=CS1,CS2=CS2,CS3=CS3,CS4=CS4)

series <- rep(1:4,times=4,each=100)
series <- sprintf("CS\%d",series)
names_A <- sprintf("A\%d",1:ndim)
ggs <- generate(list,factor.series=series,FUN=rnorm,type="covariance",names=names_A)

ggs_CS1 <- ggs[series=="CS1",]
cov(ggs_CS1)

ggs_CS3 <- ggs[series=="CS3",]
cov(ggs_CS3)
}
\seealso{
\code{\link{getVARmodel}}
}

