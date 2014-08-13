\name{generate}
\alias{generate}
\alias{generate.GPCAvarest2}
\alias{generate.default}
\alias{generate.matrix}
\alias{generate.varest}
\alias{generate.varest2}
\title{generate}
\usage{
generate(x = NULL, ...)

\method{generate}{default}(x, FUN = rnorm, n = 100, K = 3, names = NULL,
  cov = NULL, ...)

\method{generate}{varest}(x, FUN = rnorm, n = 100, names = NULL,
  noise = NULL, exogen = NULL, xprev = NULL, gap.filling = NULL, ...)

\method{generate}{varest2}(x, FUN = rnorm, n = 100, names = NULL,
  noise = NULL, exogen = NULL, xprev = NULL, gap.filling = NULL, ...)

\method{generate}{GPCAvarest2}(x, FUN = rnorm, n = 100, names = NULL,
  noise = NULL, exogen = NULL, xprev = NULL, extremes = TRUE,
  type = 3, gap.filling = NULL, ...)

\method{generate}{matrix}(x, FUN = rnorm, n = 100, noise = NULL,
  xprev = NULL, names = NULL, gap.filling = NULL, ...)
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

  \item{...}{further arguments for \code{FUN}}

  \item{extremes,type}{see \code{\link{inv_GPCA}}}
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
}
\seealso{
\code{\link{getVARmodel}}
}

