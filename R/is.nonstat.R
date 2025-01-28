## Changelog:
# MH 0.0.1 2025-01-29: initial

## Documentation
#' @title Test for nonstationarity
#' @description This function tests whether a time series is stationary or not
#' @param tseries a numerical vector
#' @param nEp number of epochs (in which time series is cut for PSR calculation)
#' @param cut.psr1 threshold for PSR value that indicates mean non-stationarity
#' @param cut.psr2 threshold for PSR value that indicates variance non-stationarity
#' @param span numerical value that is passed to the \code{loess} function
#' @return a logical scalar indicating whether the time series is stationary (\code{TRUE}) or not (\code{FALSE})

## Function definition
is.nonstat <- function( tseries, nEp=2, cut.psr1=1.10, cut.psr2=1.01, span=3 ){
		
	psr1 <- psr1( tseries=tseries, nEp=nEp )
	psr2 <- psr2( tseries=tseries, nEp=nEp, span=span )
	
	nonstat.mean <- psr1 > cut.psr1
	nonstat.var <- psr2 > cut.psr2
	
	ret <- nonstat.mean | nonstat.var
	
	attr(ret, "psr1") <- psr1
	attr(ret, "nonstat.mean") <- nonstat.mean
	attr(ret, "psr2") <- psr2
	attr(ret, "nonstat.var") <- nonstat.var

	return( ret )
}

### development

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/140_nonstationary_test/nonstat/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("is.nonstat.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

# set.seed( 1234 )
# x <- rnorm(100)
# is.nonstat( x )



### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
