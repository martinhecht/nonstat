## Changelog:
# MH 0.0.1 2025-01-29: initial
# MH 0.0.3 2025-01-31: mod and checks on time series epoch length
# MH 0.0.5 2025-02-25: changed seed of the example so that FALSE (true positive) results
#                      changed example from T=100 to T=50


## Documentation
#' @title Test for nonstationarity
#' @description This function tests whether a time series is stationary or not
#' @param tseries a numerical vector
#' @param nEp number of epochs (in which time series is cut for PSR calculation)
#' @param cut.psr1 threshold for PSR value that indicates mean non-stationarity
#' @param cut.psr2 threshold for PSR value that indicates variance non-stationarity
#' @param span numerical value that is passed to the \code{loess} function
#' @return a logical scalar indicating whether the time series is stationary (\code{TRUE}) or not (\code{FALSE})
#' @references
#' Zitzmann, S., Lindner, C., Lohmann, J. F., & Hecht, M. (2024). A novel nonvisual procedure for screening for nonstationarity in time series as obtained from intensive longitudinal designs [Preprint]. https://www.researchgate.net/publication/384354932_A_Novel_Nonvisual_Procedure_for_ Screening_for_Nonstationarity_in_Time_Series_as_Obtained_from_Intensive_Longitudinal_Designs
#' @examples
#' set.seed( 8332278 )
#' x <- rnorm( 50 )
#' is.nonstat( x )

## Function definition
is.nonstat <- function( tseries, nEp=2, cut.psr1=1.10, cut.psr2=1.01, span=3 ){
	
	# MH 0.0.3 2025-01-31: mod on time series epoch length
	# loop to remove elements until the length is both divisible by nEp and greater than nEp
	while (length(tseries) %% nEp != 0 && length(tseries) > 3*nEp) {
		tseries <- tseries[-length(tseries)]  # Remove last element
	}

	if( length(tseries) %% nEp == 0 && length( tseries ) >= 3*nEp ){

		psr1 <- try( psr1( tseries=tseries, nEp=nEp ) )
		psr2 <- try( psr2( tseries=tseries, nEp=nEp, span=span ) )

		if( inherits( psr1, "try-error" ) ) psr1 <- NULL
		if( inherits( psr2, "try-error" ) ) psr2 <- NULL
		
		if( !is.null( psr1 ) && !is.na( psr1 ) && !is.infinite( psr1 ) ) nonstat.mean <- psr1 > cut.psr1 else {
			nonstat.mean <- NULL
			warning( "Unable to assess the nonstationarity of the mean." )
		}
		if( !is.null( psr2 ) && !is.na( psr2 ) && !is.infinite( psr2 ) ) nonstat.var  <- psr2 > cut.psr2 else {
			nonstat.var  <- NULL
			warning( "Unable to assess the nonstationarity of the variance." )
		}
		
		if( !is.null( nonstat.mean ) && !is.null( nonstat.var ) ){
			ret <- nonstat.mean | nonstat.var
		} else {
			ret <- NULL
		}

		if( !is.null( ret ) ){
			attr(ret, "psr1") <- psr1
			attr(ret, "nonstat.mean") <- nonstat.mean
			attr(ret, "psr2") <- psr2
			attr(ret, "nonstat.var") <- nonstat.var
		}

	} else {
		ret <- NULL
		warning("The length of 'tseries' should be at least 3 times 'nEp', and it must be divisible by 'nEp' without a remainder.")
	}

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
# x <- rnorm( 100 )
# is.nonstat( x )

# set.seed( 1234 )
# x <- rnorm( 15 )
# is.nonstat( x, nEp=5 )



### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
