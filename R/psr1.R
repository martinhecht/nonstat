## Changelog:
# MH 0.0.1 2025-01-29: initial

## Documentation
#' @title
#' @description
#' @param
#' @return

## Function definition
psr1 <- function( tseries, nEp = 2 ){
	# Returns potential scale reduction (PSR) factor, which is also called PSR(1) in Zitzmann, Lohmann, & Hecht (2025)
	# Argument:
	#	tseries
	#		is time series
	#	nEp
	# 		is number of epoches
	# Value:
	#		is PSR
	index <- rep( 1 : nEp, each = ( length( tseries ) / nEp ) ) # 
	#per <- matrix( rep( NA, length( index ) ), nEp, ( length( tseries ) - ( length( tseries ) / nEp ) ) ) # periods as rows
	per <- matrix( rep( NA, length( index ) ), nEp, ( length( tseries ) / nEp ) ) # periods as rows
	for ( i in 1 : nEp ){
		#per[ i,  ] <- tseries[ index != i ]
		per[ i,  ] <- tseries[ index == i ]
	}
	
	# ------------ Gelman et al. (2004, p. 296) ------------ 	
	vars.w <- unlist( apply( per, 1, var ) )
	means.w <- unlist( apply( per, 1, mean ) )
	w <- mean( vars.w ) # within-tseries var
	b <-  dim(per)[2]/(dim(per)[1]-1)*sum((means.w-mean( means.w ))^2)# between-tseries var 
	val <- sqrt( ((dim(per)[2]-1)/dim(per)[2]*w+1/dim(per)[2]*b)/w )
	
	## --------- Muthen & Asparouhov (2014, p. 334) --------- 	
	#vars.w <- unlist( apply( per, 1, function(x){mean((x-mean(x))^2)} ) )
	#means.w <- unlist( apply( per, 1, mean ) )	
	#w <- mean( vars.w ) # within-tseries var
	#b <-  var( means.w ) # between-tseries var 
	#val <- sqrt( (w+b)/w )

	return( val ) 
}

### development

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/140_nonstationary_test/nonstat/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("psr1.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
