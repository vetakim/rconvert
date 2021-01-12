rad2deg <- function( rad ) {
    return ( rad * 180 / pi )
}

deg2rad <- function( deg ) {
    return ( deg * pi / 180 )
}

unionAngleGrad <- function( grad, min, sec ) {
    return( grad + min / 60 + sec / 3600 )
}
