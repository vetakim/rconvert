rotY <- function(alpha){
    # матрица вращения вокруг оcи Y
    #
    # mat = rotY(alpha)
    # где mat -- матрица вращения 3х3

    mat <- matrix( c(cos(alpha), 0, sin(alpha),
                     0, 1, 0,
                     -sin(alpha), 0, cos(alpha)),
                  nrow=3, ncol=3)

    return(t(mat))
}
