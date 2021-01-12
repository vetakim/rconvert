rotX <- function(alpha){
    # матрица вращения вокруг оси X
    #
    # mat = rotX(alpha)
    # где mat -- матрица вращения 3х3

    mat <- matrix( c(1, 0, 0,
                     0, cos(alpha), -sin(alpha),
                     0, sin(alpha),  cos(alpha)),
                  nrow=3, ncol=3)

    return(mat)
}
