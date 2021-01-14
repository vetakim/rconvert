rotZ <- function(alpha){
    # матрица вращения вокруг оcи Z
    #
    # mat = rotZ(alpha)
    # где mat -- матрица вращения 3х3

    mat <- matrix(
                  c(cos(alpha), -sin(alpha), 0,
                    sin(alpha),  cos(alpha), 0,
                    0, 0, 1),
                  nrow=3, ncol=3)

    return(t(mat))
}
