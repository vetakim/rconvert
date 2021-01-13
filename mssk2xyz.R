library(matrixcalc)

mssk2xyz <- function(koord_msk, S, SC){
    # Пересчёт сферических местных координат в геоцентрические,
    # инерциальные.
    #
    # XYZ = msk2xyz(koord_msk, S, NPos)
    #     koord_msk = [R, A, h]
    #     koord_msk = [R, A, h, dR, dA, dh]
    #         R_msk -- радиус-вектор [км]
    #         A_msk -- азимут [рад] осчитывается от севера
    #         h_msk -- астрономическая высота [рад] угол, отсчитываемый
    #                   от плоскости горизонта
    #     S -- звёздное время [рад]
    #     NPos -- номер позиции

    LENG <- length(koord_msk)

    if  (!( (LENG == 3) || (LENG == 6) ) ) {
        error('Неверно заданы входящие параметры!')
    }

    XYZ_stan <- blh2xyz(SC$B, SC$L, SC$H, S)

    OMEGA <- 7.292115e-5

    R <- koord_msk[1]
    A <- koord_msk[2]
    h <- koord_msk[3]
    if  (LENG == 6){
        R1 <- koord_msk[4]
        A1 <- koord_msk[5]
        h1 <- koord_msk[6]
    }

    X_mp <- -R * sin(A) * cos(h)
    Y_mp <- R * sin(h)
    Z_mp <- R * cos(A) * cos(h)

    XYZ_mp <- matrix(c(X_mp, Y_mp, Z_mp), nrow=3)

    if  (LENG == 6){
        X1_mp <- -R1*sin(A)*cos(h) - ...
        R*A1*cos(A)*cos(h) + ...
        R*h1*sin(A)*sin(h)

        Y1_mp <-  R1*sin(h) + ...
        R*h1*cos(h)

        Z1_mp <- R1 * cos(A) * cos(h) - ...
        R * A1 * sin(A) * cos(h) - ...
        R*h1*sin(h)*cos(A)

        XYZ_mp <- matrix(c(X_mp, Y_mp, Z_mp, X1_mp, Y1_mp, Z1_mp), nrow=6)
    }

    # матрица вращения
    MATRIX <- rotZ(pi/2) %*% rotY(SC$B) %*% rotZ(-SC$L-S)

    XYZ_tp <- matrix.inverse(MATRIX) %*% XYZ_mp[1:3]

    XYZ <- XYZ_tp + XYZ_stan

    if (LENG == 6) {
        XYZ[4:6] <- matrix.inverse(MATRIX) %*% XYZ_mp[4:6]

        XYZ[4] <- XYZ[4] - OMEGA * XYZ[2]
        XYZ[5] <- XYZ[5] + OMEGA * XYZ[1]
        XYZ[6] <- XYZ[6]
    }

    #XYZ <- matrix.inverse(XYZ)

    return(XYZ)
}
