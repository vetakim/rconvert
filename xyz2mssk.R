source(file='blh2xyz.R')
source(file='rotX.R')
source(file='rotY.R')
source(file='rotZ.R')

xyz2mssk <- function(XYZ, S, SC){
# Пересчёт прямоугольных инерциальных коордидинат в местные
# сферические.
#
#   это закомментировано: ТЕПЕРЬ НЕ СВЯЗАННЫЕ С ПОЛОТНОМ!!! ОТСЧЁТ ОТ СЕВЕРА!!!
#
# koord = xyz2msk(XYZ, S, NPos)
#     XYZ = [x, y, z]  [км]
#     XYZ = [x, y, z, dx, dy, dz] [км  км/сек]
#     S -- звёздное время  [рад]
#     NPos -- номер позиции
#     koord = [R_msk; A_msk; h_msk]
#     koord = [R_msk; A_msk; h_msk; dR_msk; dA_msk; dh_msk]
#         R_msk -- радиус-вектор [км]
#         A_msk -- азимут [рад] осчитывается от севера
#         h_msk -- астрономическая высота [рад] угол, отсчитываемый
#                   от плоскости горизонта
#
#


# угловая скорость вращения Земли
OMEGA <- 7.292115e-5

#SC <- getStanConst(Npos)

XYZ_stan <- blh2xyz(SC$B, SC$L, SC$H, S)

LENG <- length(XYZ)

if  (!( (LENG == 3) || (LENG == 6)) ){
    error('Неверно заданы входящие параметры!')
}



# -- матрица вращение на долготу пункта наблюдений
# -- матрица вращения на угол широты пункта
# -- направляем оси как в фпо :-)))
# перенаправляем оси: Z остаётся направленной на север, Y начинает смотреть
# в зенит, X теперь смотрит на запад
#
# в итоге Z смотрит перпендикулярна полотну, X смотрит налево от полотна,
# Y продолжает смотреть в зенит.

# матрица вращения
MATRIX <- rotZ(pi/2) %*% rotY(SC$B) %*% rotZ(-SC$L-S)

XYZ_mpsk <- MATRIX %*% (XYZ[1:3] - XYZ_stan)

X <- XYZ_mpsk[1]
Y <- XYZ_mpsk[2]
Z <- XYZ_mpsk[3]

R <- sqrt(X^2 + Y^2 + Z^2)

A <- - atan(X / Z)
if  (Z < 0){
     A <- pi + A
}
# A = to2pi(A);

h <- asin(Y / R)

koord <- c(R, A, h)

if  (LENG == 6){
    X1 <- XYZ[4] + OMEGA * XYZ[2]
    Y1 <- XYZ[5] - OMEGA * XYZ[1]
    Z1 <- XYZ[6]
    XYZ_mpsk[4:6] <- MATRIX %*% matrix(list(X1, Y1, Z1), nrow=3)

    X1 <- XYZ_mpsk[4]
    Y1 <- XYZ_mpsk[5]
    Z1 <- XYZ_mpsk[6]

    R1 <- (X*X1 + Y*Y1 + Z*Z1) / R

    A1 <- - (Z*X1 - Z1*X)  / (X^2 + Z^2)

    h1 <- Y1*sqrt(X^2 + Z^2) / R^2 - ...
        Y*(X*X1 + Z*Z1) / (R^2*sqrt(X^2 +Z^2))

    koord <- c(R, A, h, R1, A1, h1)
}

    return(koord)
}
