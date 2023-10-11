# intended for 350 x 350 px
# cbf
lwd <- 4
plot(1:200, 2 * cos(1:200 / 200 * 2 * pi - pi), "l", col = "black", lwd = lwd, axes = FALSE, ylim = c(-2, 4))
lines(1:200, 3 + 0.75 * cos(1:200 / 200 * 2 * pi - 1.1 * pi), col = "black", lwd = 4)
lines(1:200, 0.75 + 1.5 * cos(1:200 / 200 * 2 * pi - 0.9 * pi), col = "black", lwd = 4)
lines(1:200, 0.5 + 1.25 * cos(1:200 / 200 * 2 * pi - 1.3 * pi), col = "black", lwd = 4)
lines(1:200, 2 + 1.75 * cos(1:200 / 200 * 2 * pi - 0.7 * pi), col = "black", lwd = 4)
# cbf - popmean
lwd <- 8
plot(1:200, 2 * cos(1:200 / 200 * 2 * pi - pi), "l", col = "black", lwd = lwd, axes = FALSE, ylim = c(-2, 4))

# qt1
plot(1:200, 2 * sin(1:200 / 200 * 2 * pi), "l", col = "black", lwd = lwd, axes = FALSE, ylim = c(-2, 4))
lines(1:200, 3 + 0.75 * sin(1:200 / 200 * 2 * pi + pi / 4), col = "black", lwd = lwd)
lines(1:200, 0.75 + 1.75 * sin(1:200 / 200 * 2 * pi + 2 * pi / 4), col = "black", lwd = lwd)
lines(1:200, 0.5 + 1.5 * sin(1:200 / 200 * 2 * pi + 3 * pi / 4), col = "black", lwd = lwd)
lines(1:200, 2 + 1.25 * sin(1:200 / 200 * 2 * pi + 5 * pi / 4), col = "black", lwd = lwd)
lines(1:200, sin(1:200 / 200 * 2 * pi + 6 * pi / 4), col = "black", lwd = lwd)

# arryhthmic
plot(1:200, 0.1 * sin(1:200 / 200 * 2 * pi), "l", col = "black", lwd = lwd, axes = FALSE, ylim = c(-2, 4))
lines(1:200, 3 + 0.1 * sin(1:200 / 200 * 2 * pi + pi / 4), col = "black", lwd = lwd)
lines(1:200, 0.75 + 0.075 * sin(1:200 / 200 * 2 * pi + 2 * pi / 4), col = "black", lwd = lwd)
lines(1:200, 0.5 + 0.05 * sin(1:200 / 200 * 2 * pi + 3 * pi / 4), col = "black", lwd = lwd)
lines(1:200, 2 + 0 * sin(1:200 / 200 * 2 * pi + 5 * pi / 4), col = "black", lwd = lwd)
lines(1:200, 1 + 0.025 * sin(1:200 / 200 * 2 * pi + 6 * pi / 4), col = "black", lwd = lwd)
