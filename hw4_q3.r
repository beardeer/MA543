X = seq( from=-2, to=+2, length.out=100 )

Y = 1 + X - 2 * ( X-1 )^2 * ( X >= 1 )

plot( X, Y, type='l' )
grid()
