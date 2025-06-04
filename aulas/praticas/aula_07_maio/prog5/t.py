import math
pi = math.pi 
y_0 = 3*pi/4 
def f(y):
    return 8 * math.cos(g(y)) + 4*math.sqrt(2)
def g(y):
    return y + 0.01/2 * math.sqrt(8*math.cos(y) + 4*math.sqrt(2)) 
for i in range(1, 100):
    y_0 = y_0 + 0.01 * math.sqrt(f(y_0))
    print(y_0)