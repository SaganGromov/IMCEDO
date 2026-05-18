#include <fstream>
#include <cmath>

double lambda = -20; 

double f(double t, double y) {
    return lambda * y; // dy/dt = lambda*y
}

int main() {
    // y' = y, y(0) = 1;
    double h = 1;
    double y = 1;
    double total_integration_time = 10; // Total time for integration
    int n = std::ceil(total_integration_time / h); // Number of steps

    std::ofstream file("-20000_implicit_euler.txt");
    // Implicit midpoint method for y' = lambda*y, y(0) = 1
    // y_{n+1} = y_n + h * f(t_n + h/2, (y_n+y_{n+1})/2);
    // y_{n+1} = y_n + h * lambda * (y_n + y_{n+1})/2;
    // y_{n+1} - h*lambda/2*y_{n+1} = y_n + h*lambda/2*y_n
    // (1 - h*lambda/2) y_{n+1} = (1 + h*lambda/2) y_n
    // y_{n+1} = (1 + h*lambda/2) / (1 - h*lambda/2) * y_n
    for (int i = 0; i < n; i++) {
        double t = i*h;
        file << t << " " << y << std::endl;
        // y += h * f(t,y); // Explicit Euler (commented out)
        y /= (1 - h * lambda);  
    }
    file << n*h << " " << y << std::endl;
}