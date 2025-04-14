#include <vector>
#include <cmath>
#include <iostream>
#include <stdexcept>

using Matrix = std::vector<std::vector<double>>;
// using Vector = std::vector<double>;
// using namespace std;


int ReferenceVsValueImmutable(const int& a_in) {
    int b = a_in+1;
    return a_in;
}

int ReferenceVsValue(int& a_in) {
    a_in++;
    return a_in;
}

int ReferenceVsValue2(int a_in) {
    a_in++;
    return a_in;
}


std::vector<double> GetSolutionViaGaussSeidel(
    Matrix A,
    const std::vector<double> b,
    const std::vector<double> initial_guess,
    double tolerance
    // não precisa passar a tolerance como const porque copiar double é muito barato
) {
    std::vector<double> x = initial_guess;
    double residue = 0;
    // for(int n = 0; n < 0; n++) {
    for(int n = 0; n < 100; n++) {
        double sum = 0;
        for(int i = 0; i < A.size(); i++) {
            sum = 0;
            for(int j = 0; j < A[i].size(); j++) {
                if(i != j) {
                    sum += A[i][j] * x[j];
                }
            }
            x[i] = (b[i] - sum) / A[i][i];
        }
        residue=0;
        for(int i = 0; i < A.size(); i++) {
            sum = 0;
            for(int j = 0; j < A[i].size(); j++) {
                sum += A[i][j] * x[j];
            }
            // residue += (b[i] - sum) * (b[i] - sum);
            residue+=std::pow(b[i] - sum, 2);
        }
        if(std::sqrt(residue) < tolerance) {
            return x;
        }
    }
    //falha - iterar mas não chegar numa convergencia
    throw std::runtime_error("Gauss-Seidel method did not converge!");
    return x;
}


int main() {
    std::vector<double> b = {8.0, 3.0, 7.0};
    Matrix A = {
        {10.0, 1.0, 1.0},
        {2.0, -7.0, 0.0},
        {4.0, -2.0, 12.0}
    };
    std::vector<double> initial_guess = {3242341.0,3434561.0, 8765441.0};
    auto x = GetSolutionViaGaussSeidel(A, b, initial_guess, 1e-6);
    std::cout << x[0] << " ," << x[1] << " ," << x[2] << " ," << std::endl;

    // int a = 30;
    // std::cout << ReferenceVsValue2(a) << ", " << a << std::endl;
    // std::cout << ReferenceVsValue(a) << ", " << a << std::endl;


    // std::vector<double> x = initial_guess;
    // double residue = 0;
    // for(int n = 0; n < 100; n++) {
    //     double sum = 0;
    //     for(int i = 0; i < A.size(); i++) {
    //         sum = 0;
    //         for(int j = 0; j < A[i].size(); j++) {
    //             if(i != j) {
    //                 sum += A[i][j] * x[j];
    //             }
    //         }
    //         x[i] = (b[i] - sum) / A[i][i];
    //     }
    //     residue=0;
    //     for(int i = 0; i < A.size(); i++) {
    //         sum = 0;
    //         for(int j = 0; j < A[i].size(); j++) {
    //             sum += A[i][j] * x[j];
    //         }
    //         // residue += (b[i] - sum) * (b[i] - sum);
    //         residue+=std::pow(b[i] - sum, 2);
    //     }
    // }
    // // std::ofstream file("a.txt");
    // // quais são as vantagens de printar as coisas assim? pesquisar depois
    // std::cout << x[0] << " ," << x[1] << " ," << x[2] << " ," << residue << std::endl;
    // return 0;
}