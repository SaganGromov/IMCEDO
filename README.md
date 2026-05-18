# IMCEDO

Repositório público com materiais de estudo e códigos práticos de IMCEDO, incluindo aulas teóricas em PDF, listas de exercícios, soluções e programas numéricos em Fortran, C++ e Python.

O conteúdo cobre implementações e experimentos relacionados a métodos numéricos para equações diferenciais ordinárias, como Euler explícito/implícito, métodos de Taylor, Runge-Kutta, Adams-Bashforth, Adams-Moulton, sistemas dinâmicos, equações rígidas, pêndulo e catenária.

## Estrutura

- `aulas/teoricas/`: notas e livros em PDF.
- `aulas/praticas/`: códigos-fonte, executáveis e arquivos de saída das aulas práticas.
- `listas/`: listas de exercícios e soluções.
- `*.f90`: programas em Fortran.
- `*.cpp`: programas em C++.
- `*.dat` e `*.txt`: dados gerados pelas simulações.
- `gnuplotfile_YURI`: exemplo de script para geração de gráfico com Gnuplot.

## Uso

Para compilar um programa Fortran:

```bash
gfortran arquivo.f90 -o programa
./programa
```

Para compilar um programa C++:

```bash
g++ arquivo.cpp -o programa
./programa
```

Alguns programas geram arquivos de saída como `saida.dat`, que podem ser usados para análise ou visualização gráfica.
