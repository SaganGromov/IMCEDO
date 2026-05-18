program calcula_seno
  implicit none
  ! Declaração de variáveis
  integer :: i, n                ! i: índice do laço; n: número de pontos
  double precision :: x, dx, pi  ! x: valor atual da variável; dx: espaçamento; pi: valor de π
  double precision, parameter :: pi_val = 4.0d0 * atan(1.0d0)
  ! pi_val: valor de π calculado com precisão dupla (π = 4 * arctan(1))

  double precision, dimension(25) :: valores_x, valores_seno
  ! Arrays para armazenar os valores de x e seus senos, embora não usados diretamente no arquivo

  character(len=20) :: nome_arquivo
  ! Nome do arquivo de saída

  nome_arquivo = 'saida_seno.dat'   ! Define o nome do arquivo de saída
  n = 25                            ! Define o número de pontos no intervalo
  dx = (2.0d0 * pi_val) / (n - 1)   ! Calcula o espaçamento entre os pontos: (2π dividido em 24 intervalos)

  open(unit=10, file=nome_arquivo, status='replace')
  ! Abre o arquivo de saída para escrita (unit 10), substituindo se já existir

  do i = 1, n
    x = (i - 1) * dx                ! Calcula o valor de x no ponto i
    write(10,*) x, sin(x)
    ! Escreve no arquivo o valor de x e sin(x), separados por espaço
    ! F12.8: formato com 12 caracteres no total e 8 casas decimais
  end do

  close(10)
  ! Fecha o arquivo após a escrita

  print *, "Arquivo criado com sucesso: ", trim(nome_arquivo)
  ! Mensagem de sucesso exibida no terminal

end program calcula_seno

