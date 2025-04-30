! filepath: /home/sagan/IMCEDO/listas/solucoes/lista0/questao1/inverter_ordem.f90
program inverter_ordem
    implicit none
    real :: x, y
    integer :: ios
    character(len=200) :: input_file, output_file

    ! Caminhos absolutos dos arquivos
    input_file = "/home/sagan/IMCEDO/listas/solucoes/lista0/questao1/saida_seno.dat"
    output_file = "/home/sagan/IMCEDO/listas/solucoes/lista0/questao2/saida_seno_invertido.dat"

    ! Abra os arquivos
    open(10, file=input_file, status="old", action="read")
    open(20, file=output_file, status="replace", action="write")

    ! Leia e inverta os valores
    do
        read(10, *, iostat=ios) x, y
        if (ios /= 0) exit
        write(20, *) y, x
    end do

    ! Feche os arquivos
    close(10)
    close(20)

    print *, "Arquivo invertido gerado com sucesso:", output_file
end program inverter_ordem
