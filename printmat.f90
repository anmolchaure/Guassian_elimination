subroutine printmtx(M, rows, cols, name)
    real(8), intent(in) :: M(:,:)
    integer, intent(in) :: rows, cols
    character(len=*), intent(in) :: name
    integer :: i,j

    print *, "---- ", trim(name), " ----"
    do i = 1, rows
        write(*,'(100(f10.4,1x))') (M(i,j), j=1,cols)
    end do
    print *, "--------------------------"
end subroutine
