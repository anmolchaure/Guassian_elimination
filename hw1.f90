program numeric
    use types
    real(rkind), allocatable :: res(:)
    real(rkind) :: In
    integer :: n, nmax

    ! Read from keyboard
    print *, "Enter nmax: "
    read *, nmax

    ! important to allocate this vector, if not, leads to segmentation issues
    allocate(res(0:nmax))
    In = 1.0_rkind - exp ( -1.0_rkind )
    res(0) = In

    do n = 1 , nmax
        In = n * In - exp ( -1.0_rkind )
        print * , n , In
        res ( n ) = In
    end do

end program numeric
