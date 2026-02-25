subroutine gem(A, b, x)
    real(8), intent(in)  :: A(:,:), b(:)
    real(8), allocatable, intent(out) :: x(:)

    real(8), allocatable :: Aug(:,:)
    integer :: n
    integer :: i, j, k
    real(8) :: factor, sum

    n = size(b)
    allocate(Aug(n, n+1))
    allocate(x(n))

    !----------------------------------
    ! Build augmented matrix
    !----------------------------------
    Aug(:,1:n) = A
    Aug(:,n+1) = b

    call printmtx(Aug, n, n+1, "Initial Augmented Matrix")

    !----------------------------------
    ! FORWARD ELIMINATION
    !----------------------------------
    do k = 1, n-1

        print *, "Pivot step k = ", k
        print *, "Pivot = ", Aug(k,k)

        do i = k+1, n

            factor = Aug(i,k) / Aug(k,k)
            print *, "Eliminating row ", i, " factor = ", factor

            do j = k, n+1
                Aug(i,j) = Aug(i,j) - factor * Aug(k,j)
            end do

        end do

        call printmtx(Aug, n, n+1, "After elimination step")

    end do

    !----------------------------------
    ! BACK SUBSTITUTION
    !----------------------------------
    print *, "Starting Back Substitution"

    x(n) = Aug(n,n+1) / Aug(n,n)

    do i = n-1, 1, -1
        sum = 0.0d0

        do j = i+1, n
            sum = sum + Aug(i,j) * x(j)
        end do

        x(i) = (Aug(i,n+1) - sum) / Aug(i,i)
    end do

    print *, "Solution vector:"
    write(*,'(100(f10.4,1x))') x

end subroutine

end module
