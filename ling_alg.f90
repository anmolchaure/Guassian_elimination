module linalg
use types
implicit none
private
public :: read_system, printmtx, gem

contains

! Read A and b from file



subroutine read_system(A, b, n, file_name)
    integer(ikind), intent(out) :: n
    real(rkind), allocatable, intent(out) :: A(:,:), b(:)
    character(len=*), intent(in) :: file_name
    integer(ikind) :: i, j, file_id



    open(newunit=file_id, file=file_name, status='old', action='read')

    read(file_id,*) n
    allocate(A(n,n), b(n))

    do i = 1, n
        read(file_id,*) (A(i,j), j=1,n), b(i)
    end do

    close(file_id)
end subroutine


! Debug print matrix

subroutine printmtx(M, rows, cols, name)
    real(rkind), intent(in) :: M(:,:)
    integer(ikind), intent(in) :: rows, cols
    character(len=*), intent(in) :: name

    integer(ikind) :: i, j

    print *, "---- ", trim(name), " ----"
    do i = 1, rows
        write(*,'(100(f12.6,1x))') (M(i,j), j=1,cols)
    end do
    print *, "--------------------------"
end subroutine



! Gaussian Elimination + Back Substitution

subroutine gem(A, b, x)

    real(rkind), intent(in)  :: A(:,:), b(:)
    real(rkind), allocatable, intent(out) :: x(:)

    real(rkind), allocatable :: Aug(:,:)

    integer(ikind) :: n
    integer(ikind) :: i, j, k

    real(rkind) :: factor, sum

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

        sum = 0.0_rkind

        do j = i+1, n
            sum = sum + Aug(i,j) * x(j)
        end do

        x(i) = (Aug(i,n+1) - sum) / Aug(i,i)

    end do

    print *, "Solution vector:"
    write(*,'(100(f12.6,1x))') x

end subroutine

end module
