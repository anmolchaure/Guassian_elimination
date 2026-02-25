program test_gem
    use linalg
    implicit none

        real(8), allocatable :: A(:,:), b(:), x(:)
        integer :: n

        call read_system(A, b, n, "matrix.txt")

        call gem(A, b, x)

end program
