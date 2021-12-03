PROGRAM assembly_sim
    implicit none
    integer, parameter :: parts = 20
    integer, parameter :: engines = 20
    real :: t
    real, dimension(0:parts) :: next_part
    real, dimension(0:engines) :: build_times
    logical, dimension(0:parts) :: making_part
    integer :: arrived
    integer, dimension(parts) :: inventory
    integer, parameter :: max_inventory = 5
    real :: next_arrival
    real, parameter :: lambda = 0.2
    integer, parameter :: build_time = 3
    integer :: n
    integer :: i


    t = 0
    arrived = 0
    inventory = 0
    build_times = 0
    n = 1

    do i = 1, parts, 1
        next_part(i) = gen_uniform(3.0, 5.0)
    enddo

    making_part = .true.

    making_part(0) = .false.
    next_part(0) = 1e10

    do while (n .lt. engines)
        t = minval(next_part)
        ! Inventory management
        ! If a part is ready, add it to inventory.
        ! If not making a part and we have space, start making a new part.
        do i = 1, parts, 1
            if (next_part(i) .le. t) then
                inventory(i) = inventory(i) + 1
                making_part(i) = .false.
            endif

            if (.not. making_part(i)) then
                if (inventory(i) .lt. max_inventory) then
                    next_part(i) = t + gen_uniform(3.0, 5.0)
                    making_part(i) = .true.
                else
                    next_part(i) = 1e10
                endif
            endif
        enddo

        write(*,*) t
        write(*,"(20F5.1) ") (next_part(i), i = 1, parts)
        write(*,"(20I5) ") (inventory(i), i = 1, parts)

        ! Engine management
        ! If we have all the parts, start an engine build
        ! If an engine build is complete, harvest it
        if (.not. making_part(0)) then
            if (all(inventory .ge. 1)) then
                inventory = inventory - 1
                build_times(n) = t - build_times(n-1)
                next_part(0) = t + build_time
                making_part(0) = .true.
                write(*,*) "Built engine at time ", t
            endif
        else
            if (next_part(0) .le. t) then
                making_part(0) = .false.
                next_part(0) = 1e10
                n = n + 1
            endif
        endif
    enddo
    build_times(n) = t - build_times(n-1)
    write(*,*) build_times


    contains
    real function gen_uniform(a, b)
        real, intent(in) :: a
        real, intent(in) :: b
        real :: U
        real :: catastrophe
        call random_number(U)
        call random_number(catastrophe)
        if (catastrophe < 0.01) then
            gen_uniform = 2 * (a + (b - a) * U)
        else
            gen_uniform = a + (b - a) * U
        endif
    end function

    real function gen_Ts(s, lambda)
        real, intent(in) :: s
        real, intent(in) :: lambda
        real :: U
        call random_number(U)
        gen_Ts = s - log(U) / lambda
    end function

    integer function factorial(n)
        integer, intent(in) :: n
        integer :: acc, i
        
        acc = 1
        do i = 1, n, 1
            acc = acc * i
        enddo
        factorial = acc
    end function

    real function poisson(lambda, k)
        real, intent(in) :: lambda
        integer, intent(in) :: k

        !poisson = (lambda ** k * exp(-1 * lambda)) / factorial(k)
        poisson = exp(k * log(lambda) - lambda - log(real(factorial(k))))
    end function
end program
