PROGRAM single_server_queue
    implicit none
    real :: t
    real, dimension(10) :: arrival_times
    real, dimension(10) :: departure_times
    integer :: arrived
    integer :: departed
    integer :: customers
    real :: next_arrival
    real :: next_departure
    real, parameter :: lambda = 0.2
    integer :: service_time = 2


    customers = 0
    t = 0
    arrived = 0
    departed = 0
    arrival_times(1) = gen_Ts(0.0, lambda)
    next_arrival = arrival_times(1)
    next_departure = 1e10

    do while (departed .lt. 10)
        if (next_arrival .le. next_departure) then
            t = next_arrival
            arrived = arrived + 1
            customers = customers + 1
            next_arrival = gen_Ts(t, lambda)
            if (customers .eq. 1) then
                next_departure = t + service_time
            end if
            arrival_times(arrived) = t
        else if (next_departure .lt. next_arrival) then
            t = next_departure
            customers = customers - 1
            departed = departed + 1
            if (customers .eq. 0) then
                next_departure = 1e10
            end if
            departure_times(departed) = t
        end if
    end do
    departure_times(departed) = t
    write(*,*) arrival_times
    write(*,*) departure_times

    contains
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
