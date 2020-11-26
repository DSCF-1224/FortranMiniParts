! ==================================================================================================================================
! 
! # file created #
! 2020/11/27
! 
! # reference #
! https://gcc.gnu.org/onlinedocs/gfortran/SYSTEM_005fCLOCK.html
! 
! ==================================================================================================================================

module mod_system_clock

    ! <module>s to import
    use, intrinsic :: iso_fortran_env



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility of the <subroutine>s and <function>s in this <module>

    ! kind: type
    public :: typ_system_clock

    ! kind: function
    public :: diff_system_clock_raw
    public :: diff_system_clock_sec

    ! kind: subroutine
    private :: system_clock_fetch

    ! kind: interface
    public :: system_clock



    ! <type>s for this <module>
    type typ_system_clock
        integer(INT64), private :: count      ! a processor clock since an unspecified time in the past modulo COUNT_MAX
        integer(INT64), private :: count_rate ! the number of clock ticks per second
        integer(INT64), private :: count_max
    end type typ_system_clock



    ! <interface>s for this <module>
    interface system_clock
        module procedure :: system_clock_fetch
    end interface system_clock



    ! contained <subroutine>s and <function>s are below
    contains



    ! [objective]
    ! calculate the difference between two return values from an intrinsic subroutine <system_clock>
    pure function diff_system_clock_raw (clock_begin, clock_end) result(diff)

        ! arguments for this <function>
        type(typ_system_clock), intent(in) :: clock_begin
        type(typ_system_clock), intent(in) :: clock_end

        ! return value of this <function>
        integer(INT64) :: diff

        ! STEP.01
        ! calculate the difference between the `count`
        diff = clock_end%count - clock_begin%count

        ! STEP.02
        ! correct the diffetence the `count`
        if (diff .lt. 0_INT64) then
            diff = clock_end%count_max + diff + 1_INT64
        end if

    end function diff_system_clock_raw



    ! [objective]
    ! calculate the difference between two return values from an intrinsic subroutine <system_clock>
    ! transform the unit to `second`
    pure function diff_system_clock_sec (clock_begin, clock_end) result(diff)

        ! arguments for this <function>
        type(typ_system_clock), intent(in) :: clock_begin
        type(typ_system_clock), intent(in) :: clock_end

        ! return value of this <function>
        real(REAL64) :: diff

        diff = real( diff_system_clock_raw(clock_begin, clock_end), kind= REAL64 ) / clock_end%count_rate

    end function diff_system_clock_sec



    ! [objective]
    ! store the all return values from an intrinsic subroutine <system_clock>
    subroutine system_clock_fetch (object)

        ! arguments for this <subroutine>
        type(typ_system_clock), intent(out) :: object

        ! STEP.01
        ! call an intrinsic subroutine <system_clock> to get current time
        call system_clock(count= object%count, count_rate= object%count_rate, count_max= object%count_max)

        ! STEP.END
        return

    end subroutine system_clock_fetch

end module mod_system_clock

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
