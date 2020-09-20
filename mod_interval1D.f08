! ==================================================================================================================================
! 
! # file created #
! 2020/09/20
! 
! # tested compiler #
! gcc version 9.3.0 (Ubuntu 9.3.0-10ubuntu2)
! 
! ==================================================================================================================================

module mod_interval1D

    ! <module>s to import
    use, intrinsic :: iso_fortran_env



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility of the <subroutine>s and <function>s in this <module>

    ! kind: type
    public :: typ_interval1d_R32
    public :: typ_interval1d_R64

    ! kind: function
    private :: delta_R32
    private :: delta_R64

    private :: midpoint_R32
    private :: midpoint_R64

    private :: width_R32
    private :: width_R64

    ! kind: interface
    public :: delta
    public :: midpoint
    public :: width



    ! <type>s for this <module>
    type typ_interval1d_R32
        real(REAL32), public :: lower
        real(REAL32), public :: upper
    end type typ_interval1d_R32

    type typ_interval1d_R64
        real(REAL64), public :: lower
        real(REAL64), public :: upper
    end type typ_interval1d_R64



    ! <interface>s for this <module>
    
    interface delta
        module procedure :: delta_R32
        module procedure :: delta_R64
    end interface delta

    interface midpoint
        module procedure :: midpoint_R32
        module procedure :: midpoint_R64
    end interface midpoint
    
    interface width
        module procedure :: width_R32
        module procedure :: width_R64
    end interface width



    ! contained <subroutine>s and <function>s are below
    contains



    pure function delta_R32 (interval) result(delta)

        ! arguments for this <function>
        type(typ_interval1d_R32), intent(in) :: interval

        ! return value of this <function>
        real(REAL32) :: delta

        delta = interval%upper - interval%lower

    end function delta_R32



    pure function delta_R64 (interval) result(delta)

        ! arguments for this <function>
        type(typ_interval1d_R64), intent(in) :: interval

        ! return value of this <function>
        real(REAL64) :: delta

        delta = interval%upper - interval%lower

    end function delta_R64



    pure function midpoint_R32 (interval) result(midpoint)

        ! arguments for this <function>
        type(typ_interval1d_R32), intent(in) :: interval

        ! return value of this <function>
        real(REAL32) :: midpoint

        midpoint = 0.5_REAL32 * (interval%lower + interval%upper)

    end function midpoint_R32



    pure function midpoint_R64 (interval) result(midpoint)

        ! arguments for this <function>
        type(typ_interval1d_R64), intent(in) :: interval

        ! return value of this <function>
        real(REAL64) :: midpoint

        midpoint = 0.5_REAL64 * (interval%lower + interval%upper)

    end function midpoint_R64



    pure function width_R32 (interval) result(width)

        ! arguments for this <function>
        type(typ_interval1d_R32), intent(in) :: interval

        ! return value of this <function>
        real(REAL32) :: width

        width = abs( delta(interval) )

    end function width_R32



    pure function width_R64 (interval) result(width)

        ! arguments for this <function>
        type(typ_interval1d_R64), intent(in) :: interval

        ! return value of this <function>
        real(REAL64) :: width

        width = abs( delta(interval) )

    end function width_R64

end module mod_interval1D

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
