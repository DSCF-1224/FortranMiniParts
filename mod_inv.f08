! ==================================================================================================================================
! 
! # file created #
! 2020/09/26
! 
! ==================================================================================================================================

module mod_inv

    ! <module>s to import
    use, intrinsic :: iso_fortran_env



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility of the <subroutine>s and <function>s in this <module>

    ! kind: function
    private :: inv_R32
    private :: inv_R64

    ! kind: interface
    public :: inv



    ! <interface>s for this <module>

    interface inv
        module procedure :: inv_R32
        module procedure :: inv_R64
    end interface inv



    ! contained <subroutine>s and <function>s are below
    contains



    pure function inv_R32 (x) result(inv)

        ! arguments for this <function>
        real(REAL32), intent(in) :: x

        ! return value of this <function>
        real(REAL32) :: inv

        inv = 1.0_REAL32 / x

    end function inv_R32



    pure function inv_R64 (x) result(inv)

        ! arguments for this <function>
        real(REAL64), intent(in) :: x

        ! return value of this <function>
        real(REAL64) :: inv

        inv = 1.0_REAL64 / x

    end function inv_R64

end module mod_inv

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
