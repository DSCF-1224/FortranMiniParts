! ==================================================================================================================================
! 
! # file created #
! 2021/09/10
! 
! # objective #
! Implementation of subroutines equivalent to the C/C++ increment operator.
! 
! ==================================================================================================================================

module mod_increment

    ! <module>s to import
    use, intrinsic :: iso_fortran_env


    ! require all variables to be explicitly declared
    implicit none



    ! accessibility of the <subroutine>s and <function>s in this <module>

    ! kind: default
    private

    ! kind: interface
    public :: decrement
    public :: increment



    ! <interface>s for this <module>

    interface decrement
        module procedure :: decrement_default_int32
        module procedure :: decrement_specified_int32
        module procedure :: decrement_default_int64
        module procedure :: decrement_specified_int64
    end interface decrement

    interface increment
        module procedure :: increment_default_int32
        module procedure :: increment_specified_int32
        module procedure :: increment_default_int64
        module procedure :: increment_specified_int64
    end interface increment


    ! contained <subroutine>s and <function>s are below
    contains


    elemental subroutine decrement_default_int32 ( i )

        ! argument(s) for this <subroutine>
        integer(INT32) , intent(inout) :: i

        call decrement( i = i , inc = 1_INT32 )
        return

    end subroutine decrement_default_int32


    elemental subroutine decrement_specified_int32 ( i , inc )

        ! argument(s) for this <subroutine>
        integer(INT32) , intent(inout) :: i
        integer(INT32) , intent(in)    :: inc

        i = i - inc
        return

    end subroutine decrement_specified_int32


    elemental subroutine decrement_default_int64 ( i )

        ! argument(s) for this <subroutine>
        integer(INT64) , intent(inout) :: i

        call decrement( i = i , inc = 1_INT64 )
        return

    end subroutine decrement_default_int64


    elemental subroutine decrement_specified_int64 ( i , inc )

        ! argument(s) for this <subroutine>
        integer(INT64) , intent(inout) :: i
        integer(INT64) , intent(in)    :: inc

        i = i - inc
        return

    end subroutine decrement_specified_int64


    elemental subroutine increment_default_int32 ( i )

        ! argument(s) for this <subroutine>
        integer(INT32) , intent(inout) :: i

        call increment( i = i , inc = 1_INT32 )
        return

    end subroutine increment_default_int32


    elemental subroutine increment_specified_int32 ( i , inc )

        ! argument(s) for this <subroutine>
        integer(INT32) , intent(inout) :: i
        integer(INT32) , intent(in)    :: inc

        i = i + inc
        return

    end subroutine increment_specified_int32


    elemental subroutine increment_default_int64 ( i )

        ! argument(s) for this <subroutine>
        integer(INT64) , intent(inout) :: i

        call increment( i = i , inc = 1_INT64 )
        return

    end subroutine increment_default_int64


    elemental subroutine increment_specified_int64 ( i , inc )

        ! argument(s) for this <subroutine>
        integer(INT64) , intent(inout) :: i
        integer(INT64) , intent(in)    :: inc

        i = i + inc
        return

    end subroutine increment_specified_int64

end module mod_increment

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
