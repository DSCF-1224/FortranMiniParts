! ==================================================================================================================================
! 
! # file created #
! 2020/10/25
! 
! ==================================================================================================================================

module mod_ceiling

    ! <module>s to import
    use, intrinsic :: iso_fortran_env

    ! require all variables to be explicitly declared
    implicit none



    ! accessibility of the <subroutine>s and <function>s in this <module>
    
    ! kind: function
    private :: availability_ceiling_pow2_int32
    private :: ceiling_pow2_int32
    
    ! kind: interface
    public :: availability_ceiling_pow2
    public :: ceiling_pow2



    ! <interface>s for this <module>
    interface availability_ceiling_pow2
        module procedure :: availability_ceiling_pow2_int32
    end interface availability_ceiling_pow2

    interface ceiling_pow2
        module procedure :: ceiling_pow2_int32
    end interface ceiling_pow2



    ! contained <subroutine>s and <function>s are below
    contains

    pure function availability_ceiling_pow2_int32 (i) result(availability)

        ! arguments for this <function>
        integer(INT32), intent(in) :: i

        ! return value of this <function>
        logical :: availability

        if (i .lt. 0_INT32) then
            availability = .false.
            return
        else if ( trailz(i) .le. 30_INT32 ) then
            availability = .true.
            return
        else
            availability = .false.
            return
        end if

    end function availability_ceiling_pow2_int32



    pure function ceiling_pow2_int32 (i) result(ceiling)

        ! arguments for this <function>
        integer(INT32), intent(in) :: i

        ! return value of this <function>
        integer(INT32) :: ceiling

        ! variables for this <function>
        integer(INT32) :: buffer_trailz

        select case (i)
            case(1_INT32:)

                buffer_trailz = trailz(i)

                if ( buffer_trailz .le. 30_INT32 ) then
                    if (2_INT32 ** buffer_trailz .eq. i) then
                        ceiling = i
                        return
                    else
                        ceiling = 2_INT32 ** ( 32_INT32 - leadz(i) )
                        return
                    end if
                else
                    ceiling = huge(i)
                    return
                end if

            case default
                ceiling = 0_INT32
        end select
        
    end function ceiling_pow2_int32

end module mod_ceiling

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
