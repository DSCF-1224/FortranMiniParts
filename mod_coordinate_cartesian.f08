! ==================================================================================================================================
! 
! # file created #
! 2020/09/20
! 
! ==================================================================================================================================

module mod_coordinate_cartesian

    ! <module>s to import
    use, intrinsic :: iso_fortran_env



    ! require all variables to be explicitly declared
    implicit none


    ! accessibility of the <subroutine>s and <function>s in this <module>

    ! kind: type
    public :: typ_coordinate_cartesian_2D_R32
    public :: typ_coordinate_cartesian_2D_R64
    public :: typ_coordinate_cartesian_3D_R32
    public :: typ_coordinate_cartesian_3D_R64

    ! kind: function
    private :: add_2D_R32
    private :: add_2D_R64
    private :: add_3D_R32
    private :: add_3D_R64

    private :: dot_product_2D_R32
    private :: dot_product_2D_R64
    private :: dot_product_3D_R32
    private :: dot_product_3D_R64

    private :: norm_R32
    private :: norm_R64

    private :: norm2_R32
    private :: norm2_R64

    private :: minus_2D_R32
    private :: minus_2D_R64
    private :: minus_3D_R32
    private :: minus_3D_R64

    private :: prod_2D_sclr_R32
    private :: prod_2D_sclr_R64
    private :: prod_3D_sclr_R32
    private :: prod_3D_sclr_R64
    private :: prod_sclr_2D_R32
    private :: prod_sclr_2D_R64
    private :: prod_sclr_3D_R32
    private :: prod_sclr_3D_R64

    private :: sub_2D_R32
    private :: sub_2D_R64
    private :: sub_3D_R32
    private :: sub_3D_R64

    ! kind: interface: operator
    public :: operator(+)
    public :: operator(-)
    public :: operator(*)

    ! kind: interface: function
    public :: norm2
    public :: dot_product



    ! <type>s for this <module>

    type typ_coordinate_cartesian_2D_R32
        real(REAL32), public :: x
        real(REAL32), public :: y
    end type typ_coordinate_cartesian_2D_R32

    type typ_coordinate_cartesian_2D_R64
        real(REAL64), public :: x
        real(REAL64), public :: y
    end type typ_coordinate_cartesian_2D_R64

    type, extends(typ_coordinate_cartesian_2D_R32) :: typ_coordinate_cartesian_3D_R32
        real(REAL32), public :: z
    end type typ_coordinate_cartesian_3D_R32

    type, extends(typ_coordinate_cartesian_2D_R64) :: typ_coordinate_cartesian_3D_R64
        real(REAL64), public :: z
    end type typ_coordinate_cartesian_3D_R64




    ! <interface>s for this <module>
    interface operator(+)

        ! binary operator
        module procedure :: add_2D_R32
        module procedure :: add_2D_R64
        module procedure :: add_3D_R32
        module procedure :: add_3D_R64

    end interface operator(+)

    interface operator(-)

        ! Unary operator
        module procedure :: minus_2D_R32
        module procedure :: minus_2D_R64
        module procedure :: minus_3D_R32
        module procedure :: minus_3D_R64

        ! Binary operator
        module procedure :: sub_2D_R32
        module procedure :: sub_2D_R64
        module procedure :: sub_3D_R32
        module procedure :: sub_3D_R64

    end interface operator(-)

    interface operator(*)
        module procedure :: prod_2D_sclr_R32
        module procedure :: prod_2D_sclr_R64
        module procedure :: prod_3D_sclr_R32
        module procedure :: prod_3D_sclr_R64
        module procedure :: prod_sclr_2D_R32
        module procedure :: prod_sclr_2D_R64
        module procedure :: prod_sclr_3D_R32
        module procedure :: prod_sclr_3D_R64
    end interface operator(*)

    interface dot_product
        module procedure :: dot_product_2D_R32
        module procedure :: dot_product_2D_R64
        module procedure :: dot_product_3D_R32
        module procedure :: dot_product_3D_R64
    end interface dot_product

    interface norm
        module procedure :: norm_R32
        module procedure :: norm_R64
    end interface norm

    interface norm2
        module procedure :: norm2_R32
        module procedure :: norm2_R64
    end interface norm2



    ! contained <subroutine>s and <function>s are below
    contains



    pure function add_2D_R32 (coordinate_a, coordinate_b) result(coordinate_add)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R32) :: coordinate_add

        coordinate_add%x = coordinate_a%x + coordinate_b%x
        coordinate_add%y = coordinate_a%y + coordinate_b%y

    end function add_2D_R32



    pure function add_2D_R64 (coordinate_a, coordinate_b) result(coordinate_add)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R64) :: coordinate_add

        coordinate_add%x = coordinate_a%x + coordinate_b%x
        coordinate_add%y = coordinate_a%y + coordinate_b%y

    end function add_2D_R64



    pure function add_3D_R32 (coordinate_a, coordinate_b) result(coordinate_add)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R32) :: coordinate_add

        coordinate_add%x = coordinate_a%x + coordinate_b%x
        coordinate_add%y = coordinate_a%y + coordinate_b%y
        coordinate_add%z = coordinate_a%z + coordinate_b%z

    end function add_3D_R32



    pure function add_3D_R64 (coordinate_a, coordinate_b) result(coordinate_add)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R64) :: coordinate_add

        coordinate_add%x = coordinate_a%x + coordinate_b%x
        coordinate_add%y = coordinate_a%y + coordinate_b%y
        coordinate_add%z = coordinate_a%z + coordinate_b%z

    end function add_3D_R64



    pure function dot_product_2D_R32 (coordinate_a, coordinate_b) result(product)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        real(REAL32) :: product

        product = coordinate_a%x * coordinate_b%x + coordinate_a%y * coordinate_b%y

    end function dot_product_2D_R32



    pure function dot_product_2D_R64 (coordinate_a, coordinate_b) result(product)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        real(REAL64) :: product

        product = coordinate_a%x * coordinate_b%x + coordinate_a%y * coordinate_b%y

    end function dot_product_2D_R64



    pure function dot_product_3D_R32 (coordinate_a, coordinate_b) result(product)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        real(REAL32) :: product

        product = coordinate_a%x * coordinate_b%x + coordinate_a%y * coordinate_b%y + coordinate_a%z * coordinate_b%z

    end function dot_product_3D_R32



    pure function dot_product_3D_R64 (coordinate_a, coordinate_b) result(product)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        real(REAL64) :: product

        product = coordinate_a%x * coordinate_b%x + coordinate_a%y * coordinate_b%y + coordinate_a%z * coordinate_b%z

    end function dot_product_3D_R64



    pure function minus_2D_R32 (coordinate) result(coordinate_minus)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R32) :: coordinate_minus

        coordinate_minus%x = - coordinate%x
        coordinate_minus%y = - coordinate%y

    end function minus_2D_R32



    pure function minus_2D_R64 (coordinate) result(coordinate_minus)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R64) :: coordinate_minus

        coordinate_minus%x = - coordinate%x
        coordinate_minus%y = - coordinate%y

    end function minus_2D_R64



    pure function minus_3D_R32 (coordinate) result(coordinate_minus)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R32) :: coordinate_minus

        coordinate_minus%x = - coordinate%x
        coordinate_minus%y = - coordinate%y
        coordinate_minus%z = - coordinate%z

    end function minus_3D_R32



    pure function minus_3D_R64 (coordinate) result(coordinate_minus)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R64) :: coordinate_minus

        coordinate_minus%x = - coordinate%x
        coordinate_minus%y = - coordinate%y
        coordinate_minus%z = - coordinate%z

    end function minus_3D_R64



    pure function norm_R32 (coordinate) result(norm)

        ! arguments for this <function>
        class(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate

        ! return value of this <function>
        real(REAL32) :: norm

        norm = sqrt( norm2(coordinate) )

    end function norm_R32



    pure function norm_R64 (coordinate) result(norm)

        ! arguments for this <function>
        class(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate

        ! return value of this <function>
        real(REAL64) :: norm

        norm = sqrt( norm2(coordinate) )

    end function norm_R64



    pure function norm2_R32 (coordinate) result(norm2)

        ! arguments for this <function>
        class(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate

        ! return value of this <function>
        real(REAL32) :: norm2

        norm2 = dot_product(coordinate, coordinate)

    end function norm2_R32



    pure function norm2_R64 (coordinate) result(norm2)

        ! arguments for this <function>
        class(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate

        ! return value of this <function>
        real(REAL64) :: norm2

        norm2 = dot_product(coordinate, coordinate)

    end function norm2_R64



    pure function prod_2D_sclr_R32 (coordinate_left, scalar_right) result(coordinate_prod)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate_left
        real(REAL32),                          intent(in) :: scalar_right

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R32) :: coordinate_prod

        coordinate_prod%x = coordinate_left%x * scalar_right
        coordinate_prod%y = coordinate_left%y * scalar_right

    end function prod_2D_sclr_R32



    pure function prod_2D_sclr_R64 (coordinate_left, scalar_right) result(coordinate_prod)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate_left
        real(REAL64),                          intent(in) :: scalar_right

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R64) :: coordinate_prod

        coordinate_prod%x = coordinate_left%x * scalar_right
        coordinate_prod%y = coordinate_left%y * scalar_right

    end function prod_2D_sclr_R64



    pure function prod_3D_sclr_R32 (coordinate_left, scalar_right) result(coordinate_prod)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate_left
        real(REAL32),                          intent(in) :: scalar_right

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R32) :: coordinate_prod

        coordinate_prod%x = coordinate_left%x * scalar_right
        coordinate_prod%y = coordinate_left%y * scalar_right
        coordinate_prod%z = coordinate_left%z * scalar_right

    end function prod_3D_sclr_R32



    pure function prod_3D_sclr_R64 (coordinate_left, scalar_right) result(coordinate_prod)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate_left
        real(REAL64),                          intent(in) :: scalar_right

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R64) :: coordinate_prod

        coordinate_prod%x = coordinate_left%x * scalar_right
        coordinate_prod%y = coordinate_left%y * scalar_right
        coordinate_prod%z = coordinate_left%z * scalar_right

    end function prod_3D_sclr_R64



    pure function prod_sclr_2D_R32 (scalar_left, coordinate_right) result(coordinate_prod)

        ! arguments for this <function>
        real(REAL32),                          intent(in) :: scalar_left
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate_right

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R32) :: coordinate_prod

        coordinate_prod%x = scalar_left * coordinate_right%x
        coordinate_prod%y = scalar_left * coordinate_right%y

    end function prod_sclr_2D_R32



    pure function prod_sclr_2D_R64 (scalar_left, coordinate_right) result(coordinate_prod)

        ! arguments for this <function>
        real(REAL64),                          intent(in) :: scalar_left
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate_right

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R64) :: coordinate_prod

        coordinate_prod%x = scalar_left * coordinate_right%x
        coordinate_prod%y = scalar_left * coordinate_right%y

    end function prod_sclr_2D_R64



    pure function prod_sclr_3D_R32 (scalar_left, coordinate_right) result(coordinate_prod)

        ! arguments for this <function>
        real(REAL32),                          intent(in) :: scalar_left
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate_right

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R32) :: coordinate_prod

        coordinate_prod%x = scalar_left * coordinate_right%x
        coordinate_prod%y = scalar_left * coordinate_right%y
        coordinate_prod%z = scalar_left * coordinate_right%z

    end function prod_sclr_3D_R32



    pure function prod_sclr_3D_R64 (scalar_left, coordinate_right) result(coordinate_prod)

        ! arguments for this <function>
        real(REAL64),                          intent(in) :: scalar_left
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate_right

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R64) :: coordinate_prod

        coordinate_prod%x = scalar_left * coordinate_right%x
        coordinate_prod%y = scalar_left * coordinate_right%y
        coordinate_prod%z = scalar_left * coordinate_right%z

    end function prod_sclr_3D_R64



    pure function sub_2D_R32 (coordinate_a, coordinate_b) result(coordinate_sub)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R32) :: coordinate_sub

        coordinate_sub = - coordinate_b + coordinate_a

    end function sub_2D_R32



    pure function sub_2D_R64 (coordinate_a, coordinate_b) result(coordinate_sub)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R64) :: coordinate_sub

        coordinate_sub = - coordinate_b + coordinate_a

    end function sub_2D_R64



    pure function sub_3D_R32 (coordinate_a, coordinate_b) result(coordinate_sub)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R32) :: coordinate_sub

        coordinate_sub = - coordinate_b + coordinate_a

    end function sub_3D_R32



    pure function sub_3D_R64 (coordinate_a, coordinate_b) result(coordinate_sub)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R64) :: coordinate_sub

        coordinate_sub = - coordinate_b + coordinate_a

    end function sub_3D_R64


end module mod_coordinate_cartesian

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
