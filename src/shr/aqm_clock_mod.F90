module aqm_clock_mod

  use aqm_rc_mod
  use aqm_types_mod, only : AQM_KIND_R8

  implicit none

  type aqm_clock_type
    integer            :: yy    = 0                  ! year
    integer            :: mm    = 0                  ! month
    integer            :: dd    = 0                  ! day
    integer            :: h     = 0
    integer            :: m     = 0
    integer            :: s     = 0
    integer            :: tz    = 0
    integer            :: julday = 0                 ! julian day
    integer            :: advanceCount = 0                  ! current time step
    integer            :: numphr       = 0                  ! # of time steps/hr
    real(AQM_KIND_R8)  :: dts          = 0._AQM_KIND_R8    ! model time step (seconds)
    character(len=12)  :: tStamp = ""                 ! timestamp
  end type aqm_clock_type

  private

  public :: aqm_clock_type

  public :: aqm_clock_set
  public :: aqm_clock_get

contains

  subroutine aqm_clock_set(clock, julday, yy, mm, dd, h, m, s, tz, &
    advanceCount, dts, rc)
    type(aqm_clock_type),        intent(out) :: clock
    integer,            optional, intent(in)  :: julday
    integer,            optional, intent(in)  :: yy, mm, dd, h, m, s, tz
    integer,            optional, intent(in)  :: advanceCount
    real(AQM_KIND_R8),  optional, intent(in)  :: dts
    integer,            optional, intent(out) :: rc

    ! -- local variables

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (present(julday)) clock % julday = julday
    if (present(yy))     clock % yy     = yy
    if (present(mm))     clock % mm     = mm
    if (present(dd))     clock % dd     = dd
    if (present(h))      clock % h      = h
    if (present(m))      clock % m      = m
    if (present(s))      clock % s      = s
    if (present(tz))     clock % tz     = tz
    if (present(dts))    clock % dts    = dts
    if (present(advanceCount)) clock % advanceCount = advanceCount
    if (clock % dts > 0._AQM_KIND_R8) &
      clock % numphr = nint(3600._AQM_KIND_R8/clock % dts)
    write(clock % tStamp, '(i4.4,4i2.2)') &
      clock % yy, clock % mm, clock % dd, &
      clock % h,  clock % m
    
  end subroutine aqm_clock_set


  subroutine aqm_clock_get(clock, julday, yy, mm, dd, h, m, s, tz, &
    dts, advanceCount, tStamp, rc)
    type(aqm_clock_type),        intent(in)  :: clock
    integer,            optional, intent(out) :: julday
    integer,            optional, intent(out) :: yy, mm, dd, h, m, s, tz
    real(AQM_KIND_R8),  optional, intent(out) :: dts
    integer,            optional, intent(out) :: advanceCount
    character(len=12),  optional, intent(out) :: tstamp
    integer,            optional, intent(out) :: rc

    ! -- local variables

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (present(julday)) julday = clock % julday
    if (present(yy))     yy     = clock % yy
    if (present(mm))     mm     = clock % mm
    if (present(dd))     dd     = clock % dd
    if (present(h))      h      = clock % h
    if (present(m))      m      = clock % m
    if (present(s))      s      = clock % s
    if (present(tz))     tz     = clock % tz
    if (present(dts))    dts    = clock % dts
    if (present(advanceCount)) advanceCount = clock % advanceCount
    if (present(tStamp)) tStamp = clock % tStamp
    
  end subroutine aqm_clock_get

end module aqm_clock_mod
