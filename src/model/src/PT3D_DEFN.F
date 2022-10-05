      MODULE PT3D_DEFN
      
      USE NETCDF
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
      
      IMPLICIT NONE

      LOGICAL,              SAVE :: PT3DEMIS   ! flag in-lining plume rise
      INTEGER,              SAVE :: NPTGRPS    ! no. pt src input file groups
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_PT( :,:,:,: ) ! 3D pt src non-PM emis
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_PT_FIRE( :,:,:,: ) ! 3D pt src non-PM emis
      REAL,    ALLOCATABLE, SAVE :: PMEMIS_PT( :,:,:,: ) ! 3D pt src PM emis
      REAL,    ALLOCATABLE, SAVE :: VFRAC( :,:,: ) ! vertical fraction
      REAL,    ALLOCATABLE, SAVE :: BUFFER( : ) ! emission buffer

      PUBLIC PT3DEMIS, NPTGRPS, VDEMIS_PT, VDEMIS_PT_FIRE, PMEMIS_PT,
     &       PT3D_INIT, GET_PT3D_EMIS
      PRIVATE

C Stack emissions for sources within domain
      INTEGER,              SAVE :: EMLYRS       ! no. of emis layers
      INTEGER,              SAVE :: PM_EMLYRS    ! no. of emis layers for PM

C Gas-phase Namelist emissions factors
      REAL,    ALLOCATABLE, SAVE :: EMIS_FAC( : )  ! per species scaling factor

      CHARACTER( 240 )           :: XMSG = ' '

C Emission type
      CHARACTER( * ),  PARAMETER :: ETYPE = 'gbbepx'

C Species name for Fire Radiative Power
      CHARACTER( * ),  PARAMETER :: EMFRP = 'FRP'

      CONTAINS

C=======================================================================

         FUNCTION PT3D_INIT ( N_SPC_EMIS, EMLAYS, JDATE, JTIME, TSTEP )
     &                       RESULT ( SUCCESS )

         USE AQM_EMIS_MOD
         USE GRID_CONF                  ! horizontal & vertical domain specifications
         USE CGRID_SPCS                 ! CGRID mechanism species
         USE STK_EMIS, ONLY : STKSPC    ! hourly point source emissions
         USE PTMAP                      ! defines pt src species mapping to VDEMIS* arrays
         USE UTILIO_DEFN

         IMPLICIT NONE

C Includes:
C        INCLUDE SUBST_FILES_ID  ! file name parameters (for CTM_PT3D_DIAG)

C Arguments:
         INTEGER, INTENT( IN ) :: N_SPC_EMIS ! total no. of model emissions species
         INTEGER, INTENT( IN ) :: EMLAYS     ! number of emissions layers
         INTEGER, INTENT( IN ) :: JDATE      ! Julian date (YYYYDDD)
         INTEGER, INTENT( IN ) :: JTIME      ! time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP      ! output time step
         LOGICAL          SUCCESS

C External Functions:
         INTEGER,         EXTERNAL :: SETUP_LOGDEV

C Parameters:

C Local Variables:
         CHARACTER( 16 ), SAVE :: CTM_PT3DEMIS = 'CTM_PT3DEMIS    ' ! env var for in-line
                                                                    ! 3d pt src emis
         CHARACTER( 16 )       :: PNAME = 'PT3D_INIT       ' ! procedure name
         CHARACTER( 16 )       :: VNAME    ! variable name buffer
         CHARACTER( 16 ), SAVE, ALLOCATABLE :: STKGNAME( : ) ! stack groups file name

         INTEGER          IOS            ! i/o and allocate memory status

         INTEGER          LOGDEV

         INTEGER IDX
         INTEGER N, NSPC, NSPC1, NSPC2, NSPC3
         INTEGER S, S_OFFSET, V

         INTEGER, ALLOCATABLE :: MAP( : )

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = SETUP_LOGDEV()

C In-line 3D point source emissions?

         PT3DEMIS = ENVYN( 'CTM_PT3DEMIS',
     &                   'Flag for in-line 3d point source emissions',
     &                   .FALSE., IOS )

         IF ( PT3DEMIS ) THEN
            XMSG = 'Using in-line 3d point source emissions option'
            CALL M3MSG2( XMSG )
         ELSE
            RETURN
         END IF

C check if fire emissions are being provided

         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN

C get number of different file groups (sectors)

         NPTGRPS = 0

C create point source internal array

         ALLOCATE( STKSPC( NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'STKSPC', PNAME )

C set number of emissions layers depending on whether plumerise is on

         EMLYRS    = NLAYS
         PM_EMLYRS = NLAYS
         
C get point source emission mapping

         IF ( .NOT. PTMAP_INIT( ) ) THEN
            XMSG = 'Could not get point source mappings'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

C allocate emission arrays

         ALLOCATE ( VDEMIS_PT( NCOLS,NROWS,EMLYRS,N_SPC_PTEM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VDEMIS_PT', PNAME )
         VDEMIS_PT = 0.0   ! array assignment

         ALLOCATE ( VDEMIS_PT_FIRE( NCOLS,NROWS,EMLYRS,N_SPC_PTEM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VDEMIS_PT_FIRE', PNAME )
         VDEMIS_PT_FIRE = 0.0   ! array assignment

         ALLOCATE ( PMEMIS_PT( NCOLS,NROWS,EMLYRS,N_SPC_PTPM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PMEMIS_PT', PNAME )
         PMEMIS_PT = 0.0   ! array

         ALLOCATE ( VFRAC( NCOLS,NROWS,EMLYRS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VFRAC', PNAME )
         VFRAC = 1.0   ! array

         ALLOCATE ( BUFFER( NCOLS * NROWS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'BUFFER', PNAME )
         BUFFER = 0.0   ! array

         SUCCESS = .TRUE.; RETURN

         END FUNCTION PT3D_INIT

C=======================================================================

         SUBROUTINE GET_PT3D_EMIS ( JDATE, JTIME, TSTEP )

! Revision History.
!     Aug 12, 15 D. Wong: added code to handle parallel I/O implementation

C-----------------------------------------------------------------------

C Time step part of laypoint

         USE AQM_EMIS_MOD
         USE AQM_FIRES_MOD
         USE AQM_RC_MOD
         use aqm_model_mod, only : aqm_config_type, aqm_state_type,
     &                      aqm_model_get, aqm_model_domain_get
         
	 USE RXNS_DATA, ONLY : MECHNAME !Get Chemical Mechanism Name
         USE GRID_CONF   ! horizontal & vertical domain specifications
         USE CGRID_SPCS  ! CGRID mechanism species
         USE AERO_DATA, ONLY : N_EMIS_PM, PMEM_MAP_NAME
         USE PTMAP       ! defines pt src species mapping to VDEMIS* arrays
         USE UTILIO_DEFN
!	 use esmf
!	 use nuopc
	 
         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_CONST     ! physical and mathematical constants
         INCLUDE SUBST_FILES_ID  ! file name parameters (for CTM_PT3D_DIAG)

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME
         INTEGER, INTENT( IN ) :: TSTEP( 3 )

C Parameters:

C External functions:
         INTEGER,         EXTERNAL :: SETUP_LOGDEV

C Local variables:
         CHARACTER( 16 ) :: PNAME = 'GET_PT3D_EMIS   '   ! procedure name
         CHARACTER( 16 ) :: VNAME    ! variable name buffer

         INTEGER          IOS        ! i/o and allocate memory status
         INTEGER          L, S, V    ! counters
         INTEGER          C, R, K, N, I, J
         INTEGER          LOCALRC

         LOGICAL       :: IS_NOT_NVPOA, SAVE_POC

         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER, SAVE :: LOGDEV
         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM
	 
	 REAL             TSTK       ! temperature at top of stack [K]
         REAL             TSUM       ! tmp layer frac sum for renormalizing
         REAL             WSTK       ! wind speed  at top of stack [m/s]
         REAL             ZBOT       ! plume bottom elevation [m]
         REAL             ZTOP       ! plume top    elevation [m]
         REAL             ZDIFF      ! ZTOP - ZBOT
         REAL             DDZ        ! 1 / ZDIFF
         REAL             ZPLM       ! plume centerline height above stack [m]
         REAL             USTMP      ! temp storage for ustar [m/s]
         REAL             HFLX       ! converted heat flux
         INTEGER          LBOT       ! layer containing plume bottom
         INTEGER          LTOP       ! layer containing plume top
         INTEGER          LPBL       ! first L: ZF(L) above mixing layer - ONLY for REPORT
         INTEGER          LSTK       ! first L: ZF(L) > STKHT
         REAL             LFRC       ! intermediate LFRAC
         character(len=NF90_MAX_NAME) :: path_in
	 
! 	 type(ESMF_VM)                          :: VM_ESMF	 
	 integer myrc, my_mpi_comm,my_ntasks, is, ie, js, je
	 character(200) :: aline
	 
	 real  zf0,zf1,mxzplm
	 integer, save :: ntimes,itstep,ncid,iddim_stack,nstack,idvar,nvars,idlists(200),
     &	   id_em_gc(200),id_em_pm(200),indx_gc(200),indx_pm(200), nvars_gc,nvars_pm,ndims,
     &     elemStart(2),elemCount(2),jstartdate,jstarttime
     
	 real,save :: distnear   ! search distrance in km, sqrt(0.5)*model_resolution in km
	 integer, save, allocatable, dimension (:) :: ixt, jyt
         real, save, allocatable, dimension (:) :: tlat,tlon,stkdm,stkht,tfrac,
     &	    stktk,stkve,stkflw,stk_ddzf,stk_pres,stk_dens,stk_qv,stk_ta,stk_wspd,stk_zf,stk_zh,stk_zstk,
     &      stk_presf,stk_zzf,stk_dthdz,stk_uwind,stk_vwind
         real, save, allocatable, dimension (:,:) :: stkemis, my_area
	 real, save, allocatable, dimension (:,:,:) :: uwind,vwind
c         type(aqm_config_type), pointer :: config
	 
         INTERFACE
           SUBROUTINE PREPLM( FIREFLG, EMLAYS, HMIX, HTS, PSFC, TS, DDZF, QV,
     &                        TA, UW, VW, ZH, ZF, PRES, LSTK, LPBL, TSTK,
     &                        WSTK, DTHDZ, WSPD )
             LOGICAL, INTENT( IN )  :: FIREFLG           ! .true. => processing fire source
             INTEGER, INTENT( IN )  :: EMLAYS		 ! no. emissions layers
             REAL,    INTENT( IN )  :: HMIX		 ! mixing height
             REAL,    INTENT( IN )  :: HTS		 ! stack height
             REAL,    INTENT( IN )  :: PSFC		 ! surface pressure
             REAL,    INTENT( IN )  :: TS		 ! surface temperature
             REAL,    INTENT( IN )  :: DDZF( : )	 ! 1/( zf(l) - zf(l-1) )
             REAL,    INTENT( IN )  :: QV  ( : )	 ! mixing ratio
             REAL,    INTENT( IN )  :: TA  ( : )	 ! absolute temperature
             REAL,    INTENT( IN )  :: UW  ( : )	 ! x-direction winds
             REAL,    INTENT( IN )  :: VW  ( : )	 ! y-direction winds
             REAL,    INTENT( IN )  :: ZH  ( : )	 ! layer center  height [m]
             REAL,    INTENT( IN )  :: ZF  ( : )	 ! layer surface height [m]
             REAL,    INTENT( IN )  :: PRES( 0: )	 ! pres at full layer hts (mod by YOJ)
             INTEGER, INTENT( OUT ) :: LSTK		 ! first L: ZF(L) > STKHT
             INTEGER, INTENT( OUT ) :: LPBL		 ! first L: ZF(L) > mixing layer
             REAL,    INTENT( OUT ) :: TSTK		 ! temperature @ top of stack [K]
             REAL,    INTENT( OUT ) :: WSTK		 ! wind speed @ top of stack [m/s]
             REAL,    INTENT( OUT ) :: DTHDZ( : )	 ! potential temp. grad. 
             REAL,    INTENT( OUT ) :: WSPD ( : )	 ! wind speed [m/s]
           END SUBROUTINE PREPLM
	   
	   SUBROUTINE PLMRIS( EMLAYS, LSTK, HFX, HMIX,
     &                        STKDM, STKHT, STKTK, STKVE,
     &                        TSTK, USTAR, DTHDZ, TA, WSPD,
     &                        ZF, ZH, ZSTK, WSTK, ZPLM )
             INTEGER, INTENT( IN )  :: EMLAYS        ! no. of emission layers
             INTEGER, INTENT( IN )  :: LSTK	     ! lyr of top of stack, = RADM's KSTK
             REAL,    INTENT( IN )  :: HFX	     ! sensible heat flux [m K/s]
             REAL,    INTENT( IN )  :: HMIX	     ! mixing height [m]
             REAL,    INTENT( IN )  :: STKDM	     ! stack diameter [m]
             REAL,    INTENT( IN )  :: STKHT	     ! stack height [m]
             REAL,    INTENT( IN )  :: STKTK	     ! exhaust temperature [deg K]
             REAL,    INTENT( IN )  :: STKVE	     ! exhaust velocity [m/s]
             REAL,    INTENT( IN )  :: TSTK	     ! tmptr at top of stack [deg K]
             REAL,    INTENT( IN )  :: USTAR	     ! friction velocity [m/s]
             REAL,    INTENT( IN )  :: DTHDZ( : )    ! gradient of THETV
             REAL,    INTENT( IN )  :: TA   ( : )    ! temperature [deg K]
             REAL,    INTENT( IN )  :: WSPD ( : )    ! wind speed [m/s]
             REAL,    INTENT( IN )  :: ZF ( 0:  )    ! layer surface height [m]
             REAL,    INTENT( IN )  :: ZH   ( : )    ! layer center height [m]
             REAL,    INTENT( IN )  :: ZSTK ( : )    ! zf( l ) - stkht [m]
             REAL,    INTENT( INOUT ) :: WSTK	     ! wind speed @ top of stack [m/s]
             REAL,    INTENT( OUT ) :: ZPLM 	     ! OUT for reporting, only
           END SUBROUTINE PLMRIS		     ! temporarily, plume top height
          END INTERFACE 			     ! above stack, finally plume centerline
C----------------------------------------------------! height [m] (can be greater than the   -------------------
						     ! height of the top of the EMLAYS layer)
         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            LOGDEV = SETUP_LOGDEV()
            
	    call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, rc=myrc)
            if (aqm_rc_check(myrc, msg="Failure to retrieve grid coordinates in PT3D", 
     &         file=__FILE__, line=__LINE__)) return
	    
	    write(logdev,*)'LOCALPET, is, ie, js, je=',mylocalpet, is, ie, js, je
	    write(aline,"('PT/pt-',i4.4,'.nc')")mylocalpet
	    L=nf90_open(trim(aline),nf90_nowrite, ncid)
	    if(L.ne.nf90_noerr) then
	          IF ( AQM_RC_CHECK( L,
     &               MSG='failed to open '//trim(aline),
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
	    endif
	    call check(nf90_inq_dimid(ncid,'nlocs',iddim_stack))
            call check(nf90_inquire_dimension(ncid,iddim_stack,len=nstack))
	    allocate(tlat(nstack),tlon(nstack),ixt(nstack),jyt(nstack),stkdm(nstack),stkht(nstack),
     &	     stktk(nstack),stkve(nstack),stkflw(nstack))
            
	    call check(nf90_inq_varid(ncid,'LATITUDE',idvar))
	    call check(nf90_get_var(ncid,idvar,tlat))
	    call check(nf90_inq_varid(ncid,'LONGITUDE',idvar))
	    call check(nf90_get_var(ncid,idvar,tlon))
	    call check(nf90_inq_varid(ncid,'STKDM',idvar))
	    call check(nf90_get_var(ncid,idvar,stkdm))
	    call check(nf90_inq_varid(ncid,'STKHT',idvar))
	    call check(nf90_get_var(ncid,idvar,stkht))
	    call check(nf90_inq_varid(ncid,'STKTK',idvar))
	    call check(nf90_get_var(ncid,idvar,stktk))
	    call check(nf90_inq_varid(ncid,'STKVE',idvar))
	    call check(nf90_get_var(ncid,idvar,stkve))
	    call check(nf90_inq_varid(ncid,'STKFLW',idvar))
	    call check(nf90_get_var(ncid,idvar,stkflw))

            distnear=0.7071*haversine(grid_data%lat(1,1),grid_data%lon(1,1),grid_data%lat(2,1),grid_data%lon(2,1))
            do n=1,nstack
	     search_loop: do r = 1, my_nrows
               do c = 1, my_ncols
		 if(haversine(grid_data%lat(c,r),grid_data%lon(c,r),tlat(n),tlon(n)).le.distnear)  exit search_loop
	       enddo
	     enddo search_loop
	     if(c.le.my_ncols.and.r.le.my_nrows) then
	       ixt(n)=c; jyt(n)=r
	     else
	       ixt(n)=-999; jyt(n)=-999
	     endif  
	    enddo
         
	    call check(nf90_inq_varids(ncid,nvars,idlists))

	    nvars_gc=0
	    nvars_pm=0
	    do n=1,nvars
	      call check(nf90_inquire_variable(ncid,idlists(n),vname,ndims=ndims))
	      L=index1(vname,n_gc_emis, gc_emis)
	      if (L > 0) then
	       
	       if(ndims.ne.2) then
	        write(logdev,*)'ndims wrong',ndims,vname
		IF ( AQM_RC_CHECK( 1,
     &               MSG='gaseous ndims wrong '//trim(vname),
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
	       endif       
	       if( PTEM_MAP( L ) .gt.0) then
	        nvars_gc=nvars_gc+1
	        id_em_gc(nvars_gc)=idlists(n)
	        indx_gc(nvars_gc)=PTEM_MAP( L )
	       endif
	       
	      else ! aerosol
	       L=index1(vname,n_emis_pm, pmem_map_name) ! index in pt em
	       if (L > 0) then
	        if(ndims.ne.2) then
	         write(logdev,*)'ndims wrong',ndims,vname
		 IF ( AQM_RC_CHECK( 1,
     &               MSG='aerosol ndims wrong '//trim(vname),
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
	        endif
	        do S=1, N_SPC_PTPM
	         if(PTPM_MAP(S).eq.L) exit
	        enddo
		if (S. le. N_SPC_PTPM ) then
	          nvars_pm=nvars_pm+1
	          id_em_pm(nvars_pm)=idlists(n)
		  indx_pm(nvars_pm)=S
	        endif
	       endif
	      endif
	    enddo
	    
	    write(logdev,*)'Point Sources nstack, nvars_gc, nvars_pm=',nstack,nvars_gc, nvars_pm
	    write(logdev,*)'ncols,nrows,my_ncols,my_nrows=',ncols,nrows,my_ncols,my_nrows
	    allocate(my_area(my_ncols,my_nrows))
	    if(.not.interpx(GRID_CRO_2D,'AREA','emis',1,my_ncols,1,my_nrows,1,1,jdate,jtime,my_area)) then
	     IF ( AQM_RC_CHECK( 1,
     &               MSG='failed to get area '//trim(aline),
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
            endif
	    
	    allocate(stkemis(nstack,nvars_gc+nvars_pm),uwind(my_ncols,my_nrows,emlyrs), vwind(my_ncols,my_nrows,emlyrs),
     &	    stk_ddzf(emlyrs),stk_pres(emlyrs),stk_dens(emlyrs),stk_qv(emlyrs),stk_ta(emlyrs),stk_wspd(emlyrs),stk_zf(emlyrs),
     &      stk_zh(emlyrs),stk_zstk(emlyrs),stk_dthdz(emlyrs),stk_uwind(emlyrs),stk_vwind(emlyrs),tfrac(emlyrs),
     &      stk_presf(0:emlyrs),stk_zzf(0:emlyrs))

	    jstartdate=jdate
	    jstarttime=jtime
	 END IF

C        ... initialize emission arrays ...

         VDEMIS_PT      = 0.0   ! array assignment
         VDEMIS_PT_FIRE = 0.0   ! array assignment
         PMEMIS_PT      = 0.0   ! array assignment


C--- anthropogenic point sources
         
	 itstep=secsdiff(jstartdate,jstarttime,jdate,jtime)/3600+1
	 write(logdev,*)'process PT emission ',jdate,jtime,tstep(1),itstep, mylocalpet,nstack
	 n=nf90_inq_path(ncid,L,path_in)
	 if(n.ne.nf90_noerr) then
	  write(logdev,*)itstep,'ncid wrong ',trim(nf90_strerror(n))
          IF ( AQM_RC_CHECK( 1,
     &               MSG='ncid wrong ',
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
	 endif
	   
	 elemStart(1)=1; elemStart(2)=itstep
	 elemCount(1)=nstack; elemCount(2)=1
	 do v=1,nvars_gc
c           write(logdev,*)'read PT emission of gas ',v,itstep
	   L=nf90_get_var(ncid,id_em_gc(v),stkemis(:,v),start=elemStart,count=elemCount)
	   if(L.ne.nf90_noerr) then
            write(logdev,*)trim(nf90_strerror(L)),' error reading PT emission of gas ',v,itstep,nstack,id_em_gc(v)
	    S=nf90_get_var(ncid,id_em_gc(v),stkemis(:,v),start=[1,1],count=elemCount)
	    if(S.ne.nf90_noerr) then
	     write(logdev,*)'also error 1-step reading PT emission of gas ',v,trim(nf90_strerror(S)),id_em_gc(v)
	    else
	      write(logdev,*)'OK for 1-step reading PT emission of gas ',v,trim(nf90_strerror(S)),id_em_gc(v)
	    endif
	    IF ( AQM_RC_CHECK( 1,
     &               MSG='failed to read PT gas emission  ',
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
	   endif 
	 enddo
	 
	 do v=1,nvars_pm
c         write(logdev,*)'read PT emission of PM ',v
	   L=nf90_get_var(ncid,id_em_pm(v),stkemis(:,v+nvars_gc),start=elemStart,count=elemCount)
	   if(L.ne.nf90_noerr) then
	    write(logdev,*)'error reading PT emission of pm ',v,itstep,nstack,id_em_pm(v)
	    IF ( AQM_RC_CHECK( 1,
     &               MSG='failed to read PT aerosol emission  ',
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
	   endif 
	 enddo
c	 call check(nf90_close(ncid))
	 
	 if(.not.interpx(MET_CRO_3D,'UWINDA','PT3D_DEFN',1,my_ncols,1,my_nrows,1,emlyrs,jdate,jtime,uwind)) then
	   IF ( AQM_RC_CHECK( 1,
     &               MSG='failed to read wind field ',
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
         endif
	 if(.not.interpx(MET_CRO_3D,'VWINDA','PT3D_DEFN',1,my_ncols,1,my_nrows,1,emlyrs,jdate,jtime,vwind)) then
	   IF ( AQM_RC_CHECK( 1,
     &               MSG='failed to read wind field ',
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
         endif
	 
	 mxzplm=0.0
	   
         do n=1,nstack
	   if(ixt(n).lt.1.or.jyt(n).lt.1) cycle
           c=ixt(n)
	   r=jyt(n)
	   
	   stk_zf(1:emlyrs)=met_data%zf(c,r,1:emlyrs)
	   stk_zh(1:emlyrs)=met_data%zh(c,r,1:emlyrs)
           stk_zzf(1:emlyrs)=stk_zf(1:emlyrs)
	   stk_zzf(0)=0.
	   
c-----calculate ddzf	   
	   zf0=stk_zf(1)
	   stk_ddzf(1)=1./zf0
	   stk_zstk(1)=zf0-stkht(n)
	   do L=2,emlyrs
	    zf1=stk_zf(L)
	    stk_zstk(L)=zf1-stkht(n)
	    stk_ddzf(L)=1./(zf1-zf0)
	    zf0=zf1
	   enddo
	   
	   stk_ta(1:emlyrs)=met_data%ta(c,r,1:emlyrs)
	   stk_qv(1:emlyrs)=met_data%qv(c,r,1:emlyrs)
	   stk_uwind(1:emlyrs)=uwind(c,r,1:emlyrs)
	   stk_vwind(1:emlyrs)=vwind(c,r,1:emlyrs)
	   stk_presf(1:emlyrs)=met_data%pres(c,r,1:emlyrs)  ! full level pressure
	   stk_presf(0)=met_data%prsfc(c,r)

C Compute derived met vars needed before layer assignments
               CALL PREPLM( .FALSE. , EMLYRS,
     &                      Met_Data%PBL(C,R), STKHT(N), met_data%prsfc(c,r),
     &                      Met_Data%TEMP2(C,R),    stk_ddzf,
     &                      stk_qv,    stk_ta,
     &                      stk_uwind, stk_vwind,
     &                      stk_zh,    stk_zf,
     &                      stk_presf, LSTK,  LPBL, TSTK, WSTK,
     &                      stk_DTHDZ, stk_WSPD )

C Trap USTAR at a minimum realistic value
               USTMP = MAX1( Met_Data%USTAR(C, R ), 0.1 )

C Convert heat flux (watts/m2 to m K /s )
               HFLX = Met_Data%HFX( C,R ) / ( 1004.7642148 * Met_Data%DENS( C,R,1 ) )

                  CALL PLMRIS( EMLYRS, LSTK, HFLX, Met_Data%PBL(C,R),
     &                         STKDM(N), STKHT(N),
     &                         STKTK(N), STKVE( N ),
     &                         TSTK, USTMP,
     &                         stk_DTHDZ,     stk_TA,
     &                         stk_WSPD,      stk_ZZF,
     &                         stk_ZH, stk_ZSTK,
     &                         WSTK, ZPLM )
	     
 	       if ( zplm .gt. mxzplm ) mxzplm = zplm
	     
C Default Turner approach.  Plume thickness = amount of plume rise
C Plume rise DH = ZPLM minus the stack height STKHT
                     ZTOP = STKHT( N )
     &                    + 1.5 * ( ZPLM - STKHT( N ) )
                     ZBOT = STKHT( N )
     &                    + 0.5 * ( ZPLM - STKHT( N ) )

C Set up for computing plume fractions, assuming uniform distribution in pressure
C (~mass concentration -- minor hydrostatic assumption) from bottom to top.

               IF ( ZTOP .LT. STKHT( N ) ) THEN
                  WRITE( LOGDEV,94010 ) 'ERROR: Top of plume is less than '
     &                                // 'top of stack for source:', N
                  WRITE( LOGDEV,* ) '    Zbot: ', ZBOT, ' Ztop: ', ZTOP
                  WRITE( LOGDEV,* ) '    Stack Top: ', STKHT( N ),
     &                              ' Plume Top: ', ZPLM
                  IF ( AQM_RC_CHECK( 1,
     &               MSG='ERROR: Top of plume is less than stack height ',
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
         
               END IF

C Compute LBOT, LTOP such that
C  ZZF( LBOT-1 ) <= ZBOT < ZZF( LBOT ) and
C  ZZF( LTOP-1 ) <= ZTOP < ZZF( LTOP )

               DO L = 1, EMLYRS - 1
                  IF ( ZBOT .LE. STK_ZZF( L ) ) THEN
                     LBOT = L
                     GO TO  122
                  ELSE
                     TFRAC( L ) = 0.0             ! fractions below plume
                  END IF
               END DO
               LBOT = EMLYRS                      !  fallback

122            CONTINUE                           !  loop exit:  bottom found at LBOT

               IF ( ZTOP .LE. stk_ZZF( LBOT ) ) THEN  ! plume in this layer

                  TFRAC( LBOT ) = 1.0
                  LTOP = LBOT

                  DO L = LBOT + 1, EMLYRS         ! fractions above plume
                     TFRAC( L ) = 0.0
                  END DO

               ELSE IF ( LBOT .EQ. EMLYRS ) THEN  ! plume above top layer

                  TFRAC( LBOT ) = 1.0

                  DO L = 1, EMLYRS - 1            ! fractions below plume
                     TFRAC( L ) = 0.0
                  END DO

               ELSE                               ! plume crosses layers

                  DO L = LBOT + 1, EMLYRS
                     IF ( ZTOP .LE. STK_ZZF( L ) ) THEN
                        LTOP = L
                        GO TO 126
                     END IF
                  END DO
                  LTOP = EMLYRS                   !  fallback

126               CONTINUE

                  ZDIFF = ZTOP - ZBOT
                  IF ( ZDIFF .GT. 0.0 ) THEN
                     DDZ  = 1.0 / ZDIFF
                     TFRAC( LBOT ) = DDZ * ( stk_ZZF( LBOT ) - ZBOT )
                     TFRAC( LTOP ) = DDZ * ( ZTOP - stk_ZZF( LTOP-1 ) )

                  ELSE   ! ZDIFF .le. 0                     
                     WRITE(logdev,* )
     &                  'Infinitely small plume created for source:,'
     &                   ,N,'All emissions put in first layer.'
                     LBOT = 1; LTOP = 1
                     TFRAC( LBOT ) = 1.0
                  END IF

                  DO L = LBOT + 1, LTOP - 1       ! layers in plume
                     TFRAC( L ) = DDZ * ( stk_ZZF( L ) - stk_ZZF( L-1 ) )
                  END DO

                  DO L = LTOP + 1, EMLYRS         ! fractions above plume
                     TFRAC( L ) = 0.0
                  END DO

               END IF

C If layer fractions are negative, put in the first layer

               IF ( MINVAL( TFRAC( 1:EMLYRS ) ) .LT. 0.0 ) THEN
                  WRITE( logdev,* ) 'WARNING: One or more negative plume '
     &                     // 'fractions found for source:' , N, 'Plume reset to '
     &                     // 'put all emissions in surface layer.'
                  TFRAC( 1 ) = 1.0
                  TFRAC( 2:EMLYRS ) = 0.0
               END IF

C Apportion emissions to the layers

               DO L = 1, EMLYRS
                  LFRC = TFRAC( L )
                  IF ( LFRC .LE. 0.0 ) CYCLE

                  DO V = 1, nvars_gc
                     I = indx_gc( V )
                     VDEMIS_PT( C,R,L,I ) = VDEMIS_PT( C,R,L,I )                                                                                                                  + LFRC * STKEM( J )
     &                                    + LFRC * stkemis(n,V)/my_area (c,r)
                  END DO
                  DO V = 1, nvars_pm
                     I = indx_pm( V )
                     PMEMIS_PT( C,R,L,I ) = PMEMIS_PT( C,R,L,I )
     &                                    + LFRC * stkemis(n,V+nvars_gc)/my_area (c,r)   ! emis fac applied in AERO_EMIS
                  END DO

               END DO

	   
         enddo  ! end loop of nstack

c-----FIRE emissions

         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN

C For each time step, compute the layer fractions...

         WRITE( XMSG,'(A, I7.6)' )
     &   'Calculating emissions point source layer fractions for', JTIME
         WRITE( LOGDEV,* ) ' '
         CALL M3MSG2( XMSG )


C        ... initialize vertical fraction arrays ...
C        ... fire emissions are added to surface only by default ...

         VFRAC        = 0.0
         VFRAC(:,:,1) = 1.0

C Retrieve fire emissions and distribute according to plume-rise algorithm

C        ... plumerise   ...        

         SELECT CASE ( TRIM( EM % PLUMERISE ) )
            CASE ( "sofiev" )
               N = INDEX1( EMFRP, SIZE( EM % SPECIES ), EM % SPECIES )
               IF ( N > 0 ) THEN
C                 read in frp
                  BUFFER = 0.0
                  CALL AQM_EMIS_READ( ETYPE, EMFRP, BUFFER, RC=LOCALRC )
                  IF ( AQM_RC_CHECK( LOCALRC,
     &               MSG="failure while reading frp from " //
     &               TRIM( ETYPE ) // " emissions",
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
                  CALL AQM_PLUME_SOFIEV( EM, BUFFER, VFRAC, RC=LOCALRC )
                  IF ( AQM_RC_CHECK( LOCALRC,
     &               MSG="failed to compute plume rise",
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
               ELSE
                  CALL M3WARN( PNAME, JDATE, JTIME,
     &               EMFRP // "species not found. " //
     &               "Adding fire emissions to surface only" )
               END IF
            CASE ( "none" )
C                no plume rise
            CASE DEFAULT
C                plume rise is disabled by default
         END SELECT
         
C        ... gas species ...        

         IS_NOT_NVPOA = ( INDEX( MECHNAME, 'NVPOA' ) .EQ. 0 )

         DO S = 1, N_GSPC_EMIS
            N = PTEM_MAP( S )
            IF ( N .GT. 0 ) THEN
               BUFFER = 0.0
               CALL AQM_EMIS_READ( ETYPE, EM % TABLE( N, 1 ), BUFFER, RC=LOCALRC )
               IF ( AQM_RC_CHECK( LOCALRC, MSG="Failure while reading " //
     &            TRIM( EM % TABLE( N, 1 ) ) // " from " // TRIM( ETYPE ) // " emissions",
     &            FILE=__FILE__, LINE=__LINE__)) RETURN
C              Read Non-Carbon Organic Matter too if POC is Requested
               SAVE_POC = .FALSE.
               IF ( IS_NOT_NVPOA .AND. EM % TABLE( N, 1 ) .EQ. 'POC' ) THEN
                  CALL AQM_EMIS_READ( ETYPE, 'PNCOM', BUFFER, RC=LOCALRC )
                  IF ( AQM_RC_CHECK( LOCALRC,
     &               MSG="Failure while reading PNCOM" //
     &               " from " // TRIM( ETYPE ) // " emissions",
     &               FILE=__FILE__, LINE=__LINE__)) RETURN
                  SAVE_POC = IS_NOT_NVPOA
               END IF
               DO L = 1, EMLYRS
                  K = 0
                  DO R = 1, MY_NROWS
                     DO C = 1, MY_NCOLS
                        K = K + 1
                        VDEMIS_PT( C,R,L,N ) = VDEMIS_PT( C,R,L,N )+ VFRAC( C,R,L ) * BUFFER( K )
                     END DO
                  END DO
               END DO
               IF ( SAVE_POC ) THEN
                  DO L = 1, EMLYRS
                     K = 0
                     DO R = 1, MY_NROWS
                        DO C = 1, MY_NCOLS
                           K = K + 1
                           VDEMIS_PT_FIRE( C,R,L,N ) = VFRAC( C,R,L ) * BUFFER( K )
                        END DO
                     END DO
                  END DO
               END IF
            END IF
         END DO

C        ... aerosol species ...        

         DO S = 1, N_SPC_PTPM  ! FIRE inventory index of aerosol  
            V = PTPM_MAP( S )   ! index in aerosol emission holder
            BUFFER = 0.0
            CALL AQM_EMIS_READ( ETYPE, PMEM_MAP_NAME( V ), BUFFER, RC=LOCALRC )
            IF ( AQM_RC_CHECK( LOCALRC, MSG="Failure while reading " //
     &         TRIM( PMEM_MAP_NAME( V ) ) // " from " // TRIM( ETYPE ) // " emissions",
     &         FILE=__FILE__, LINE=__LINE__)) RETURN
            DO L = 1, PM_EMLYRS
               K = 0
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     K = K + 1
                     PMEMIS_PT( C,R,L,S ) = PMEMIS_PT( C,R,L,S ) + VFRAC( C,R,L ) * BUFFER( K )
                  END DO
               END DO
            END DO
         END DO

94010    FORMAT( 12( A, :, I8, :, 1X ) )
         
         RETURN

         END SUBROUTINE GET_PT3D_EMIS
      
      function to_radian(degree) result(rad)
          ! degrees to radians
          real,intent(in) :: degree
          real, parameter :: deg_to_rad = atan(1.0)/45 ! exploit intrinsic atan to generate pi/180 runtime constant
          real :: rad
 
          rad = degree*deg_to_rad
      end function to_radian
 
      function haversine(deglat1,deglon1,deglat2,deglon2) result (dist)
          real,intent(in) :: deglat1,deglon1,deglat2,deglon2
          real :: a,c,dist,dlat,dlon,lat1,lat2
          real,parameter :: radius = 6372.8 ! in km
 
          dlat = to_radian(deglat2-deglat1)
          dlon = to_radian(deglon2-deglon1)
          lat1 = to_radian(deglat1)
          lat2 = to_radian(deglat2)
          a = (sin(dlat/2))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2))**2
          c = 2*asin(sqrt(a))
          dist = radius*c
      end function haversine

       subroutine check(status)
       integer, intent ( in) :: status

       if(status /= nf90_noerr) then
         print *, 'netcdf error in PT3D_DEFN.F ', trim(nf90_strerror(status))
         stop "Stopped"
       end if
      end subroutine check

      END MODULE PT3D_DEFN
