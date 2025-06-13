      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME
C
      DOUBLE PRECISION q_total, eta, r0, h, v, pi
      DOUBLE PRECISION x, y, z, t, z_center, r
C
C     Laser parameters (calibrated for 1.5 mm plate)
      q_total = 2000.0D0     ! Total power [W]
      eta = 0.75D0           ! Absorption efficiency
      r0 = 0.0005D0          ! Beam radius (0.5 mm) 
      h = 0.0015D0           ! Equal to plate thickness (1.5 mm) 
      v = 0.0333333D0        ! Welding speed (2 m/min) 
      pi = 3.141592653589793D0
C
C     Current position and time
      t = TIME(1)            ! Correct time variable
      z_center = v * t       ! Moving source position
      x = COORDS(1)
      y = COORDS(2)
      z = COORDS(3)
C
C     Radial distance calculation
      r = SQRT(x**2 + y**2)
C
C     Cylindrical heat source model
      IF (r <= r0 .AND. ABS(z - z_center) <= h/2.0D0) THEN
          FLUX(1) = (q_total*eta)/(pi*r0**2*h)  ! Uniform flux within cylinder
      ELSE
          FLUX(1) = 0.0D0                       ! No flux outside
      ENDIF
      FLUX(2) = 0.0D0
      JLTYP = 0              ! Volumetric heat flux
C
      RETURN
      END
