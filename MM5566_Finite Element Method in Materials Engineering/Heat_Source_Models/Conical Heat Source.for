      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME
C
      DOUBLE PRECISION q_total, eta, r0, h, v, pi
      DOUBLE PRECISION x, y, z, t, zs, r, z_rel, q_flux
C
C     Laser parameters (calibrated for 1.5 mm plate)
      q_total = 2000.0D0     ! Total power [W]
      eta = 0.75D0           ! Absorption efficiency
      r0 = 0.0005D0          ! Base radius at surface (0.5 mm)
      h = 0.0015D0           ! Height = plate thickness (1.5 mm)
      v = 0.0333333D0        ! Welding speed (2 m/min)
      pi = 3.141592653589793D0
C
C     Current position and time
      t = TIME(1)            ! Correct temporal variable
      zs = v * t             ! Moving source position (Z-axis)
      x = COORDS(1)
      y = COORDS(2)
      z = COORDS(3)
C
C     Relative Z-position within heat source
      z_rel = z - zs + h/2.0D0  ! Offset to start at surface
C
C     Conical heat source conditions
      IF (z_rel >= 0.0D0 .AND. z_rel <= h) THEN
          r_current = r0 * (1.0D0 - z_rel/h)  ! Tapering radius
          radial_dist = SQRT(x**2 + y**2)
          
          IF (radial_dist <= r_current) THEN
              q_flux = (3.0D0*q_total*eta)/(pi*r0**2*h) 
     1               * (1.0D0 - z_rel/h)      ! Axial decay
          ELSE
              q_flux = 0.0D0
          ENDIF
      ELSE
          q_flux = 0.0D0
      ENDIF
C
C     Apply flux and set type
      FLUX(1) = q_flux
      FLUX(2) = 0.0D0
      JLTYP = 0              ! Volumetric heat flux
C
      RETURN
      END
