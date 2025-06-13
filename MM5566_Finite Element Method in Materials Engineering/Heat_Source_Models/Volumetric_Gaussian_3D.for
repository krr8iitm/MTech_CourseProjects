      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME
C
      DOUBLE PRECISION q_total, eta, sigma_r, sigma_z, v, pi
      DOUBLE PRECISION x, y, z, t, zs, r, q_flux
C
C     Laser parameters (calibrated for 1.5 mm plate)
      q_total = 2000.0D0     ! Total power [W]
      eta = 0.75D0           ! Absorption efficiency [2]
      sigma_r = 0.0005D0     ! Radial std dev (0.5 mm) [2][3]
      sigma_z = 0.0005D0     ! Axial std dev (0.5 mm) [5]
      v = 0.0333333D0        ! Welding speed (2 m/min) [1]
      pi = 3.141592653589793D0
C
C     Current position and time
      t = TIME(1)            ! Current simulation time
      zs = v * t             ! Moving source position
      x = COORDS(1)
      y = COORDS(2)
      z = COORDS(3)
C
C     3D Gaussian distribution (volumetric)
      q_flux = (q_total*eta)/((2*pi)**1.5 * sigma_r**2 * sigma_z)
     1       * EXP(-(x**2 + y**2)/(2*sigma_r**2) 
     2       - (z - zs)**2/(2*sigma_z**2))
C
C     Apply flux and set type
      FLUX(1) = q_flux
      FLUX(2) = 0.0D0
      JLTYP = 0              ! Volumetric heat flux
C
      RETURN
      END
