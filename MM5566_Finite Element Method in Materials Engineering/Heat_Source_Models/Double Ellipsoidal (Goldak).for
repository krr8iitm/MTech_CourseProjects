      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME
C
      DOUBLE PRECISION q_total, eta, a_front, a_rear, b, c, v, pi
      DOUBLE PRECISION x, y, z, t, zs, dist_x, dist_y, dist_z
      DOUBLE PRECISION f_front, f_rear, q_front, q_rear
C
C     Laser parameters (calibrated for 1.5 mm plate)
      q_total = 2000.0D0     ! Total power [W]
      eta = 0.75D0           ! Absorption efficiency
      a_front = 0.002D0      ! Front ellipsoid length (2 mm)
      a_rear = 0.004D0       ! Rear ellipsoid length (4 mm)
      b = 0.00075D0          ! Width semi-axis (0.75 mm) 
      c = 0.00075D0          ! Depth semi-axis (0.75 mm)
      v = 0.0333333D0        ! Welding speed (2 m/min)
      pi = 3.141592653589793D0
C
C     Current position and time
      t = TIME(1)            ! Correct temporal variable
      zs = v * t             ! Moving along Z-axis
      x = COORDS(1)
      y = COORDS(2)
      z = COORDS(3)
C
C     Distance from heat source center (Z-axis movement)
      dist_x = x
      dist_y = y
      dist_z = z - zs
C
C     Front ellipsoid contribution (60% power)
      f_front = (6.0D0*SQRT(3.0D0)*0.6D0*q_total*eta) /
     1         (a_front*b*c*pi**1.5D0)
      f_front = f_front * EXP(-3.0D0*((dist_x/a_front)**2 +
     1          (dist_y/b)**2 + (dist_z/c)**2))
C
C     Rear ellipsoid contribution (40% power)
      f_rear = (6.0D0*SQRT(3.0D0)*0.4D0*q_total*eta) /
     1        (a_rear*b*c*pi**1.5D0)
      f_rear = f_rear * EXP(-3.0D0*((dist_x/a_rear)**2 +
     1         (dist_y/b)**2 + (dist_z/c)**2))
C
C     Total heat flux
      FLUX(1) = f_front + f_rear
      FLUX(2) = 0.0D0
      JLTYP = 0              ! Volumetric heat flux
C
      RETURN
      END
