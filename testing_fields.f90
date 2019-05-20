PROGRAM testing_fields
USE charged_objects
USE vector_functions
IMPLICIT NONE

!!Definiendo mis objetos masivos
TYPE(charged_particles) :: antiproton !!Objeto tipo particula cargada: antiproton
TYPE(charged_particles) :: proton !!Objeto tipo particula
!!cargada: proton estacionario
TYPE(data) :: sim !!Objeto con informaci´on de la simulación

!!Definiendo variables locales
REAL(d) :: t !!Instante de tiempo en cada aproximaci´on
REAL(d) :: scale=1E9 !!Escalar longitudes en los plots
REAL ( d ), DIMENSION ( 3 ) :: runit !!Vector Unitario
INTEGER :: unit0 !!Unidad para archivo de salida
!!Asignando valores físicos a la particula en movimiento (MKS)
antiproton%mass = 1.7E-27_d !!Masa del antiproton
antiproton%q = -1.6E-19_d !!carga del antiproton
!!Asignando valores físicos a la particula fija (MKS)
proton%mass = 1.7E-27_d !!Masa del proton
proton%q = 1.6E-19_d !!Carga del proton
!Construyendo el vector de velocidad inicial del antiproton.
antiproton%vel = vector(5E1_d,8E2_d,5E1_d)
!Construyendo el vector de posici´on inicial del antiproton.
antiproton%pos = vector(0.5E-8_d,-1E-8_d,0.0_d)
!Vector de velocidad del proton (Fijo en esta aproximaci´on)
proton%vel = 0.0_d
!Vector de posici´on del proton (Fijo en esta aproximaci´on)
proton%pos = vector(-0.5E-8_d,0.0_d,0.0_d)
!!Construyendo el vector de Momento lineal inicial del antiproton.
antiproton%mom = antiproton%mass*antiproton%vel
antiproton%Eelec = 0.0_d
!!Ajustando el tama~no de paso (Cantidad de veces en las que se
!!actualizar´a la informaci´on del sistema).
sim%N_step = 5000 !!Usar´e s´olo 5000 pasos.
sim%ttot = 2E-11_d !!En este ejemplo se realizar´a la
!!evoluci´on durante unos segundos
!sim%ttot = sim%ttot/t0 !!Traduciendo a unidades naturales.
sim%dt = sim%ttot/sim%N_step !!Calculando el ancho de paso
!!Abriendo el archivo de texto
OPEN(NEWUNIT=unit0,FILE="Sim_ejemplo",STATUS="UNKNOWN",ACCESS="APPEND")
t=0 !!La simulaci´on inicia al tiempo t=0
!Repetir el proceso hasta que se cumpla la condici´on
DO
IF(t > sim%ttot ) EXIT
sim%pos2 = antiproton%pos-proton%pos !!Calculando el vector r=r2-r1
runit=sim%pos2/mag(sim%pos2)
!!Calculando la interacci´on el´ectrica.
antiproton%Eelec=( k*proton%q / mag(sim%pos2)**2)*runit
antiproton%Felec = antiproton%q*antiproton%Eelec
!!Actualizando el momento lineal
antiproton%mom = antiproton%mom + antiproton%Felec * sim%dt
!!Actualizando la posici´on
antiproton%pos = antiproton%pos + antiproton%mom / antiproton%mass * sim%dt
WRITE(unit0,*) t , antiproton%pos*scale !!Escribiendo en el archivo
t = t + sim%dt !!Actualizando el tiempo
ENDDO

CLOSE ( unit0 )
!!Este archivo sirve para la informaci´on del sol
OPEN ( NEWUNIT = unit0 , FILE = "proton" , STATUS = "UNKNOWN" )
WRITE ( unit0 , * ) proton%pos*scale
CLOSE ( unit0 )

CALL SYSTEM("gnuplot -p data_plot.plt") !!Graficar los resultados
END PROGRAM testing_fields
