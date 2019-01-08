PROGRAM menu_prgm
IMPLICIT none
INTEGER,INTENT    :: choix


PRINT*,' Options possibles :'
PRINT*,'     < 1 > : question 1'
PRINT*,'     < 2 > : question 2'
PRINT*,'     < 3 > : question 3'
PRINT*,'     < 4 > : question 4'
PRINT*, 'Entrer votre choix :'
READ*, choix

IF ((opt.lt.0).or.(opt.gt.4)) THEN
      PRINT*, 'mauvais choix : stop'
      STOP
END IF



END PROGRAM menu_prgm
