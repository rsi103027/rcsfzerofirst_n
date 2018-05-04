!***********************************************************************
!                                                                      *
      SUBROUTINE LODCSL_Part(CSF_Number,MAXN) 
!                                                                      *
!   Loads the data from the  .csl  file. A number of checks are made   *
!   to ensure correctness and consistency.                             *
!                                                                      *
!                                                                      *
!   Written by  G. Gaigalas                     Vilnius, May 2016      *
!                                                                      *
!***********************************************************************
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE BLK_C,            only: NBLOCK,NCFBLK
      USE rang_Int_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER,  INTENT(OUT)   :: CSF_Number
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER            :: I, IOS, NCF, N, MAXN
      CHARACTER(LEN=256) :: RECORD,RECORD_C_shell,RECORD_C_quant,RECORD_C_coupl
      CHARACTER(LEN=2)   :: N_C
!-----------------------------------------------
!
!      print *,                                                          &
!      "Give the maxn of the zero-order space"
!      read(*,'(i2)') MAXN
!
      CSF_Number = 0
      DO 
         CSF_Number = CSF_Number + 1
         Found_CSF = 0
         IF(CSF_Number .EQ. 1) NCF = NCFBLK(NBLOCK) + 1 
         READ (20, '(A)', IOSTAT=IOS) RECORD 
         IF (IOS == 0) THEN 
            IF (RECORD(1:2) == ' *') RETURN
            RECORD_C_shell = RECORD
            N_C=RECORD_C_shell(len_trim(RECORD_C_shell)-7:len_trim(RECORD_C_shell)-6)
            read(N_C,'(i2)')N
            
!
!   Read the J_sub and v quantum numbers
!
            READ (20, '(A)') RECORD_C_quant
!
!   Read the X, J, and (sign of) P quantum numbers
!
            READ (20, '(A)') RECORD_C_coupl 
            IF(N > MAXN) THEN
               WRITE(22,'(A)') TRIM(RECORD_C_shell)
               WRITE(22,'(A)') TRIM(RECORD_C_quant)
               WRITE(22,'(A)') TRIM(RECORD_C_coupl)
            ENDIF
         ELSE
            RETURN
         ENDIF 
      END DO
      RETURN  
!
      END SUBROUTINE LODCSL_Part
