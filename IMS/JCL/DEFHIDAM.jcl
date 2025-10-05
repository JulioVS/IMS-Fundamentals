//DEFHIDAM  JOB FB3,NOTIFY=&SYSUID
//*
//*   UTILITY JCL FOR IDCAMS  -- ALLOCATE FILES FOR 'HIDAM/OSAM' DB
//*                              KEYS => VSAM <KSDS>
//*                              DATA => OSAM
//*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT   DD SYSOUT=*
//SYSIN      DD *,SYMBOLS=CNVTSYS
  DELETE &SYSUID..IMS.IVPDB1  NONVSAM
  DELETE &SYSUID..IMS.IVPDB1I CLUSTER
  SET MAXCC=0

  ALLOCATE                                    -
      DSNAME('&SYSUID..IMS.IVPDB1')           -
      FILE(IVPDB1)                            -
      STORCLAS(ZXPS)                          -
      DATACLAS(ZXPDX)                         -
      RECFM(F,B,S)                            -
      LRECL(2048)                             -
      BLKSIZE(2048)                           -
      DSORG(PS)                               -
      NEW CATALOG                             -
      SPACE(2) CYLINDERS

  DEFINE CLUSTER(                             -
          NAME('&SYSUID..IMS.IVPDB1I')        -
            INDEXED                           -
            STORCLAS(ZXPS)                    -
            DATACLAS(ZXPDX)                   -
            KEYS(10 05)                       -
            FREESPACE(10 10)                  -
            RECORDSIZE(16 16)                 -
            SHAREOPTIONS(3 3)                 -
            SPEED                             -
            UNIQUE                            -
            TRACKS(05)                        -
          )                                   -
          DATA(                               -
            NAME('&SYSUID..IMS.IVPDB1I.DATA') -
            CONTROLINTERVALSIZE(2048)         -
          )                                   -
          INDEX(                              -
            NAME('&SYSUID..IMS.IVPDB1I.INDX') -
            CONTROLINTERVALSIZE(4096)         -
          )
/*
