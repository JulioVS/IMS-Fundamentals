//IDCAMSA   JOB FB3,NOTIFY=&SYSUID
//*
//*   UTILITY JCL FOR IDCAMS  -- ALLOCATE VSAM OR NONVSAM FILES.-
//*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT   DD SYSOUT=*
//SYSIN      DD *,SYMBOLS=CNVTSYS
  DELETE &SYSUID..IMS.SKILL CLUSTER
  SET MAXCC=0

  DEFINE CLUSTER(                             -
          NAME('&SYSUID..IMS.SKILL')          -
            NONINDEXED                        -
            STORCLAS(ZXPS)                    -
            DATACLAS(ZXPDX)                   -
            FREESPACE(10,10)                  -
            RECORDSIZE(1017,1017)             -
            TRK(5,1)                          -
          )                                   -
          DATA(                               -
            NAME('&SYSUID..IMS.SKILL.DATA')   -
            CONTROLINTERVALSIZE(1024)         -
          )
/*
