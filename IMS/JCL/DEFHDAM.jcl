//DEFHDAM   JOB FB3,NOTIFY=&SYSUID
//*
//*   UTILITY JCL FOR IDCAMS  -- ALLOCATE FILE FOR 'HDAM' DB
//*                              DATA => VSAM <ESDS>
//*
// EXPORT SYMLIST=(*)
// SET MYDB='SKILL'
//*
//*   'HDAM' RECORD SIZE MUST BE EXACTLY 7 BYTES LESS THAN C.I. SIZE!
//*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT   DD SYSOUT=*
//SYSIN      DD *,SYMBOLS=CNVTSYS
  DELETE &SYSUID..IMS.&MYDB. CLUSTER
  SET MAXCC=0

  DEFINE CLUSTER(                             -
          NAME('&SYSUID..IMS.&MYDB.')         -
            NONINDEXED                        -
            STORCLAS(ZXPS)                    -
            DATACLAS(ZXPDX)                   -
            FREESPACE(10,10)                  -
            RECORDSIZE(1017,1017)             -
            TRK(5,1)                          -
          )                                   -
          DATA(                               -
            NAME('&SYSUID..IMS.&MYDB..DATA')  -
            CONTROLINTERVALSIZE(1024)         -
          )
/*
