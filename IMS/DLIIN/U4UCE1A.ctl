S 1 1 1 1 1    COURSE
U
U              --- Assume 'LOAD' mode ---
U                (Re-create VSAM first)
U
U Call 1A
L        ISRT  COURSE
L        DATA  CM17                   SEGM #1
U Call 1B
L        ISRT  COURSE  (CODE    = CM17)                                X
L              NAME
L        DATA  ANNAJONES              SEGM #2
U Call 1C
L        ISRT  COURSE  (CODE    = CM17)                                X
L              NAME    (LASTNM  = JONES)                               X
L              EXPR
L        DATA  SOME EXPERIENCE AL5    SEGM #3
