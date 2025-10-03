S 1 1 1 1 1    COURSE
U
U              --- Assume 'UPDATE' mode ---
U                 (Use Procopt "A" PSB!)
U
U              --- Assume SG#1-5 exist  ---
U                   (Fully loaded DB)
U
U Call 10A
L        GHU   COURSE  (CODE    = CM17)                                X
L              NAME    (LASTNM  = JONES)
U Call 10B
L        DLET
