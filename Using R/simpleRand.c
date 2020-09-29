/* using the random number generator - slightly more complicated
   since we will call into R to figure out which RNG is in use
   then we generate "num" random Normal random variables and
   return them in a vector. Before returning we attach an attribute
   which states the RNG that was used.

   The "type" variable is not used. It is intended to allow 
   some simple exercises to be carried out by pupils, without the
   need to redefine the interface.

*/

SEXP simpleRand(SEXP num, SEXP type)
{
    SEXP ans, Rc, tmp;
    int i, x;

    /* isNumeric is true for LGLSXP - might not want that */
    if( !isNumeric(num) || length(num) > 1 )
        error("incorrect input");

    x = asInteger(num);
    PROTECT(ans = allocVector(REALSXP, x)); 

    /* allocate random normals, need to get and put state */
    GetRNGstate();
    for(i=0; i<x; i++)
        REAL(ans)[i] = norm_rand();
    PutRNGstate;

    /*create the language structure needed to call R */
    PROTECT(Rc = lang1(install("RNGkind")));
    PROTECT(tmp = eval(Rc, R_GlobalEnv));

    setAttrib(ans, install("RNG"), tmp);
    UNPROTECT(3);
    return(ans);
}

