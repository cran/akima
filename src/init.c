
#include <R.h>
#include <Rinternals.h>

#include "akima.h"

#include <R_ext/Rdynload.h>

/* Fortran interface descriptions: */

static R_NativePrimitiveArgType idbvip_t[13] = {
  INTSXP,  /* MD,   */
  INTSXP,  /* NCP,  */
  INTSXP,  /* NDP,  */
  REALSXP, /* XD,   */
  REALSXP, /* YD,   */
  REALSXP, /* ZD,   */
  INTSXP,  /* NIP,  */
  REALSXP, /* XI,   */
  REALSXP, /* YI,   */
  REALSXP, /* ZI,   */
  INTSXP,  /* IWK,  */
  REALSXP, /* WK,   */
  LGLSXP   /* MISSI */
};

static R_NativePrimitiveArgType sdbi3p_t[16] = {
  INTSXP,  /* MD,     */
  INTSXP,  /* NDP,    */
  REALSXP, /* XD,     */
  REALSXP, /* YD,     */
  REALSXP, /* ZD,     */
  INTSXP,  /* NIP,    */
  REALSXP, /* XI,     */
  REALSXP, /* YI,     */
  REALSXP, /* ZI,     */
  INTSXP,  /* IER,    */
  REALSXP, /* WK,     */
  INTSXP,  /* IWK,    */
  LGLSXP,  /* EXTRPI, */
  INTSXP,  /* NEAR,   */
  INTSXP,  /* NEXT,   */
  REALSXP  /* DIST    */
};

static R_NativePrimitiveArgType idsfft_t[14] = {
  INTSXP,  /* MD,   */
  INTSXP,  /* NCP,  */
  INTSXP,  /* NDP,  */
  REALSXP, /* XD,   */
  REALSXP, /* YD,   */
  REALSXP, /* ZD,   */
  INTSXP,  /* NXI,  */
  INTSXP,  /* NYI,  */
  REALSXP, /* XI,   */
  REALSXP, /* YI,   */
  REALSXP, /* ZI,   */
  INTSXP,  /* IWK,  */
  REALSXP, /* WK,   */
  LGLSXP   /* MISSI */
};

static R_NativePrimitiveArgType sdsf3p_t[17] = {
  INTSXP,  /* MD,     */
  INTSXP,  /* NDP,    */
  REALSXP, /* XD,     */
  REALSXP, /* YD,     */
  REALSXP, /* ZD,     */
  INTSXP,  /* NXI,    */
  REALSXP, /* XI,     */
  INTSXP,  /* NYI,    */
  REALSXP, /* YI,     */
  REALSXP, /* ZI,     */
  INTSXP,  /* IER,    */
  REALSXP, /* WK,     */
  INTSXP,  /* IWK,    */
  LGLSXP,  /* EXTRPI, */
  INTSXP,  /* NEAR,   */
  INTSXP,  /* NEXT,   */
  REALSXP  /* DIST    */
};

static R_NativePrimitiveArgType uvip3p_t[8] = {
  INTSXP,  /* NP, */
  INTSXP,  /* ND, */
  REALSXP, /* XD, */
  REALSXP, /* YD, */
  INTSXP,  /* NI, */
  REALSXP, /* XI, */
  REALSXP, /* YI, */
  INTSXP   /* ERR */
};

static R_NativePrimitiveArgType intrpl_t[7] = {
  INTSXP,  /* L,  */
  REALSXP, /* X,  */
  REALSXP, /* Y,  */
  INTSXP,  /* N,  */
  REALSXP, /* U,  */
  REALSXP, /* V,  */
  INTSXP   /* ERR */
};

static R_FortranMethodDef fortranMethods[] = {
  {"idbvip", (DL_FUNC) &F77_SUB(idbvip), 13, idbvip_t}, /* interpp.old */
  {"sdbi3p", (DL_FUNC) &F77_SUB(sdbi3p), 16, sdbi3p_t}, /* interpp.new */
  {"idsfft", (DL_FUNC) &F77_SUB(idsfft), 14, idsfft_t}, /* interp.old  */
  {"sdsf3p", (DL_FUNC) &F77_SUB(sdsf3p), 17, sdsf3p_t}, /* interp.new  */
  {"uvip3p", (DL_FUNC) &F77_SUB(uvip3p), 8, uvip3p_t},  /* aspline     */
  {"intrpl", (DL_FUNC) &F77_SUB(intrpl), 7, intrpl_t},  /* aspline     */
  {NULL, NULL, 0}
};

void
R_init_akima(DllInfo *info)
{
  R_registerRoutines(info, 
		     NULL /*cMethods*/, NULL /*callMethods*/, 
		     fortranMethods, NULL/*externalMethods*/);
}
