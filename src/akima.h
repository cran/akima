
#include <R.h>

int F77_NAME(idbvip) (int *md, int *ncp, int *ndp,
		      double *xd, double *yd, double *zd,
		      int *nip, double *xi, double *yi, double *zi,
		      int *iwk, double *wk, int *missi);

int F77_NAME(sdbi3p) (int *md, int *ndp, double *xd, double *yd, double *zd,
		      int *nip, double *xi, double *yi, double *zi,
		      int *ier, double *wk, int *iwk,
		      int *extrpi, int *near, int *next, double *dist);

int F77_NAME(idsfft) (int *md, int *ncp, int *ndp, 
		      double *xd, double *yd,double *zd,
		      int *nxi, int *nyi, 
		      double *xi, double *yi, double *zi,
		      int *iwk, double *wk, int *missi);

int F77_NAME(sdsf3p) (int *md, int *ndp, double *xd, double *yd, double *zd,
		      int *nxi, double *xi, int *nyi, double *yi, double *zi,
		      int *ier, double *wk, int *iwk,
		      int *extrpi, int *near, int *next, double *dist);

int F77_NAME(uvip3p) (int *np, int *nd, double *xd, double *yd,
		      int *ni, double *xi, double *yi, int *err);

int F77_NAME(intrpl) (int *l,double *x, double *y, int *n,
		      double *u, double *v, int *err);

int F77_NAME(rgbi3p) (int *md, int *nxd, int *nyd, double *xd, double *yd, double *zd,
		      int *nip, double *xi, double *yi, double *zi, int *err);
