#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>


#define DEG2RAD(x) (.01745329251994329576*(x))
#define RAD2DEG(x) (57.29577951308232087721*(x))


/* ----- CIE-XYZ <-> Device dependent RGB -----
 *
 *  Gamma Correction
 *
 *  The following two functions provide gamma correction which
 *  maps between device independent and device dependent RGB
 *  spaces sRGB.
 *
 *  The standard value of gamma for sRGB displays is 2.2.
 *
 *  gtrans maps device independent RGB to device dependent RGB.
 *  ftrans provides the inverse map.
 *
 */

static double gtrans(double u, double gamma)
{
    if (u > 0.00304)
	return 1.055 * pow(u, (1 / gamma)) - 0.055;
    else
	return 12.92 * u;
}

static double ftrans(double u, double gamma)
{
    if (u > 0.03928)
	return pow((u + 0.055) / 1.055, gamma);
    else
	return u / 12.92;
}

static int DEVRGB_to_RGB(double R, double G, double B, double gamma,
			 double *r, double *g, double *b)
{
    *r = ftrans(R, gamma);
    *g = ftrans(G, gamma);
    *b = ftrans(B, gamma);
    return 1;
}

static int RGB_to_DEVRGB(double R, double G, double B, double gamma,
			 double *r, double *g, double *b)
{
    *r = gtrans(R, gamma);
    *g = gtrans(G, gamma);
    *b = gtrans(B, gamma);
    return 1;
}

/* ----- CIE-XYZ <-> Device independent RGB -----
 *
 *  R, G, and B give the levels of red, green and blue as values
 *  in the interval [0,1].  X, Y and Z give the CIE chromaticies.
 *  XN, YN, ZN gives the chromaticity of the white point.
 *
 */

static int RGB_to_XYZ(double R, double G, double B,
		      double XN, double YN, double ZN,
		      double *X, double *Y, double *Z)
{
    *X = YN * (0.412453 * R + 0.357580 * G + 0.180423 * B);
    *Y = YN * (0.212671 * R + 0.715160 * G + 0.072169 * B);
    *Z = YN * (0.019334 * R + 0.119193 * G + 0.950227 * B);
    return 1;
}

static int XYZ_to_RGB(double X, double Y, double Z,
		      double XN, double YN, double ZN,
		      double *R, double *G, double *B)
{
    *R = ( 3.240479 * X - 1.537150 * Y - 0.498535 * Z) / YN;
    *G = (-0.969256 * X + 1.875992 * Y + 0.041556 * Z) / YN;
    *B = ( 0.055648 * X - 0.204043 * Y + 1.057311 * Z) / YN;
    return 1;
}


/* ----- CIE-XYZ <-> CIE-LAB ----- */

static double g(double t)
{
    return (t > 7.999625) ? pow(t, 3) : (t - 16.0 / 116.0) / 7.787;
}

static int LAB_to_XYZ(double L, double A, double B,
		      double XN, double YN, double ZN,
		      double *X, double *Y, double *Z)
{
    double fx, fy, fz;

    if (L <= 0)
	*Y = 0.0;
    else if (L <= 8.0)
	*Y = L * YN / 903.3;
    else if (L <= 100) 
	*Y = YN * pow((L + 16) / 116, 3);
    else
	*Y = YN;
    
    /* CHECKME - IS 0.00856 CORRECT???!!! */
    if (*Y <= 0.00856 * YN)
	fy = 7.787 * *Y / YN + 16.0 / 116.0;
    else
	fy = pow(*Y / YN, 1.0/3.0);
    
    fx = fy + (A / 500.0);
    if (pow(fx, 3) <= 0.008856)
	*X = XN * (fx - 16.0 / 116.0) / 7.787;
    else
	*X = XN * pow(fx, 3);
    
    fz = fy - (B / 200.0);
    if (pow(fz, 3) <= 0.008856)
	*Z = ZN * (fz - 16.0 / 116.0) / 7.787;
    else
	*Z = ZN * pow(fz, 3);
    return 1;
}

static double f(double t)
{
    return (t > 0.008856) ? pow(t, 1.0/3.0) : 7.787 * t + 16.0/116.0;
} 

static int XYZ_to_LAB(double X, double Y, double Z,
		      double XN, double YN, double ZN,
		      double *L, double *A, double *B)
{
    double xr, yr, zr, xt, yt ,zt;
    xr = X / XN;
    yr = Y / YN;
    zr = Z / ZN;
    if (yr > 0.008856)
	*L = 116.0 * pow(yr, 1.0/3.0) - 16.0;
    else
	*L = 903.3 * yr;
    xt = f(xr);
    yt = f(yr);
    zt = f(zr);
    *A = 500.0 * (xt - yt);
    *B = 200.0 * (yt - zt);
    return 1;
}


/* ----- CIE-XYZ <-> Hunter LAB -----
 *
 *  Hunter LAB is no longer part of the public API, but the code
 *  is still here in case it is needed.
 *
 */

static int XYZ_to_HLAB(double X, double Y, double Z,
		       double XN, double YN, double ZN,
		       double *L, double *A, double *B)
{
    X = X / XN;
    Y = Y / YN;
    Z = Z / ZN;
    *L = sqrt(Y);
    *A = 17.5 * (((1.02 * X) - Y) / *L);
    *B = 7 * ((Y - (0.847 * Z)) / *L);
    *L = 10 * *L;
}

static int HLAB_to_XYZ(double L, double A, double B,
		       double XN, double YN, double ZN,
		       double *X, double *Y, double *Z)
{
    double vX, vY, vZ;
    vY = L / 10;

    vX = (A / 17.5) * (L / 10);
    vZ = (B / 7) * (L / 10);
    vY = vY * vY;

    *Y = vY;
    *X = (vX + vY) / 1.02;
    *Z = -(vZ - vY) / 0.847;

    *X = *X * XN;
    *Y = *Y * YN;
    *Z = *Z * ZN;
    return 1;
}

/* ----- LAB <-> polarLAB ----- */

static int LAB_to_polarLAB(double L, double A, double B,
			   double *l, double *c, double *h)
{
    double vH;
    vH = RAD2DEG(atan2(B, A));
    while (vH > 360) vH -= 360;
    while (vH < 0) vH += 360;
    *l = L;
    *c = sqrt(A * A + B * B);
    *h = vH;
    return 1;
}

static int polarLAB_to_LAB(double L, double C, double H,
			   double *l, double *a, double *b)
{
    *l = L;
    *a = cos(DEG2RAD(H)) * C;
    *b = sin(DEG2RAD(H)) * C;
}

/* ----- RGB <-> HSV ----- */

static double max3(double a, double b, double c)
{
    if (a < b) a = b;
    if (a < c) a = c;
    return a;
}

static double min3(double a, double b, double c)
{
    if (a > b) a = b;
    if (a > c) a = c;
    return a;
}

static int RGB_to_HSV(double r, double g, double b,
		      double *h, double *s, double *v)
{
    double y, x, f;
    int i;
    x = min3(r, g, b);
    y = max3(r, g, b);
    if (y != x) {
	f = (r == x) ? g - b : ((g == x) ? b - r : r - g);
	i = (r == x) ? 3 : ((g == x) ? 5 : 1);
	*h = 60 * (i - f /(y - x));
	*s = (y - x)/y;
	*v = y;
    }
    else {
#ifdef MONO
	*h = NA_REAL; *s = 0; *v = y;
#else
	*h = 0; *s = 0; *v = y;
#endif
    }
    return 1;
}

#define RETURN_RGB(red,green,blue) *r=red;*g=green;*b=blue;break;

static int HSV_to_RGB(double h, double s, double v,
                double *r, double *g, double *b)
{
    double m, n, f;
    int i;
    if (h == NA_REAL) {
	*r = v; *g = v; *b = v;
    }
    else {
	h = h /60;		/* convert to [0, 6] */
	i = floor(h);
	f = h - i;
	if(!(i & 1))	/* if i is even */
	    f = 1 - f;
	m = v * (1 - s);
	n = v * (1 - s * f);
	switch (i) {
	case 6:
	case 0: RETURN_RGB(v, n, m);
	case 1: RETURN_RGB(n, v, m);
	case 2: RETURN_RGB(m, v, n);
	case 3: RETURN_RGB(m, n, v);
	case 4: RETURN_RGB(n, m, v);
	case 5: RETURN_RGB(v, m, n);
	}
    }
    return 1;
}

/* 
 * rgb all in [0,1] 
 * h in [0, 360], ls in [0,1]
 *
 * From:
 * http://wiki.beyondunreal.com/wiki/RGB_To_HLS_Conversion
 */
static int RGB_to_HLS(double r, double g, double b,
		      double *h, double *l, double *s)
{
    double min, max;

    min = min3(r, g, b);
    max = max3(r, g, b);

    *l = (max + min)/2;

    if (max != min) {
        if (*l <  0.5)  
            *s = (max - min)/(max + min);
        if (*l >= 0.5)  
            *s = (max - min)/(2.0 - max - min);

        if (r == max) 
            *h = (g - b)/(max - min);
        if (g == max) 
            *h = 2.0 + (b - r)/(max - min);
        if (b == max) 
            *h = 4.0 + (r - g)/(max - min);

        *h = *h * 60;
        if (*h < 0) 
            *h = *h + 360;
        if (*h > 360) 
            *h = *h - 360;
    } else {
        *s = 0;
#ifdef MONO
	*h = NA_REAL; 
#else
	*h = 0;
#endif
    }
}

static double qtrans(double q1, double q2, double hue) {
    double result = NA_REAL;

    if (hue > 360) 
        hue = hue - 360;
    if (hue < 0) 
        hue = hue + 360;

    if (hue < 60) 
        result = q1 + (q2 - q1) * hue / 60;
    else if (hue < 180)
        result = q2;
    else if (hue < 240) 
        result = q1 + (q2 - q1) * (240 - hue) / 60;
    else 
        result = q1;
    return result;
}

static int HLS_to_RGB(double h, double l, double s,
                      double *r, double *g, double *b)
{  
    double p1 = NA_REAL;
    double p2 = NA_REAL;
    
    if (l <= 0.5) 
        p2 = l * (1 + s); 
    else 
        p2 = l + s - (l * s);
    p1 = 2 * l - p2;

    if (s == 0) {
        *r = 1;
        *g = 1;
        *b = l;
    } else {
        *r = qtrans(p1, p2, h + 120);
        *g = qtrans(p1, p2, h);
        *b = qtrans(p1, p2, h - 120);
    }
}

/* ----- CIE-XYZ <-> CIE-LUV ----- */

static void XYZ_to_uv(double X, double Y, double Z, double *u, double *v)
{
    double t, x, y;
    t = X + Y + Z;
    x = X / t;
    y = Y / t;
    *u = 2 * x / (6 * y - x + 1.5);
    *v = 4.5 * y / (6 * y - x + 1.5);
}

static int XYZ_to_LUV(double X, double Y, double Z,
		      double XN, double YN, double ZN,
		      double *L, double *U, double *V)
{
    double u, v, uN, vN, y;
    XYZ_to_uv(X, Y, Z, &u, &v);
    XYZ_to_uv(XN, YN, ZN, &uN, &vN);
    y = Y / YN;
    *L = (y > 0.008856) ? 116 * pow(y, 1.0/3.0) - 16 : 903.3 * y;
    *U = 13 * *L * (u - uN);
    *V = 13 * *L * (v - vN);
}

static int LUV_to_XYZ(double L, double U, double V,
		      double XN, double YN, double ZN,
		      double *X, double *Y, double *Z)
{
    double u, v, uN, vN;
    if (L <= 0 && U == 0 && V == 0) {
	*X = 0; *Y = 0; *Z = 0;
    }
    else {
	*Y = YN * ((L > 7.999592) ? pow((L + 16)/116, 3) : L / 903.3);
	XYZ_to_uv(XN, YN, ZN, &uN, &vN);
	u = U / (13 * L) + uN;
	v = V / (13 * L) + vN;
	*X =  9.0 * *Y * u / (4 * v);
	*Z =  - *X / 3 - 5 * *Y + 3 * *Y / v;
    }
    return 1;
}


/* ----- LUV <-> polarLUV ----- */

static int LUV_to_polarLUV(double L, double U, double V,
			   double *l, double *c, double *h)
{
    *l = L;
    *c = sqrt(U * U + V * V);
    *h = RAD2DEG(atan2(V, U));
    while (*h > 360) *h -= 360;
    while (*h < 0) *h += 360;
    return 1;
}

static int polarLUV_to_LUV(double l, double c, double h,
		      double *L, double *U, double *V)
{
    h = DEG2RAD(h);
    *L = l;
    *U = c * cos(h);
    *V = c * sin(h);
    return 1;
}

/* ----- Argument Checking ----- */

#define CIEXYZ    0
#define RGB       1
#define HSV       2
#define CIELUV    3
#define POLARLUV  4
#define CIELAB    5
#define POLARLAB  6
#define HLS       7

static void CheckSpace(SEXP space, int *spacecode)
{
    if (!isString(space) || length(space) != 1)
	error("invalid color space in C routine \"CheckSpace\" (1)");
    if (!strcmp(CHAR(STRING_ELT(space, 0)), "XYZ"))
	*spacecode = CIEXYZ;
    else if (!strcmp(CHAR(STRING_ELT(space, 0)), "RGB"))
	*spacecode = RGB;
    else if (!strcmp(CHAR(STRING_ELT(space, 0)), "HSV"))
	*spacecode = HSV;
    else if (!strcmp(CHAR(STRING_ELT(space, 0)), "HLS"))
	*spacecode = HLS;
    else if (!strcmp(CHAR(STRING_ELT(space, 0)), "LUV"))
	*spacecode = CIELUV;
    else if (!strcmp(CHAR(STRING_ELT(space, 0)), "polarLUV"))
	*spacecode = POLARLUV;
    else if (!strcmp(CHAR(STRING_ELT(space, 0)), "LAB"))
	*spacecode = CIELAB;
    else if (!strcmp(CHAR(STRING_ELT(space, 0)), "polarLAB"))
	*spacecode = POLARLAB;
    else
	error("invalid color space in C routine \"CheckSpace\" (2)");
}

static void CheckColor(SEXP color, int *n)
{
    if (!isNumeric(color))
        error("color error - numeric values required");
    if (!isMatrix(color) || ncols(color) != 3)
        error("color error - a matrix with 3 columns required");
    *n = nrows(color);
}

static void CheckHex(SEXP hex, int *n)
{
    int i, j;
    if (!isString(hex))
        goto badhex;
    *n = length(hex);
    for (i = 0; i < *n; i++) {
        if (strlen(CHAR(STRING_ELT(hex, i))) != 7 ||
                CHAR(STRING_ELT(hex, i))[0] != '#')
            goto badhex;
        for (j = 1; j < 7; j++) {
            if (!isxdigit(CHAR(STRING_ELT(hex, i))[j]))
                goto badhex;
        }
    }
    return;
badhex:
    error("color error - hex values required");
}

static void CheckWhite(SEXP white, double *Xn, double *Yn, double *Zn)
{
    int n;
    if (isNull(white)) {
	/* Use D65 by default. */
	*Xn =  95.047;
	*Yn = 100.000;
	*Zn = 108.883;
    }
    else {
	CheckColor(white, &n);
	if (n != 1 ||
	    REAL(white)[0] <= 0 ||
	    REAL(white)[1] <= 0 ||
	    REAL(white)[2] <= 0)
	    error("color error || invalid white point");
	*Xn = REAL(white)[0];
	*Yn = REAL(white)[1];
	*Zn = REAL(white)[2];
    }
}

static void CheckGamma(SEXP gamma, double *gammaval)
{
    *gammaval = asReal(gamma);
    if (!R_FINITE(*gammaval) || *gammaval <= 0)
	error("invalid gamma value");
}

static void CheckFixup(SEXP fixup, int *fixupval)
{
    *fixupval = asLogical(fixup);
    if (*fixupval == NA_LOGICAL)
	*fixupval = 1;
}


/* ----- Entry Points for Coersion Methods ----- */

SEXP as_XYZ(SEXP color, SEXP space, SEXP white)
{
    double Xn, Yn, Zn;
    int spacecode;
    int i, n;
    SEXP ans;

    CheckColor(color, &n);
    CheckSpace(space, &spacecode);
    CheckWhite(white, &Xn, &Yn, &Zn);

    ans = allocMatrix(REALSXP, n, 3);

    switch(spacecode) {
    case CIEXYZ:
	for(i = 0; i < n; i++) {
	    REAL(ans)[i] = REAL(color)[i];
	    REAL(ans)[i+n] = REAL(color)[i+n];
	    REAL(ans)[i+2*n] = REAL(color)[i+2*n];
	}
	break;
    case RGB:
	for(i = 0; i < n; i++) {
	    RGB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HSV:
	for(i = 0; i < n; i++) {
	    HSV_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HLS:
	for(i = 0; i < n; i++) {
	    HLS_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELUV:
	for(i = 0; i < n; i++) {
	    LUV_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLUV:
	for(i = 0; i < n; i++) {
	    polarLUV_to_LUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELAB:
	for(i = 0; i < n; i++) {
	    LAB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLAB:
	for(i = 0; i < n; i++) {
	    polarLAB_to_LAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    default:
	error("unimplemented colour space (3)");
    }
    return ans;
}

SEXP as_RGB(SEXP color, SEXP space, SEXP white)
{
    double Xn, Yn, Zn;
    int spacecode;
    int i, n;
    SEXP ans;

    CheckColor(color, &n);
    CheckSpace(space, &spacecode);
    CheckWhite(white, &Xn, &Yn, &Zn);

    ans = allocMatrix(REALSXP, n, 3);

    switch(spacecode) {
    case CIEXYZ:
	for(i = 0; i < n; i++) {
	    XYZ_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case RGB:
	for(i = 0; i < n; i++) {
	    REAL(ans)[i] = REAL(color)[i];
	    REAL(ans)[i+n] = REAL(color)[i+n];
	    REAL(ans)[i+2*n] = REAL(color)[i+2*n];
	}
	break;
    case HSV:
	for(i = 0; i < n; i++) {
	    HSV_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HLS:
	for(i = 0; i < n; i++) {
	    HLS_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELUV:
	for(i = 0; i < n; i++) {
	    LUV_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLUV:
	for(i = 0; i < n; i++) {
	    polarLUV_to_LUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELAB:
	for(i = 0; i < n; i++) {
	    LAB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLAB:
	for(i = 0; i < n; i++) {
	    polarLAB_to_LAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    default:
	error("unimplemented colour space (3)");
    }
    return ans;
}

SEXP as_HSV(SEXP color, SEXP space, SEXP white)
{
    double Xn, Yn, Zn;
    int spacecode;
    int i, n;
    SEXP ans;

    CheckColor(color, &n);
    CheckSpace(space, &spacecode);
    CheckWhite(white, &Xn, &Yn, &Zn);

    ans = allocMatrix(REALSXP, n, 3);

    switch(spacecode) {
    case CIEXYZ:
	for(i = 0; i < n; i++) {
	    XYZ_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HSV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case RGB:
	for(i = 0; i < n; i++) {
	    RGB_to_HSV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HSV:
	for(i = 0; i < n; i++) {
	    REAL(ans)[i] = REAL(color)[i];
	    REAL(ans)[i+n] = REAL(color)[i+n];
	    REAL(ans)[i+2*n] = REAL(color)[i+2*n];
	}
	break;
    case HLS:
	for(i = 0; i < n; i++) {
	    HLS_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HSV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELUV:
	for(i = 0; i < n; i++) {
	    LUV_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HSV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLUV:
	for(i = 0; i < n; i++) {
	    polarLUV_to_LUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HSV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELAB:
	for(i = 0; i < n; i++) {
	    LAB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HSV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLAB:
	for(i = 0; i < n; i++) {
	    polarLAB_to_LAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HSV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    default:
	error("unimplemented colour space (3)");
    }
    return ans;
}

SEXP as_HLS(SEXP color, SEXP space, SEXP white)
{
    double Xn, Yn, Zn;
    int spacecode;
    int i, n;
    SEXP ans;

    CheckColor(color, &n);
    CheckSpace(space, &spacecode);
    CheckWhite(white, &Xn, &Yn, &Zn);

    ans = allocMatrix(REALSXP, n, 3);

    switch(spacecode) {
    case CIEXYZ:
	for(i = 0; i < n; i++) {
	    XYZ_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HLS(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case RGB:
	for(i = 0; i < n; i++) {
	    RGB_to_HLS(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HSV:
	for(i = 0; i < n; i++) {
	    HSV_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HLS(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
        break;
    case HLS:
	for(i = 0; i < n; i++) {
	    REAL(ans)[i] = REAL(color)[i];
	    REAL(ans)[i+n] = REAL(color)[i+n];
	    REAL(ans)[i+2*n] = REAL(color)[i+2*n];
	}
	break;
    case CIELUV:
	for(i = 0; i < n; i++) {
	    LUV_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HLS(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLUV:
	for(i = 0; i < n; i++) {
	    polarLUV_to_LUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HLS(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELAB:
	for(i = 0; i < n; i++) {
	    LAB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HLS(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLAB:
	for(i = 0; i < n; i++) {
	    polarLAB_to_LAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_RGB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_HLS(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    default:
	error("unimplemented colour space (3)");
    }
    return ans;
}


SEXP as_LUV(SEXP color, SEXP space, SEXP white)
{
    double Xn, Yn, Zn;
    int spacecode;
    int i, n;
    SEXP ans;

    CheckColor(color, &n);
    CheckSpace(space, &spacecode);
    CheckWhite(white, &Xn, &Yn, &Zn);

    ans = allocMatrix(REALSXP, n, 3);

    switch(spacecode) {
    case CIEXYZ:
	for(i = 0; i < n; i++) {
	    XYZ_to_LUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case RGB:
	for(i = 0; i < n; i++) {
	    RGB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HSV:
	for(i = 0; i < n; i++) {
	    HSV_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HLS:
	for(i = 0; i < n; i++) {
	    HLS_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELUV:
	for(i = 0; i < n; i++) {
	    REAL(ans)[i] = REAL(color)[i];
	    REAL(ans)[i+n] = REAL(color)[i+n];
	    REAL(ans)[i+2*n] = REAL(color)[i+2*n];
	}
	break;
    case POLARLUV:
	for(i = 0; i < n; i++) {
	    polarLUV_to_LUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]); 
	}
	break;
    case CIELAB:
	for(i = 0; i < n; i++) {
	    LAB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLAB:
	for(i = 0; i < n; i++) {
	    polarLAB_to_LAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    default:
	error("unimplemented colour space (3)");
    }
    return ans;
}

SEXP as_polarLUV(SEXP color, SEXP space, SEXP white)
{
    double Xn, Yn, Zn;
    int spacecode;
    int i, n;
    SEXP ans;

    CheckColor(color, &n);
    CheckSpace(space, &spacecode);
    CheckWhite(white, &Xn, &Yn, &Zn);

    ans = allocMatrix(REALSXP, n, 3);

    switch(spacecode) {
    case CIEXYZ:
	for(i = 0; i < n; i++) {
	    XYZ_to_LUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_polarLUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case RGB:
	for(i = 0; i < n; i++) {
	    RGB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_polarLUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HSV:
	for(i = 0; i < n; i++) {
	    HSV_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_polarLUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HLS:
	for(i = 0; i < n; i++) {
	    HLS_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_polarLUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELUV:
	for(i = 0; i < n; i++) {
	    LUV_to_polarLUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLUV:
	for(i = 0; i < n; i++) {
	    REAL(ans)[i] = REAL(color)[i];
	    REAL(ans)[i+n] = REAL(color)[i+n];
	    REAL(ans)[i+2*n] = REAL(color)[i+2*n];
	}
	break;
    case CIELAB:
	for(i = 0; i < n; i++) {
	    LAB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_polarLUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLAB:
	for(i = 0; i < n; i++) {
	    polarLAB_to_LAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_polarLUV(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    default:
	error("unimplemented colour space (3)");
    }
    return ans;
}

SEXP as_LAB(SEXP color, SEXP space, SEXP white)
{
    double Xn, Yn, Zn;
    int spacecode;
    int i, n;
    SEXP ans;

    CheckColor(color, &n);
    CheckSpace(space, &spacecode);
    CheckWhite(white, &Xn, &Yn, &Zn);

    ans = allocMatrix(REALSXP, n, 3);

    switch(spacecode) {
    case CIEXYZ:
	for(i = 0; i < n; i++) {
	    XYZ_to_LAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case RGB:
	for(i = 0; i < n; i++) {
	    RGB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HSV:
	for(i = 0; i < n; i++) {
	    HSV_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HLS:
	for(i = 0; i < n; i++) {
	    HLS_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELUV:
	for(i = 0; i < n; i++) {
	    LUV_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLUV:
	for(i = 0; i < n; i++) {
	    polarLUV_to_LUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELAB:
	for(i = 0; i < n; i++) {
	    REAL(ans)[i] = REAL(color)[i];
	    REAL(ans)[i+n] = REAL(color)[i+n];
	    REAL(ans)[i+2*n] = REAL(color)[i+2*n];
	}
	break;
    case POLARLAB:
	for(i = 0; i < n; i++) {
	    LAB_to_polarLAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    default:
	error("unimplemented colour space");
    }
    return ans;
}

SEXP as_polarLAB(SEXP color, SEXP space, SEXP white)
{
    double Xn, Yn, Zn;
    int spacecode;
    int i, n;
    SEXP ans;

    CheckColor(color, &n);
    CheckSpace(space, &spacecode);
    CheckWhite(white, &Xn, &Yn, &Zn);

    ans = allocMatrix(REALSXP, n, 3);

    switch(spacecode) {
    case CIEXYZ:
	for(i = 0; i < n; i++) {
	    XYZ_to_LAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_polarLAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case RGB:
	for(i = 0; i < n; i++) {
	    RGB_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_polarLAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HSV:
	for(i = 0; i < n; i++) {
	    HSV_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_polarLAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case HLS:
	for(i = 0; i < n; i++) {
	    HLS_to_RGB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    RGB_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_polarLAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELUV:
	for(i = 0; i < n; i++) {
	    LUV_to_XYZ(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_polarLAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLUV:
	for(i = 0; i < n; i++) {
	    polarLUV_to_LUV(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LUV_to_XYZ(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    XYZ_to_LAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n], Xn, Yn, Zn,
		       &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	    LAB_to_polarLAB(REAL(ans)[i], REAL(ans)[i+n], REAL(ans)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case CIELAB:
	for(i = 0; i < n; i++) {
	    LAB_to_polarLAB(REAL(color)[i], REAL(color)[i+n], REAL(color)[i+2*n],
			    &REAL(ans)[i], &REAL(ans)[i+n], &REAL(ans)[i+2*n]);
	}
	break;
    case POLARLAB:
	for(i = 0; i < n; i++) {
	    REAL(ans)[i] = REAL(color)[i];
	    REAL(ans)[i+n] = REAL(color)[i+n];
	    REAL(ans)[i+2*n] = REAL(color)[i+2*n];
	}
	break;
    default:
	error("unimplemented colour space (3)");
    }
    return ans;
}

static int FixupColor(int *r, int *g, int *b)
{
    int fix = 0;
    if (*r < 0) { *r = 0; fix = 1; } else if (*r > 255) { *r = 255; fix = 1; }
    if (*g < 0) { *g = 0; fix = 1; } else if (*g > 255) { *g = 255; fix = 1; }
    if (*b < 0) { *b = 0; fix = 1; } else if (*b > 255) { *b = 255; fix = 1; }
    return fix;
}

static const char HEXDIG[] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
};

SEXP RGB_to_RColor(SEXP rgb, SEXP gamma, SEXP fixup)
{
    double gammavalue, r, g, b, Xn, Yn, Zn;
    int fixupvalue, i, ir, ig, ib, n, nagen;
    char hex[8];
    SEXP ans;

    CheckColor(rgb, &n);
    CheckGamma(gamma, &gammavalue);
    CheckFixup(fixup, &fixupvalue);

    PROTECT(ans = allocVector(STRSXP, n));
    nagen = 0;

    for (i = 0; i < n; i++) {
	r = REAL(rgb)[i];
	g = REAL(rgb)[i+n];
	b = REAL(rgb)[i+2*n];
	RGB_to_DEVRGB(r, g, b, gammavalue, &r, &g, &b);
        if (R_FINITE(r) && R_FINITE(g) && R_FINITE(b)) {
	    /* Hardware color representation */
	    ir = 255 * r + .5;
	    ig = 255 * g + .5;
	    ib = 255 * b + .5;
	    if (FixupColor(&ir, &ig, &ib) && !fixupvalue) {
		SET_STRING_ELT(ans, i, NA_STRING);
	    }
	    else {
		hex[0] = '#';
		hex[1] = HEXDIG[(ir / 16) % 16];
		hex[2] = HEXDIG[ir % 16];
		hex[3] = HEXDIG[(ig / 16) % 16];
		hex[4] = HEXDIG[ig % 16];
		hex[5] = HEXDIG[(ib / 16) % 16];
		hex[6] = HEXDIG[ib % 16];
		hex[7] = '\0';
		SET_STRING_ELT(ans, i, mkChar(hex));
	    }
	}
	else
            SET_STRING_ELT(ans, i, NA_STRING);
    }
    UNPROTECT(1);
    return ans;
}

static int decodeHexDigit(int x)
{
  switch(x) {
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
	return x - '0';
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	return x - 'A' + 10;
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	return x - 'a' + 10;
  default:
	return -1;
  }
}

static void decodeHexStr(char *x, double *r, double *g, double *b)
{
  int d1, d2, d3, d4, d5, d6;
  d1 = decodeHexDigit(x[1]);
  d2 = decodeHexDigit(x[2]);
  d3 = decodeHexDigit(x[3]);
  d4 = decodeHexDigit(x[4]);
  d5 = decodeHexDigit(x[5]);
  d6 = decodeHexDigit(x[6]);
  if (d1 >= 0 && d1 >= 0 &&
      d3 >= 0 && d4 >= 0 &&
      d5 >= 0 && d6 >= 0) {
      *r = (16 *d1 + d2)/255.;
      *g = (16 *d3 + d4)/255.;
      *b = (16 *d5 + d6)/255.;
  }
  else {
    *r = NA_REAL;
    *g = NA_REAL;
    *b = NA_REAL;
  }
}

SEXP hex_to_RGB(SEXP hex, SEXP gamma)
{
    double gammavalue, r, g, b;
    int i, n;
    SEXP ans;
    CheckHex(hex, &n);
    gammavalue = asReal(gamma);
    ans = allocMatrix(REALSXP, n, 3);
    for(i = 0; i < n; i++) {
        decodeHexStr(CHAR(STRING_ELT(hex,i)), &r, &g, &b);
        if (R_FINITE(gammavalue) && gammavalue > 0)
            DEVRGB_to_RGB(r, g, b, gammavalue, &r, &g, &b);
        REAL(ans)[i] = r;
        REAL(ans)[i+n] = g;
        REAL(ans)[i+2*n] = b;
    }
    return ans;
}
