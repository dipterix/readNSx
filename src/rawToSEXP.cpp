#include <cstring>
#include <cpp11.hpp>

[[cpp11::register]]
SEXP rawToUInt8(SEXP x) {
  if( TYPEOF(x) != RAWSXP ) {
    cpp11::stop("C++ `rawToUInt8`: Input must be raw");
  }
  R_xlen_t xlen = XLENGTH(x);
  uint8_t buf;
  uint8_t* bufptr = &buf;
  SEXP re = PROTECT(Rf_allocVector(INTSXP, xlen));
  int* ptr = INTEGER(re);
  Rbyte* xptr = RAW(x);

  for(R_xlen_t i = 0; i < xlen; i++, ptr++, xptr++) {
    std::memcpy(bufptr, xptr, 1);
    *ptr = buf;
  }

  UNPROTECT(1);
  return re;
}

[[cpp11::register]]
SEXP rawToInt8(SEXP x) {
  if( TYPEOF(x) != RAWSXP ) {
    cpp11::stop("C++ `rawToInt8`: Input must be raw");
  }
  R_xlen_t xlen = XLENGTH(x);
  int8_t buf;
  int8_t* bufptr = &buf;

  SEXP re = PROTECT(Rf_allocVector(INTSXP, xlen));
  int* ptr = INTEGER(re);
  Rbyte* xptr = RAW(x);

  for(R_xlen_t i = 0; i < xlen; i++, ptr++, xptr++) {
    std::memcpy(bufptr, xptr, 1);
    *ptr = buf;
  }

  UNPROTECT(1);
  return re;
}

[[cpp11::register]]
SEXP rawToUInt16(SEXP x) {

  const R_xlen_t esize = 2;
  R_xlen_t xlen = XLENGTH(x);
  if( TYPEOF(x) != RAWSXP || (xlen % esize) != 0 ) {
    cpp11::stop("C++ `rawToUInt16`: Input must be multiple of raw("+ std::to_string(esize) +")");
  }
  xlen /= esize;
  SEXP re = PROTECT(Rf_allocVector(INTSXP, xlen));
  int* ptr = INTEGER(re);
  Rbyte* xptr = RAW(x);

  uint16_t buf = 0;
  uint16_t* bufptr = &buf;
  R_xlen_t i;

  for(i = 0; i < xlen; i++, ptr++) {
    std::memcpy(bufptr, xptr, esize);
    *ptr = buf;
    xptr += esize;
  }

  UNPROTECT(1);
  return re;
}

[[cpp11::register]]
SEXP rawToInt16(SEXP x) {
  const R_xlen_t esize = 2;
  R_xlen_t xlen = XLENGTH(x);
  if( TYPEOF(x) != RAWSXP || (xlen % esize) != 0 ) {
    cpp11::stop("C++ `rawToInt16`: Input must be multiple of raw("+ std::to_string(esize) +")");
  }
  xlen /= esize;
  SEXP re = PROTECT(Rf_allocVector(INTSXP, xlen));
  int* ptr = INTEGER(re);
  Rbyte* xptr = RAW(x);

  int16_t buf = 0;
  int16_t* bufptr = &buf;
  R_xlen_t i;

  for(i = 0; i < xlen; i++, ptr++) {
    std::memcpy(bufptr, xptr, esize);
    *ptr = buf;
    xptr += esize;
  }


  UNPROTECT(1);
  return re;
}

[[cpp11::register]]
SEXP rawToUInt32(SEXP x) {
  const R_xlen_t esize = 4;
  R_xlen_t xlen = XLENGTH(x);
  if( TYPEOF(x) != RAWSXP || (xlen % esize) != 0 ) {
    cpp11::stop("C++ `rawToUInt32`: Input must be multiple of raw("+ std::to_string(esize) +")");
  }
  xlen /= esize;
  SEXP re = PROTECT(Rf_allocVector(REALSXP, xlen));
  double* ptr = REAL(re);
  Rbyte* xptr = RAW(x);

  uint32_t buf = 0;
  uint32_t* bufptr = &buf;
  R_xlen_t i;

  for(i = 0; i < xlen; i++, ptr++) {
    std::memcpy(bufptr, xptr, esize);
    *ptr = (double) buf;
    xptr += esize;
  }


  UNPROTECT(1);
  return re;
}

[[cpp11::register]]
SEXP rawToInt32(SEXP x) {
  const R_xlen_t esize = 4;
  R_xlen_t xlen = XLENGTH(x);
  if( TYPEOF(x) != RAWSXP || (xlen % esize) != 0 ) {
    cpp11::stop("C++ `rawToInt32`: Input must be multiple of raw("+ std::to_string(esize) +")");
  }
  xlen /= esize;
  SEXP re = PROTECT(Rf_allocVector(INTSXP, xlen));
  int* ptr = INTEGER(re);
  Rbyte* xptr = RAW(x);

  int32_t buf = 0;
  int32_t* bufptr = &buf;
  R_xlen_t i;

  for(i = 0; i < xlen; i++, ptr++) {
    std::memcpy(bufptr, xptr, esize);
    *ptr = (int) buf;
    xptr += esize;
  }


  UNPROTECT(1);
  return re;
}

[[cpp11::register]]
SEXP rawToInt64(SEXP x) {
  const R_xlen_t esize = 8;
  R_xlen_t xlen = XLENGTH(x);
  if( TYPEOF(x) != RAWSXP || (xlen % esize) != 0 ) {
    cpp11::stop("C++ `rawToInt64`: Input must be multiple of raw("+ std::to_string(esize) +")");
  }
  xlen /= esize;
  SEXP re = PROTECT(Rf_allocVector(REALSXP, xlen));
  double* ptr = REAL(re);
  Rbyte* xptr = RAW(x);

  R_xlen_t i;

  for(i = 0; i < xlen; i++, ptr++) {
    std::memcpy(ptr, xptr, esize);
    xptr += esize;
  }

  SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(cls, 0, Rf_mkChar("integer64"));
  Rf_setAttrib(re, R_ClassSymbol, cls);

  UNPROTECT(2);
  return re;
}

[[cpp11::register]]
SEXP rawToFloat(SEXP x) {

  const R_xlen_t esize = 4;
  R_xlen_t xlen = XLENGTH(x);
  if( TYPEOF(x) != RAWSXP || (xlen % esize) != 0 ) {
    cpp11::stop("C++ `rawToFloat`: Input must be multiple of raw("+ std::to_string(esize) +")");
  }
  xlen /= esize;
  SEXP re = PROTECT(Rf_allocVector(REALSXP, xlen));
  double* ptr = REAL(re);
  Rbyte* xptr = RAW(x);

  float buf = 0.0;
  float* bufptr = &buf;
  R_xlen_t i;

  for(i = 0; i < xlen; i++, ptr++) {
    std::memcpy(bufptr, xptr, esize);
    *ptr = *bufptr;
    xptr += esize;
  }

  UNPROTECT(1);
  return re;
}

[[cpp11::register]]
SEXP rawToString(SEXP x) {

  if( TYPEOF(x) != RAWSXP ) {
    cpp11::stop("C++ `rawToString`: Input must be raw");
  }
  std::string re;
  Rbyte* xptr = RAW(x);
  re.assign(xptr, xptr + XLENGTH(x));

  return cpp11::as_sexp(re);
}


/***R
rawToString(as.raw(c(10:0, 66)))
*/
