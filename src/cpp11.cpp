// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// rawToSEXP.cpp
SEXP rawToUInt8(SEXP x);
extern "C" SEXP _readNSx_rawToUInt8(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(rawToUInt8(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// rawToSEXP.cpp
SEXP rawToInt8(SEXP x);
extern "C" SEXP _readNSx_rawToInt8(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(rawToInt8(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// rawToSEXP.cpp
SEXP rawToUInt16(SEXP x);
extern "C" SEXP _readNSx_rawToUInt16(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(rawToUInt16(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// rawToSEXP.cpp
SEXP rawToInt16(SEXP x);
extern "C" SEXP _readNSx_rawToInt16(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(rawToInt16(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// rawToSEXP.cpp
SEXP rawToUInt32(SEXP x);
extern "C" SEXP _readNSx_rawToUInt32(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(rawToUInt32(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// rawToSEXP.cpp
SEXP rawToInt32(SEXP x);
extern "C" SEXP _readNSx_rawToInt32(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(rawToInt32(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// rawToSEXP.cpp
SEXP rawToInt64(SEXP x);
extern "C" SEXP _readNSx_rawToInt64(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(rawToInt64(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// rawToSEXP.cpp
SEXP rawToFloat(SEXP x);
extern "C" SEXP _readNSx_rawToFloat(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(rawToFloat(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// rawToSEXP.cpp
SEXP rawToString(SEXP x);
extern "C" SEXP _readNSx_rawToString(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(rawToString(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// readNSxDataPacket.cpp
SEXP readNSxDataPacket30(const std::string& filePath, const uint32_t& nBytes, const double& sampleRate, const int& nChannels, const uint32_t& skipBytes, const double& slope, const double& intercept);
extern "C" SEXP _readNSx_readNSxDataPacket30(SEXP filePath, SEXP nBytes, SEXP sampleRate, SEXP nChannels, SEXP skipBytes, SEXP slope, SEXP intercept) {
  BEGIN_CPP11
    return cpp11::as_sexp(readNSxDataPacket30(cpp11::as_cpp<cpp11::decay_t<const std::string&>>(filePath), cpp11::as_cpp<cpp11::decay_t<const uint32_t&>>(nBytes), cpp11::as_cpp<cpp11::decay_t<const double&>>(sampleRate), cpp11::as_cpp<cpp11::decay_t<const int&>>(nChannels), cpp11::as_cpp<cpp11::decay_t<const uint32_t&>>(skipBytes), cpp11::as_cpp<cpp11::decay_t<const double&>>(slope), cpp11::as_cpp<cpp11::decay_t<const double&>>(intercept)));
  END_CPP11
}
// readNSxDataPacket.cpp
SEXP readNSxDataPacket2x(const std::string& filePath, const uint32_t& nBytes, const double& sampleRate, const int& nChannels, const uint32_t& skipBytes, const double& slope, const double& intercept);
extern "C" SEXP _readNSx_readNSxDataPacket2x(SEXP filePath, SEXP nBytes, SEXP sampleRate, SEXP nChannels, SEXP skipBytes, SEXP slope, SEXP intercept) {
  BEGIN_CPP11
    return cpp11::as_sexp(readNSxDataPacket2x(cpp11::as_cpp<cpp11::decay_t<const std::string&>>(filePath), cpp11::as_cpp<cpp11::decay_t<const uint32_t&>>(nBytes), cpp11::as_cpp<cpp11::decay_t<const double&>>(sampleRate), cpp11::as_cpp<cpp11::decay_t<const int&>>(nChannels), cpp11::as_cpp<cpp11::decay_t<const uint32_t&>>(skipBytes), cpp11::as_cpp<cpp11::decay_t<const double&>>(slope), cpp11::as_cpp<cpp11::decay_t<const double&>>(intercept)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_readNSx_rawToFloat",          (DL_FUNC) &_readNSx_rawToFloat,          1},
    {"_readNSx_rawToInt16",          (DL_FUNC) &_readNSx_rawToInt16,          1},
    {"_readNSx_rawToInt32",          (DL_FUNC) &_readNSx_rawToInt32,          1},
    {"_readNSx_rawToInt64",          (DL_FUNC) &_readNSx_rawToInt64,          1},
    {"_readNSx_rawToInt8",           (DL_FUNC) &_readNSx_rawToInt8,           1},
    {"_readNSx_rawToString",         (DL_FUNC) &_readNSx_rawToString,         1},
    {"_readNSx_rawToUInt16",         (DL_FUNC) &_readNSx_rawToUInt16,         1},
    {"_readNSx_rawToUInt32",         (DL_FUNC) &_readNSx_rawToUInt32,         1},
    {"_readNSx_rawToUInt8",          (DL_FUNC) &_readNSx_rawToUInt8,          1},
    {"_readNSx_readNSxDataPacket2x", (DL_FUNC) &_readNSx_readNSxDataPacket2x, 7},
    {"_readNSx_readNSxDataPacket30", (DL_FUNC) &_readNSx_readNSxDataPacket30, 7},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_readNSx(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
