#ifndef READNSX_COMMON_H
#define READNSX_COMMON_H

#include <cstring>
#include <cpp11.hpp>

namespace readnsx {

// -----------------------------------------------------------------------------
// makeNamedList: Create named lists using cpp11 without _nm initializer lists
// This avoids rchk PROTECT warnings in cpp11's list.hpp
// See: https://github.com/r-lib/cpp11/issues/445
// -----------------------------------------------------------------------------

inline cpp11::writable::list makeNamedList(
    const char* n1, SEXP v1,
    const char* n2, SEXP v2
) {
    cpp11::writable::list result(2);
    result[0] = v1;
    result[1] = v2;
    cpp11::writable::strings names(2);
    names[0] = n1;
    names[1] = n2;
    result.names() = names;
    return result;
}

inline cpp11::writable::list makeNamedList(
    const char* n1, SEXP v1,
    const char* n2, SEXP v2,
    const char* n3, SEXP v3,
    const char* n4, SEXP v4
) {
    cpp11::writable::list result(4);
    result[0] = v1;
    result[1] = v2;
    result[2] = v3;
    result[3] = v4;
    cpp11::writable::strings names(4);
    names[0] = n1;
    names[1] = n2;
    names[2] = n3;
    names[3] = n4;
    result.names() = names;
    return result;
}

inline cpp11::writable::list makeNamedList(
    const char* n1, SEXP v1,
    const char* n2, SEXP v2,
    const char* n3, SEXP v3,
    const char* n4, SEXP v4,
    const char* n5, SEXP v5,
    const char* n6, SEXP v6,
    const char* n7, SEXP v7,
    const char* n8, SEXP v8
) {
    cpp11::writable::list result(8);
    result[0] = v1;
    result[1] = v2;
    result[2] = v3;
    result[3] = v4;
    result[4] = v5;
    result[5] = v6;
    result[6] = v7;
    result[7] = v8;
    cpp11::writable::strings names(8);
    names[0] = n1;
    names[1] = n2;
    names[2] = n3;
    names[3] = n4;
    names[4] = n5;
    names[5] = n6;
    names[6] = n7;
    names[7] = n8;
    result.names() = names;
    return result;
}

template <typename A>
inline void convertBuffer(A* ptr, char* bufptr, const int& n) {
  int esize = sizeof( A ) / sizeof( char );
  for(int i = 0; i < n; i++, ptr++, bufptr += esize ) {
    std::memcpy(ptr, bufptr, esize);
  }
}

inline std::string ltrim(const std::string &s) {
  for( std::size_t i = 0; i < s.size(); i++ ) {
    if( s[i] != ' ' ) {
      return s.substr(i);
    }
  }
  return "";
}

inline std::string rtrim(const std::string &s) {
  for( std::size_t i = s.size() - 1; i > 0; i-- ) {
    if( s[i] != ' ' ) {
      return s.substr(0, i + 1);
    }
  }
  return "";
}

inline std::string trim(const std::string &s) {
  return ltrim( rtrim( s ) );
}

}

#endif // READNSX_COMMON_H
