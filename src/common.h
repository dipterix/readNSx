#ifndef READNSX_COMMON_H
#define READNSX_COMMON_H

#include <cstring>

namespace readnsx {

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
